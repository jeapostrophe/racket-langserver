#lang racket/base

;; Single-owner scheduling of analysis and Resyntax. Attempts, their queue,
;; and worker handles stay private; callers hold only completion events.
(require racket/async-channel
         racket/contract
         "../../common/source.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt"
         "completion-event.rkt"
         "supervised-worker.rkt")

(provide Analysis-Scheduler?
         make-analysis-scheduler
         scheduled-analysis-event
         request-analysis!
         schedule-resyntax!
         cancel-source-analysis!
         stop-analysis-scheduler!
         pump-analysis-scheduler!
         Analysis-Completed?
         finish-analysis-task!)

;; parent-completion identifies the accepted analysis refined by Resyntax.
;; worker is #f until execution starts. Terminal attempts leave the registry.
(struct/contract Analysis-Attempt
  ([snapshot Source-Snapshot?]
   [kind (or/c 'analysis 'resyntax)]
   [parent-completion (or/c #f evt?)]
   [completion Completion-Event?]
   [(state #:mutable) (or/c 'queued 'running 'terminal)]
   [(worker #:mutable) (or/c #f Supervised-Worker?)]))

(struct/contract Analysis-Scheduler
  ([events async-channel?]
   [analyze (-> Source-Snapshot? Analysis-Product?)]
   [resyntax (or/c #f (-> string? string? list?))]
   [timeout (and/c real? (>=/c 0))]
   [slots exact-positive-integer?]
   [attempts (hash/c (cons/c Source-Id? (or/c 'analysis 'resyntax))
                     Analysis-Attempt? #:immutable #f)]
   [(queue #:mutable) (listof Analysis-Attempt?)]
   [(stopped? #:mutable) boolean?]))

;; The event envelope carries an identity privately; replay cannot revive an
;; attempt removed by completion, cancellation, or a same-revision retry.
(struct Analysis-Completed (attempt result))

(define (make-analysis-scheduler events #:analyze analyze #:resyntax resyntax
                                 #:timeout [timeout 90] #:slots [slots 4])
  (Analysis-Scheduler events analyze resyntax timeout slots (make-hash) '() #f))

(define (attempt-key attempt)
  (cons (Source-Snapshot-id (Analysis-Attempt-snapshot attempt))
        (Analysis-Attempt-kind attempt)))

(define (registered? scheduler attempt)
  (eq? attempt (hash-ref (Analysis-Scheduler-attempts scheduler) (attempt-key attempt) #f)))

(define (scheduled-analysis-event scheduler source)
  (define attempt
    (hash-ref (Analysis-Scheduler-attempts scheduler) (cons (source-id source) 'analysis) #f))
  (and attempt (Analysis-Attempt-completion attempt)))

(define (enqueue! scheduler snapshot kind parent-completion)
  (define attempt
    (Analysis-Attempt snapshot kind parent-completion (make-completion-event) 'queued #f))
  (hash-set! (Analysis-Scheduler-attempts scheduler) (attempt-key attempt) attempt)
  (set-Analysis-Scheduler-queue! scheduler
                                 (append (Analysis-Scheduler-queue scheduler) (list attempt)))
  (Analysis-Attempt-completion attempt))

;; Requests for the same revision share an attempt. A successor revision
;; supersedes outstanding work for this source, including its Resyntax.
(define (request-analysis! scheduler snapshot)
  (define source (Source-Snapshot-source snapshot))
  (define active
    (hash-ref (Analysis-Scheduler-attempts scheduler) (cons (source-id source) 'analysis) #f))
  (cond
    [(Analysis-Scheduler-stopped? scheduler) (ready-evt (Analysis-Outcome 'stopped 'shutdown))]
    [(and active
          (eq? (Source-Snapshot-revision snapshot)
               (Source-Snapshot-revision (Analysis-Attempt-snapshot active))))
     (Analysis-Attempt-completion active)]
    [else
     (cancel-source-analysis! scheduler source)
     (enqueue! scheduler snapshot 'analysis #f)]))

(define (schedule-resyntax! scheduler snapshot parent-completion)
  (when (and (not (Analysis-Scheduler-stopped? scheduler))
             (Analysis-Scheduler-resyntax scheduler))
    (define previous
      (hash-ref (Analysis-Scheduler-attempts scheduler) (cons (Source-Snapshot-id snapshot) 'resyntax) #f))
    (when previous (settle! scheduler previous (Analysis-Outcome 'stopped 'superseded)))
    (enqueue! scheduler snapshot 'resyntax parent-completion))
  (void))

(define (settle! scheduler attempt outcome)
  (unless (eq? 'terminal (Analysis-Attempt-state attempt))
    (when (Analysis-Attempt-worker attempt)
      (stop-supervised-worker! (Analysis-Attempt-worker attempt)))
    (set-Analysis-Attempt-state! attempt 'terminal)
    (when (registered? scheduler attempt)
      (hash-remove! (Analysis-Scheduler-attempts scheduler) (attempt-key attempt)))
    (resolve-completion! (Analysis-Attempt-completion attempt) outcome)))

(define (cancel-source-analysis! scheduler source [reason 'source-changed])
  (for ([kind (in-list '(analysis resyntax))])
    (define attempt
      (hash-ref (Analysis-Scheduler-attempts scheduler) (cons (source-id source) kind) #f))
    (when attempt (settle! scheduler attempt (Analysis-Outcome 'stopped reason))))
  (set-Analysis-Scheduler-queue!
    scheduler (filter (lambda (attempt) (registered? scheduler attempt))
                      (Analysis-Scheduler-queue scheduler))))

(define (stop-analysis-scheduler! scheduler)
  (set-Analysis-Scheduler-stopped?! scheduler #t)
  (for ([attempt (in-list (hash-values (Analysis-Scheduler-attempts scheduler)))])
    (settle! scheduler attempt (Analysis-Outcome 'stopped 'shutdown)))
  (set-Analysis-Scheduler-queue! scheduler '()))

;; current? checks source eligibility at dispatch, after any time spent queued.
;; It receives a snapshot and never receives the scheduler's private state.
(define (pump-analysis-scheduler! scheduler current?)
  (unless (Analysis-Scheduler-stopped? scheduler)
    (let loop ()
      (define queue (Analysis-Scheduler-queue scheduler))
      (define running
        (for/sum ([attempt (in-hash-values (Analysis-Scheduler-attempts scheduler))]
                  #:when (eq? 'running (Analysis-Attempt-state attempt)))
          1))
      (when (and (pair? queue) (< running (Analysis-Scheduler-slots scheduler)))
        (define attempt (car queue))
        (set-Analysis-Scheduler-queue! scheduler (cdr queue))
        (if (and (registered? scheduler attempt)
                 (current? (Analysis-Attempt-snapshot attempt)))
            (start-attempt! scheduler attempt)
            (settle! scheduler attempt (Analysis-Outcome 'stopped 'source-changed)))
        (loop)))))

(define (start-attempt! scheduler attempt)
  (define snapshot (Analysis-Attempt-snapshot attempt))
  (set-Analysis-Attempt-state! attempt 'running)
  (define analysis? (eq? 'analysis (Analysis-Attempt-kind attempt)))
  (define worker
    (start-supervised-worker!
      (Analysis-Scheduler-events scheduler) (Analysis-Scheduler-timeout scheduler)
      (if analysis?
          (lambda () ((Analysis-Scheduler-analyze scheduler) snapshot))
          (lambda () ((Analysis-Scheduler-resyntax scheduler)
                      (Source-Snapshot-text snapshot) (Source-Snapshot-uri snapshot))))
      #:fail (lambda (reason) (Analysis-Outcome 'failed reason))
      #:exited 'worker-exited
      #:timed-out 'analysis-timeout
      #:done (lambda (result) (Analysis-Completed attempt result))))
  (set-Analysis-Attempt-worker! attempt worker))

;; accept runs inline for the current running attempt only. It receives
;; snapshot, kind, completion, parent-completion, and worker result, and returns
;; an Analysis-Outcome after installing/discarding the result and translating
;; domain failures. Only then may the shared completion become ready.
(define (finish-analysis-task! scheduler message accept)
  (define attempt (Analysis-Completed-attempt message))
  (when (and (registered? scheduler attempt) (eq? 'running (Analysis-Attempt-state attempt)))
    (define outcome
      (accept (Analysis-Attempt-snapshot attempt) (Analysis-Attempt-kind attempt)
              (Analysis-Attempt-completion attempt) (Analysis-Attempt-parent-completion attempt)
              (Analysis-Completed-result message)))
    (settle! scheduler attempt outcome)))
