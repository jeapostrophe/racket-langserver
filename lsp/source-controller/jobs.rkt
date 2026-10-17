#lang racket/base

;; Shared analysis and Resyntax jobs: acquisition, the bounded FIFO runner,
;; terminal outcomes, and acceptance of worker products into live documents.
(require racket/match
         "../../common/debug.rkt"
         "../../common/json-util.rkt"
         "../../common/source.rkt"
         "../../common/settings.rkt"
         "../../common/version.rkt"
         "../../doclib/doc.rkt"
         "../../workspace/current.rkt"
         "../../workspace/state.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt"
         "../safedoc.rkt"
         "completion.rkt"
         "retained.rkt"
         "state.rkt"
         "worker.rkt")

(provide (struct-out Job-Done)
         install-analysis!
         settle-job!
         invalidate-jobs!
         pump-jobs!
         finish-job!
         source-controller-acquire-analysis!
         source-controller-query-analysis!
         source-controller-analyze!)

(struct Job-Done (job result) #:transparent)

(define (running-jobs controller)
  (for/sum ([job (in-hash-values (controller-jobs controller))])
    (if (eq? 'running (Job-state job)) 1 0)))

;; Only successful analysis stays cached, and on a closed file only while its
;; facts are retained. Waiters of a forgotten attempt still observe its
;; outcome, but a later acquisition can retry the same revision.
(define (settle-job! controller job outcome)
  (unless (eq? 'terminal (Job-state job))
    (when (Job-custodian job)
      (custodian-shutdown-all (Job-custodian job)))
    (set-Job-state! job 'terminal)
    (complete! (Job-completion job) outcome)
    (define source (job-source job))
    (unless (and (eq? 'analysis (Job-kind job))
                 (eq? 'succeeded (Analysis-Outcome-state outcome))
                 (or (source-buffer controller source)
                     (retained-analysis controller source)))
      (forget-job! controller job))))

(define (invalidate-jobs! controller source)
  (for ([kind (in-list '(analysis resyntax))])
    (define job (hash-ref (controller-jobs controller) (cons (source-id source) kind) #f))
    (when job
      (settle-job! controller job (Analysis-Outcome 'stopped 'source-changed))
      (forget-job! controller job)))
  (define runner (Source-Controller-runner controller))
  (set-Runner-queue! runner
                     (filter (lambda (job) (current-job? controller job)) (Runner-queue runner))))

(define (enqueue-job! controller snapshot kind [parent #f])
  (define runner (Source-Controller-runner controller))
  (define job (Job snapshot kind parent (make-completion) 'queued #f))
  (hash-set! (Runner-jobs runner) (job-key job) job)
  (set-Runner-queue! runner (append (Runner-queue runner) (list job)))
  job)

;; This is one consumer operation: snapshots supplied here were acquired from
;; this controller, and are checked again before joining or creating an attempt.
(define (source-controller-acquire-analysis! controller snapshot)
  (define source (Source-Snapshot-source snapshot))
  (cond
    [(or (stopped? controller)
         (not (eq? (Source-Snapshot-revision snapshot)
                   (Source-View-revision (source-controller-view controller source)))))
     (ready-evt (Analysis-Outcome 'stopped 'ineligible))]
    [else
     (define job
       (hash-ref (controller-jobs controller) (cons (source-id source) 'analysis)
                 (lambda () (enqueue-job! controller snapshot 'analysis))))
     (define event (completion-evt (Job-completion job)))
     (define buffer (source-buffer controller source))
     (when buffer
       (with-write-safedoc buffer (lambda (sd) (set-SafeDoc-analysis! sd event))))
     (pump-jobs! controller)
     event]))

;; Query availability without treating restoration as fresh completion. Always
;; start/join the revision's job first; only pending work can use retained facts.
(define (source-controller-query-analysis! controller snapshot)
  (define event (source-controller-acquire-analysis! controller snapshot))
  (define buffer (source-buffer controller (Source-Snapshot-source snapshot)))
  (define available
    (if buffer
        (with-read-safedoc buffer SafeDoc-available)
        (matching-retained controller snapshot)))
  (if (or (sync/timeout 0 event) (not available))
      event
      (ready-evt (Analysis-Outcome 'succeeded
                                   (Analysis-Product-facts (Available-Analysis-product available))))))

;; Unlike acquisition, this reruns analysis even when the current revision
;; already has a successful result.
(define (source-controller-analyze! controller source)
  (define snapshot (source-controller-snapshot controller source))
  (when snapshot
    (define previous (hash-ref (controller-jobs controller) (cons (source-id source) 'analysis) #f))
    (when (and previous (eq? 'terminal (Job-state previous)))
      (invalidate-jobs! controller source))
    (source-controller-acquire-analysis! controller snapshot))
  (void))

(define (pump-jobs! controller)
  (define runner (Source-Controller-runner controller))
  (unless (stopped? controller)
    (let loop ()
      (match (Runner-queue runner)
        [(cons job rest)
         #:when (< (running-jobs controller) (Runner-slots runner))
         (set-Runner-queue! runner rest)
         (if (and (current-job? controller job) (job-current-source? controller job))
             (start-job! controller job)
             (settle-job! controller job (Analysis-Outcome 'stopped 'source-changed)))
         (loop)]
        [_ (void)]))))

(define (start-job! controller job)
  (define snapshot (Job-snapshot job))
  (define custodian (make-custodian))
  (set-Job-state! job 'running)
  (set-Job-custodian! job custodian)
  (define runner (Source-Controller-runner controller))
  (define-values (work launch)
    (case (Job-kind job)
      [(analysis)
       (values (lambda () ((Runner-analyze runner) snapshot))
               ;; Keep each expansion pool under its attempt custodian so
               ;; cancellation releases its processor resources with the worker.
               (if support/parallel-threads?
                   (lambda (thunk) (thread thunk #:pool 'own))
                   thread))]
      [(resyntax)
       (values (lambda ()
                 ((Runner-resyntax runner)
                  (Source-Snapshot-text snapshot)
                  (Source-Snapshot-uri snapshot)))
               thread)]))
  (start-worker! (Source-Controller-events controller) custodian (Runner-timeout runner) work
                 #:launch launch
                 #:fail (lambda (reason) (Analysis-Outcome 'failed reason))
                 #:exited 'worker-exited
                 #:timed-out 'analysis-timeout
                 #:done (lambda (result) (Job-Done job result))))

(define (finish-job! controller job result)
  (when (and (current-job? controller job) (eq? 'running (Job-state job)))
    (define outcome
      ;; Acceptance can reject a malformed executor product. The runner has
      ;; already exited, so this failure must release the slot and settle here.
      (with-handlers ([(lambda (_) #t) (lambda (reason) (Analysis-Outcome 'failed reason))])
        (cond
          [(not (job-current-source? controller job)) (Analysis-Outcome 'stopped 'source-changed)]
          [(Analysis-Outcome? result) result]
          [(eq? 'analysis (Job-kind job)) (accept-analysis! controller job result)]
          [else (accept-resyntax! controller job result)])))
    (case (Job-kind job)
      [(analysis) (finish-analysis! controller job outcome)]
      [(resyntax) (finish-resyntax! controller job outcome)])))

(define (finish-analysis! controller job outcome)
  (define source (job-source job))
  (define buffer (source-buffer controller source))
  (define succeeded? (eq? 'succeeded (Analysis-Outcome-state outcome)))
  (unless succeeded?
    (remove-retained! controller source)
    (when buffer
      (with-write-safedoc buffer (lambda (sd) (set-SafeDoc-available! sd #f)))))
  ;; Acceptance installs the trace before broadcasting success to queries.
  (settle-job! controller job outcome)
  (when (and succeeded? buffer (get-resyntax-enabled)
             (Runner-resyntax (Source-Controller-runner controller)))
    (enqueue-job! controller (Job-snapshot job) 'resyntax job)))

(define (finish-resyntax! controller job outcome)
  (settle-job! controller job outcome)
  (unless (eq? 'succeeded (Analysis-Outcome-state outcome))
    (log-racket-langserver-info "Resyntax discarded: ~v" outcome)))

(define (install-analysis! controller sd snapshot product)
  (define doc (SafeDoc-doc sd))
  (define contribution (Analysis-Product-contribution product))
  (doc-update-trace! doc (Analysis-Product-trace product) contribution (Doc-version doc))
  (set-SafeDoc-available! sd (Available-Analysis snapshot product))
  (when contribution
    (workspace-set-contribution! current-workspace contribution))
  (publish-diagnostics! controller (Doc-uri doc) (Analysis-Product-diagnostics product)))

(define (accept-analysis! controller job product)
  (define succeeded? (Analysis-Product-succeeded? product))
  ;; Validate executor products before installation. Document assembly owns
  ;; contribution eligibility; an invalid executor must not bypass that boundary.
  (when (and succeeded?
             (not (and (Analysis-Product-facts product)
                       (if (source-file-path (job-source job))
                           (Analysis-Product-contribution product)
                           (not (Analysis-Product-contribution product))))))
    (error 'accept-analysis! "successful analysis requires facts and a file-only contribution"))
  (define snapshot (Job-snapshot job))
  (define source (job-source job))
  (define buffer (source-buffer controller source))
  (cond
    [buffer
     (with-write-safedoc buffer
       (lambda (sd)
         (cond
           [succeeded? (install-analysis! controller sd snapshot product)]
           [else
            (set-SafeDoc-available! sd #f)
            (publish-diagnostics! controller (Doc-uri (SafeDoc-doc sd))
                                  (Analysis-Product-diagnostics product))])))]
    [(and succeeded? (eq? 'disk (Source-Snapshot-origin snapshot)))
     (retain! controller source (Available-Analysis snapshot product))])
  (Analysis-Outcome (if succeeded? 'succeeded 'unsuccessful) (Analysis-Product-facts product)))

(define (accept-resyntax! controller job results)
  (define buffer (source-buffer controller (job-source job)))
  (when (and buffer (current-job? controller (Job-parent job)))
    (with-write-doc buffer
      (lambda (doc)
        (doc-update-resyntax-result! doc results)
        (publish-diagnostics! controller (Doc-uri doc) (->jsexpr (doc-diagnostics doc))))))
  (Analysis-Outcome 'succeeded #f))
