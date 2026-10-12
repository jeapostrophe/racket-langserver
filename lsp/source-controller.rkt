#lang racket/base

;; Per-session owner of source records, ordered disk reads, shared analysis
;; jobs, bounded execution and result acceptance. The session consumer applies
;; every mutation and table read. Other threads submit commands and wait on
;; reply or change events; disk reads and analysis run on private workers.
;; Source ids are exact filepaths or tagged complete virtual URIs. Virtual
;; backend locations belong to snapshots, never to the source registry.
(require racket/async-channel
         racket/contract
         racket/file
         racket/match
         racket/port
         "../common/debug.rkt"
         "../common/json-util.rkt"
         "../common/path-util.rkt"
         "../common/settings.rkt"
         "../doclib/doc.rkt"
         "../doclib/external/resyntax.rkt"
         "../workspace/current.rkt"
         "../workspace/state.rkt"
         "../workspace/source-store.rkt"
         "analysis.rkt"
         "resyntax-place.rkt"
         "safedoc.rkt")

(provide make-source-controller
         Source-Controller?
         current-source-controller
         source-controller-events
         source-controller-call-evt
         source-controller-refresh-evt
         source-controller-handle!
         source-controller-view
         source-controller-snapshot
         source-controller-open!
         source-controller-edited!
         source-controller-close!
         source-controller-invalidate!
         source-controller-analyze!
         source-controller-acquire-analysis!
         source-controller-set-notifier!
         source-controller-shutdown-evt
         source-controller-shutdown!
         (struct-out Source-Result)
         (struct-out Source-Failure)
         (struct-out Disk-Missing))

(struct/contract Source-Failure
  ([reason any/c])
  #:transparent)

(struct/contract Source-Result
  ([view Source-View?]
   [snapshot (or/c #f Source-Snapshot?)]
   [error (or/c #f Source-Failure?)])
  #:transparent)

(struct/contract Disk-Missing () #:transparent)

(struct/contract Completion
  ([result (and/c box? (not/c immutable?))]
   [ready semaphore?])
  #:transparent)

(struct/contract Read-Attempt
  ([ticket Source-View?]
   [batch Completion?]
   [custodian custodian?])
  #:transparent)

(struct/contract Read-Lane
  ([active Read-Attempt?]
   [next (or/c #f Completion?)])
  #:mutable)

(struct/contract Source-Controller
  ([store (and/c hash? (not/c immutable?))]
   [events async-channel?]
   [lanes (and/c hash? (not/c immutable?))]
   [read (-> path? any/c)]
   [timeout (and/c real? (>=/c 0))]
   [shutdown semaphore?]
   [jobs (and/c hash? (not/c immutable?))]
   [queue list?]
   [running exact-nonnegative-integer?]
   [slots exact-positive-integer?]
   [analyze (-> Source-Snapshot? Analysis-Product?)]
   [resyntax (or/c #f (-> string? string? list?))]
   [analysis-timeout (and/c real? (>=/c 0))]
   [notify (-> string? any/c any/c)])
  #:mutable
  #:transparent)

;; Identity is the attempt object, not just its source revision. A retry of
;; the same revision cannot accept a terminal attempt's late completion.
(struct Job (snapshot kind parent completion state custodian) #:mutable)
(struct Job-Done (job result) #:transparent)

(struct/contract Owner-Call
  ([proc (-> Source-Controller? any/c)]
   [reply Completion?])
  #:transparent)

(struct/contract Refresh
  ([source-id source-id?]
   [reply Completion?])
  #:transparent)

(struct/contract Read-Done
  ([source-id source-id?]
   [attempt Read-Attempt?]
   [result any/c])
  #:transparent)

(define (missing-file-errno? errno)
  (member errno '((2 . posix) (20 . posix) (2 . windows) (3 . windows))))

(define (read-source-text path)
  (with-handlers ([exn:fail:filesystem:errno?
                   (lambda (e)
                     (if (missing-file-errno? (exn:fail:filesystem:errno-errno e))
                         (Disk-Missing)
                         (raise e)))])
    (file->string path)))

(define (make-source-controller #:read [read read-source-text] #:read-timeout [timeout 90]
                                #:analyze [analyze analyze-source]
                                #:analysis-slots [slots 2]
                                #:analysis-timeout [analysis-timeout 90]
                                #:resyntax [resyntax (and (resyntax-available?) run-resyntax/in-place)])
  (Source-Controller (make-source-store) (make-async-channel) (make-hash)
                     read timeout (make-semaphore 0) (make-hash) '() 0
                     slots analyze resyntax analysis-timeout (lambda (_method _params) (void))))

(define (source-controller-set-notifier! controller notify)
  (set-Source-Controller-notify! controller notify))

(define current-source-controller (make-parameter (make-source-controller)))

(define (source-controller-events controller)
  (Source-Controller-events controller))

(define (shutdown-evt controller)
  (semaphore-peek-evt (Source-Controller-shutdown controller)))

(define source-controller-shutdown-evt shutdown-evt)

(define (stopped? controller)
  (and (sync/timeout 0 (shutdown-evt controller)) #t))

(define (make-completion)
  (Completion (box #f) (make-semaphore 0)))

(define (completion-evt completion)
  (wrap-evt (semaphore-peek-evt (Completion-ready completion))
            (lambda (_) (unbox (Completion-result completion)))))

(define (complete! completion result)
  ;; The first result wins; replacing it could disagree with earlier waiters.
  (unless (sync/timeout 0 (semaphore-peek-evt (Completion-ready completion)))
    (set-box! (Completion-result completion) result)
    (semaphore-post (Completion-ready completion))))

(define (submit controller make-command)
  (define reply (make-completion))
  (async-channel-put (Source-Controller-events controller) (make-command reply))
  (choice-evt (completion-evt reply)
              (wrap-evt (shutdown-evt controller)
                        (lambda (_) (Source-Failure 'shutdown)))))

(define (source-controller-call-evt controller proc)
  (submit controller (lambda (reply) (Owner-Call proc reply))))

;; Each refresh is a barrier: it cannot join a read that started before the
;; owner accepted the command. Pending callers share the next ordered read.
(define (source-controller-refresh-evt controller source-id)
  (replace-evt (submit controller (lambda (reply) (Refresh source-id reply)))
               (lambda (result)
                 (if (evt? result)
                     result
                     (wrap-evt always-evt (lambda (_) result))))))

(define (source-controller-view controller source-id)
  (source-store-view (Source-Controller-store controller) source-id))

(define (source-controller-snapshot controller source-id)
  (define store (Source-Controller-store controller))
  (define buffer (source-store-buffer store source-id))
  (if buffer
      (with-read-safedoc buffer
        (lambda (sd)
          (define view (SafeDoc-source-view sd))
          (Source-Snapshot (Doc-source (SafeDoc-doc sd)) (Source-View-revision view) 'buffer
                           (string->immutable-string (doc-get-text (SafeDoc-doc sd))))))
      (source-store-disk-snapshot store source-id)))

(define (current-result controller source-id [error #f])
  (Source-Result (source-controller-view controller source-id)
                 (source-controller-snapshot controller source-id) error))

(define (complete-lane! lane result)
  (complete! (Read-Attempt-batch (Read-Lane-active lane)) result)
  (when (Read-Lane-next lane)
    (complete! (Read-Lane-next lane) result)))

;; Open, close, and invalidate abandon the in-flight read. Answer both the
;; active batch and the queued batch with the current view, which may be
;; unobserved. The abandoned read must not stay pending or be installed.
(define (invalidate-reads! controller source-id)
  (define lanes (Source-Controller-lanes controller))
  (define lane (hash-ref lanes source-id #f))
  (when lane
    (define active (Read-Lane-active lane))
    (custodian-shutdown-all (Read-Attempt-custodian active))
    (define result (current-result controller source-id))
    (complete-lane! lane result)
    (hash-remove! lanes source-id)))

(define (source-controller-open! controller source-id safe-doc)
  (with-write-safedoc safe-doc
    (lambda (sd)
      (set-SafeDoc-source-view! sd
                                (source-store-open! (Source-Controller-store controller)
                                                    source-id sd))))
  (invalidate-jobs! controller source-id)
  (invalidate-reads! controller source-id))

;; Called within the edit's existing write-lock section. This updates the
;; authoritative record and its SafeDoc projection before another reader enters.
(define (source-controller-edited! controller source-id sd)
  (set-SafeDoc-source-view! sd
                            (source-store-edit! (Source-Controller-store controller) source-id))
  (set-SafeDoc-analysis! sd #f)
  (invalidate-jobs! controller source-id))

;; Close drops buffer authority without a read. Files become unobserved;
;; virtual sources become closed and have no disk fallback.
(define (source-controller-close! controller source-id safe-doc)
  (define uri
    (with-write-safedoc safe-doc
      (lambda (sd)
        (source-store-close! (Source-Controller-store controller) source-id)
        (set-SafeDoc-source-view! sd #f)
        (set-SafeDoc-analysis! sd #f)
        (Doc-uri (SafeDoc-doc sd)))))
  (invalidate-jobs! controller source-id)
  (when (path? source-id)
    (workspace-remove-path! current-workspace source-id))
  (publish-diagnostics! controller uri '())
  (invalidate-reads! controller source-id)
  (pump-jobs! controller))

(define (source-controller-invalidate! controller source-id)
  (when (path? source-id)
    (define store (Source-Controller-store controller))
    (define buffer (source-store-buffer store source-id))
    (if buffer
        (with-write-safedoc buffer
          (lambda (sd)
            (set-SafeDoc-source-view! sd (source-store-invalidate! store source-id))
            (set-SafeDoc-analysis! sd #f)))
        (source-store-invalidate! store source-id))
    (invalidate-jobs! controller source-id)
    (invalidate-reads! controller source-id)
    (when buffer (source-controller-analyze! controller source-id))
    (pump-jobs! controller)))

(define (start-read! controller source-id batch)
  (define deadline (+ (current-inexact-monotonic-milliseconds)
                      (* 1000 (Source-Controller-timeout controller))))
  (define custodian (make-custodian))
  (define attempt
    (Read-Attempt (source-controller-view controller source-id) batch custodian))
  (define result-ch (make-async-channel))
  ;; Reader threads inherit this port. An injected read must not write the
  ;; LSP stdout, even when the consumer's own redirection changes.
  (parameterize ([current-custodian custodian]
                 [current-output-port (open-output-nowhere)])
    (define worker
      (thread
        (lambda ()
          (define result
            (with-handlers ([(lambda (_) #t) Source-Failure])
              (define value ((Source-Controller-read controller) source-id))
              (if (string? value) (string->immutable-string value) value)))
          (async-channel-put result-ch result))))
    (thread
      (lambda ()
        (define outcome
          (sync result-ch
                (wrap-evt (thread-dead-evt worker)
                          (lambda (_)
                            (or (sync/timeout 0 result-ch) (Source-Failure 'reader-exited))))
                (wrap-evt (alarm-evt deadline #t)
                          (lambda (_) (Source-Failure 'read-timeout)))))
        (async-channel-put (Source-Controller-events controller)
                           (Read-Done source-id attempt outcome))
        ;; Clean up even while the consumer is busy; a timed-out read must not
        ;; keep running until its completion is accepted.
        (custodian-shutdown-all custodian))))
  attempt)

(define (refresh! controller source-id reply)
  (define view (source-controller-view controller source-id))
  (cond
    [(Virtual-Id? source-id)
     (complete! reply (current-result controller source-id
                                      (and (eq? 'closed (Source-View-origin view))
                                           (Source-Failure 'closed))))]
    ;; An open buffer is the source text. Do not read disk over it.
    [(eq? 'buffer (Source-View-origin view))
     (complete! reply (current-result controller source-id))]
    [else
     (define lanes (Source-Controller-lanes controller))
     (define lane (hash-ref lanes source-id #f))
     (define batch
       (cond
         [(not lane)
          (define created (make-completion))
          (hash-set! lanes source-id
                     (Read-Lane (start-read! controller source-id created) #f))
          created]
         [else
          (unless (Read-Lane-next lane)
            (set-Read-Lane-next! lane (make-completion)))
          (Read-Lane-next lane)]))
     (complete! reply (completion-evt batch))]))

;; Apply a completion only while its attempt is still the lane's active read.
;; Invalidation already removed the lane and answered its waiters.
(define (finish-read! controller source-id attempt result)
  (define lanes (Source-Controller-lanes controller))
  (define lane (hash-ref lanes source-id #f))
  (when (and lane (eq? attempt (Read-Lane-active lane)))
    (custodian-shutdown-all (Read-Attempt-custodian attempt))
    (define-values (state text error)
      (match result
        [(? string? text) (values 'present text #f)]
        [(Disk-Missing) (values 'missing #f #f)]
        [(? Source-Failure? failure) (values 'unavailable #f failure)]
        [_ (values 'unavailable #f (Source-Failure result))]))
    (source-store-observe! (Source-Controller-store controller)
                           (Read-Attempt-ticket attempt) state text)
    ;; A new disk view stops jobs captured under the view it replaced.
    ;; An unchanged reread keeps the ticket and its jobs.
    (unless (eq? (Read-Attempt-ticket attempt) (source-controller-view controller source-id))
      (invalidate-jobs! controller source-id))
    (complete! (Read-Attempt-batch attempt) (current-result controller source-id error))
    (define next (Read-Lane-next lane))
    (cond
      [next
       (set-Read-Lane-next! lane #f)
       (set-Read-Lane-active! lane (start-read! controller source-id next))]
      [else
       (hash-remove! lanes source-id)])))

(define (source-controller-handle! controller event)
  (match event
    [(Owner-Call proc reply)
     (complete! reply
                (if (stopped? controller)
                    (Source-Failure 'shutdown)
                    (with-handlers ([(lambda (_) #t) Source-Failure])
                      (proc controller))))]
    [(Refresh source-id reply)
     (if (stopped? controller)
         (complete! reply (Source-Failure 'shutdown))
         (refresh! controller source-id reply))]
    [(Read-Done source-id attempt result)
     (finish-read! controller source-id attempt result)]
    [(Job-Done job result)
     (finish-job! controller job result)])
  (pump-jobs! controller))

(define (source-controller-shutdown! controller)
  (unless (stopped? controller)
    (semaphore-post (Source-Controller-shutdown controller))
    (for ([job (in-list (hash-values (Source-Controller-jobs controller)))])
      (settle-job! controller job (Analysis-Outcome 'stopped 'shutdown)))
    (hash-clear! (Source-Controller-jobs controller))
    (set-Source-Controller-queue! controller '())
    (for ([lane (in-hash-values (Source-Controller-lanes controller))])
      (define active (Read-Lane-active lane))
      (custodian-shutdown-all (Read-Attempt-custodian active))
      (complete-lane! lane (Source-Failure 'shutdown)))
    (hash-clear! (Source-Controller-lanes controller))))

(define (job-key job)
  (cons (Source-Snapshot-source-id (Job-snapshot job)) (Job-kind job)))

(define (current-job? controller job)
  (eq? job (hash-ref (Source-Controller-jobs controller) (job-key job) #f)))

(define (job-current-source? controller job)
  (define snapshot (Job-snapshot job))
  (eq? (Source-Snapshot-revision snapshot)
       (Source-View-revision
         (source-controller-view controller (Source-Snapshot-source-id snapshot)))))

(define (settle-job! controller job outcome)
  (unless (eq? 'terminal (Job-state job))
    (when (eq? 'running (Job-state job))
      (set-Source-Controller-running! controller (sub1 (Source-Controller-running controller))))
    (when (Job-custodian job)
      (custodian-shutdown-all (Job-custodian job)))
    (set-Job-state! job 'terminal)
    (complete! (Job-completion job) outcome)
    ;; Only successful analysis is cached; failed attempts remain observable
    ;; by their waiters, but a later acquisition can retry the same revision.
    (unless (and (eq? 'analysis (Job-kind job))
                 (eq? 'succeeded (Analysis-Outcome-state outcome)))
      (when (current-job? controller job)
        (hash-remove! (Source-Controller-jobs controller) (job-key job))))))

(define (invalidate-jobs! controller source-id)
  (for ([kind (in-list '(analysis resyntax))])
    (define key (cons source-id kind))
    (define job (hash-ref (Source-Controller-jobs controller) key #f))
    (when job
      (settle-job! controller job (Analysis-Outcome 'stopped 'source-changed))
      (hash-remove! (Source-Controller-jobs controller) key)))
  (set-Source-Controller-queue!
    controller
    (filter (lambda (job) (current-job? controller job)) (Source-Controller-queue controller))))

(define (enqueue-job! controller snapshot kind [parent #f])
  (define job (Job snapshot kind parent (make-completion) 'queued #f))
  (hash-set! (Source-Controller-jobs controller) (job-key job) job)
  (set-Source-Controller-queue! controller (append (Source-Controller-queue controller) (list job)))
  job)

;; This is one consumer operation: snapshots supplied here were acquired from
;; this controller, and are checked again before joining or creating an attempt.
(define (source-controller-acquire-analysis! controller snapshot)
  (define source-id (Source-Snapshot-source-id snapshot))
  (define view (source-controller-view controller source-id))
  (cond
    [(or (stopped? controller)
         (not (eq? (Source-Snapshot-revision snapshot) (Source-View-revision view))))
     (wrap-evt always-evt (lambda (_) (Analysis-Outcome 'stopped 'ineligible)))]
    [else
     (define job
       (hash-ref (Source-Controller-jobs controller) (cons source-id 'analysis)
                 (lambda () (enqueue-job! controller snapshot 'analysis))))
     (define event (completion-evt (Job-completion job)))
     (define buffer (source-store-buffer (Source-Controller-store controller) source-id))
     (when buffer
       (with-write-safedoc buffer (lambda (sd) (set-SafeDoc-analysis! sd event))))
     (pump-jobs! controller)
     event]))

(define (source-controller-analyze! controller source-id)
  (define snapshot (source-controller-snapshot controller source-id))
  (when snapshot (source-controller-acquire-analysis! controller snapshot))
  (void))

(define (start-job! controller job)
  (define snapshot (Job-snapshot job))
  (define deadline (+ (current-inexact-monotonic-milliseconds)
                      (* 1000 (Source-Controller-analysis-timeout controller))))
  (define custodian (make-custodian))
  (set-Job-state! job 'running)
  (set-Job-custodian! job custodian)
  (set-Source-Controller-running! controller (add1 (Source-Controller-running controller)))
  (define result-ch (make-async-channel))
  ;; Reading, lexing, expansion and extraction all inherit this output scope.
  (parameterize ([current-custodian custodian]
                 [current-output-port (open-output-nowhere)])
    (define worker
      (thread
        (lambda ()
          (define result
            (with-handlers ([(lambda (_) #t) (lambda (reason) (Analysis-Outcome 'failed reason))])
              (case (Job-kind job)
                [(analysis) ((Source-Controller-analyze controller) snapshot)]
                [(resyntax) ((Source-Controller-resyntax controller)
                             (Source-Snapshot-text snapshot)
                             (source-uri (Source-Snapshot-source snapshot)))])))
          (async-channel-put result-ch result))))
    (thread
      (lambda ()
        (define result
          (sync result-ch
                (wrap-evt (thread-dead-evt worker)
                          (lambda (_)
                            (or (sync/timeout 0 result-ch)
                                (Analysis-Outcome 'failed 'worker-exited))))
                (wrap-evt (alarm-evt deadline #t)
                          (lambda (_) (Analysis-Outcome 'failed 'analysis-timeout)))))
        (async-channel-put (Source-Controller-events controller) (Job-Done job result))
        (custodian-shutdown-all custodian)))))

(define (pump-jobs! controller)
  (unless (stopped? controller)
    (let loop ()
      (when (and (< (Source-Controller-running controller) (Source-Controller-slots controller))
                 (pair? (Source-Controller-queue controller)))
        (define job (car (Source-Controller-queue controller)))
        (set-Source-Controller-queue! controller (cdr (Source-Controller-queue controller)))
        (cond
          [(and (current-job? controller job) (job-current-source? controller job))
           (start-job! controller job)]
          [else (settle-job! controller job (Analysis-Outcome 'stopped 'source-changed))])
        (loop)))))

(define (publish-diagnostics! controller uri diagnostics)
  ((Source-Controller-notify controller) "textDocument/publishDiagnostics"
                                         (hasheq 'uri uri 'diagnostics diagnostics)))

(define (accept-analysis! controller job product)
  (when (and (Analysis-Product-succeeded? product)
             (or (not (Analysis-Product-facts product))
                 (not (Analysis-Product-contribution product))))
    (error 'accept-analysis! "successful analysis requires facts and a contribution"))
  (define source-id (Source-Snapshot-source-id (Job-snapshot job)))
  (define buffer (source-store-buffer (Source-Controller-store controller) source-id))
  (when buffer
    (with-write-safedoc buffer
      (lambda (sd)
        (define doc (SafeDoc-doc sd))
        (when (Analysis-Product-succeeded? product)
          (define contribution (Analysis-Product-contribution product))
          (doc-update-trace! doc (Analysis-Product-trace product) contribution (Doc-version doc))
          (when (path? source-id)
            (workspace-set-contribution! current-workspace contribution)))
        (publish-diagnostics! controller (Doc-uri doc) (Analysis-Product-diagnostics product)))))
  (Analysis-Outcome (if (Analysis-Product-succeeded? product) 'succeeded 'unsuccessful)
                    (Analysis-Product-facts product)))

(define (accept-resyntax! controller job results)
  (define source-id (Source-Snapshot-source-id (Job-snapshot job)))
  (define buffer (source-store-buffer (Source-Controller-store controller) source-id))
  (when (and buffer (current-job? controller (Job-parent job)))
    (with-write-doc buffer
      (lambda (doc)
        (doc-update-resyntax-result! doc results)
        (publish-diagnostics! controller (Doc-uri doc) (->jsexpr (doc-diagnostics doc))))))
  (Analysis-Outcome 'succeeded #f))

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
    ;; Acceptance installs the trace before broadcasting success to queries.
    (settle-job! controller job outcome)
    (when (and (eq? 'analysis (Job-kind job))
               (eq? 'succeeded (Analysis-Outcome-state outcome))
               (source-store-buffer (Source-Controller-store controller)
                                    (Source-Snapshot-source-id (Job-snapshot job)))
               (get-resyntax-enabled) (Source-Controller-resyntax controller))
      (enqueue-job! controller (Job-snapshot job) 'resyntax job))
    (when (and (eq? 'resyntax (Job-kind job))
               (not (eq? 'succeeded (Analysis-Outcome-state outcome))))
      (log-racket-langserver-info "Resyntax discarded: ~v" outcome))))

(module+ test-support
  ;; Replay a captured runner message to exercise terminal-attempt rejection.
  (provide Job-Done Job-Done-job))
