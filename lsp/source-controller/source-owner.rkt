#lang racket/base

;; Consumer-only source ownership and acceptance policy. This module keeps the
;; controller record private and coordinates its isolated subsystems. Command
;; transport lives in ../source-controller.rkt; dependencies point toward this
;; owner, never back to that public interface. See ../analysis-lifecycle.md.
(require racket/async-channel
         racket/class
         racket/contract
         racket/match
         "../../common/debug.rkt"
         "../../common/source.rkt"
         "../../common/settings.rkt"
         "../../doclib/doc.rkt"
         "../../doclib/external/resyntax.rkt"
         "../../workspace/source-store.rkt"
         "../../workspace/current.rkt"
         "../analysis.rkt"
         "../safedoc.rkt"
         "../resyntax-place.rkt"
         "analysis-scheduler.rkt"
         "analysis-publisher.rkt"
         "closed-analysis-cache.rkt"
         "completion-event.rkt"
         "disk-reader.rkt")

(provide make-source-controller
         Source-Controller?
         current-source-controller
         source-controller-events
         source-controller-shutdown-evt
         source-controller-stopped?
         source-controller-observe!
         source-controller-acquire-analysis!
         source-controller-current-view?
         source-controller-handle-completion!
         source-controller-pump!
         source-controller-open!
         source-controller-edit!
         source-controller-close!
         source-controller-invalidate!
         source-controller-set-notifier!
         source-controller-shutdown!
         source-controller-view
         source-controller-snapshot
         source-controller-ensure-analysis!
         source-controller-refresh-analysis!
         (struct-out Analysis-Handle)
         (struct-out Source-Result)
         (struct-out Source-Failure)
         (struct-out Disk-Missing))

;; Failed command or disk read. `reason` is a symbol such as `shutdown`,
;; `closed`, `reader-exited`, or `read-timeout`, or the value raised while
;; handling the command or reading the source.
(struct/contract Source-Failure
  ([reason any/c])
  #:transparent)

;; Current knowledge of one source after observation.
;;
;; view: Present for every file, including failed or missing reads. `#f` only
;;   for a virtual source with no open buffer.
;; snapshot: Frozen text when buffer or disk text is available; otherwise `#f`.
;; error: Failure of this observation (`closed`, `reader-exited`, `read-timeout`,
;;   or a raised reader error). Shutdown is reported as a bare `Source-Failure`
;;   on the command reply, not in this field. A missing file is `file-not-found`
;;   on the view, with no error.
(struct/contract Source-Result
  ([view (or/c #f Source-View?)]
   [snapshot (or/c #f Source-Snapshot?)]
   [error (or/c #f Source-Failure?)])
  #:transparent)

;; Private composition root. Subsystem representations and mutable fields
;; are available only through their own operations.
(struct/contract Source-Controller
  ([store Source-Store?]
   [events async-channel?]
   [shutdown semaphore?]
   [reader Disk-Reader?]
   [scheduler Analysis-Scheduler?]
   [closed-analysis Closed-Analysis-Cache?]
   [publisher Analysis-Publisher?]))

;; Timeouts are seconds. The workspace supplies publication only, not session
;; isolation for request-layer workspace access. Zero cache capacity disables
;; closed-file retention; #f Resyntax disables its follow-on work.
(define (make-source-controller #:workspace [workspace current-workspace]
                                #:read [read read-source-text]
                                #:read-timeout [read-timeout 3]
                                #:analyze [analyze analyze-source]
                                #:analysis-slots [slots 4]
                                #:analysis-timeout [analysis-timeout 90]
                                #:closed-analysis-limit [closed-analysis-limit 64]
                                #:resyntax [resyntax (and (resyntax-available?) run-resyntax/in-place)])
  (define events (make-async-channel))
  (Source-Controller (make-source-store) events (make-semaphore 0)
                     (make-disk-reader events #:read read #:timeout read-timeout)
                     (make-analysis-scheduler events #:analyze analyze #:resyntax resyntax
                                              #:slots slots #:timeout analysis-timeout)
                     (make-closed-analysis-cache closed-analysis-limit)
                     (make-analysis-publisher workspace)))

(define current-source-controller (make-parameter (make-source-controller)))

(define (source-controller-set-notifier! controller notify)
  (set-analysis-notifier! (Source-Controller-publisher controller) notify))

(define (source-controller-events controller)
  (Source-Controller-events controller))

(define (source-controller-shutdown-evt controller)
  (semaphore-peek-evt (Source-Controller-shutdown controller)))

(define (source-controller-stopped? controller)
  (evt-ready? (source-controller-shutdown-evt controller)))

(define (source-buffer controller source)
  (source-store-buffer (Source-Controller-store controller) source))

(define (source-controller-view controller source)
  (source-store-view (Source-Controller-store controller) source))

;; Buffer text under the SafeDoc lock, or cached disk text. `#f` when the
;; source is closed and virtual, or a file whose disk content is unknown or
;; missing.
(define (source-controller-snapshot controller source)
  (define buffer (source-buffer controller source))
  (if buffer
      (with-read-safedoc buffer
        (lambda (sd)
          (define doc (SafeDoc-doc sd))
          (Source-Snapshot (Doc-source doc) (Source-View-revision (SafeDoc-source-view sd)) 'buffer
                           (string->immutable-string (doc-get-text doc)))))
      (source-store-disk-snapshot (Source-Controller-store controller) source)))

(define (current-result controller source [error #f])
  (Source-Result (source-controller-view controller source)
                 (source-controller-snapshot controller source) error))

(define (snapshot-current? controller snapshot)
  (define view (source-controller-view controller (Source-Snapshot-source snapshot)))
  (and view
       (eq? (Source-Snapshot-revision snapshot)
            (Source-View-revision view))))

;; Worker envelopes are interpreted here so the public dispatcher has no
;; dependency on reader/scheduler representations. Acceptance still runs inline.
(define (source-controller-handle-completion! controller event)
  (match event
    [(? Disk-Read-Completed?)
     (finish-disk-read! (Source-Controller-reader controller) event
                        (lambda (view result) (accept-disk-observation! controller view result)))]
    [(? Analysis-Completed?)
     (finish-analysis-task! (Source-Controller-scheduler controller) event
                            (lambda (snapshot kind completion parent result)
                              (accept-analysis-result! controller snapshot kind completion parent result)))]))

(define (source-controller-current-view? controller view)
  (eq? view (source-controller-view controller (Source-View-source view))))

(define (source-controller-pump! controller)
  (pump-analysis-scheduler! (Source-Controller-scheduler controller)
                            (lambda (snapshot) (snapshot-current? controller snapshot))))

(define (source-controller-observe! controller source)
  (match (source-controller-view controller source)
    [#f (current-result controller source (Source-Failure 'closed))]
    [(struct* Source-View ([origin 'buffer])) (current-result controller source)]
    [view (request-disk-read! (Source-Controller-reader controller) view)]))

;; Commit source knowledge and invalidate obsolete analysis before the reader
;; answers observers or starts the next read from the successor disk view.
(define (accept-disk-observation! controller started-view result)
  (define source (Source-View-source started-view))
  (define-values (content-state text error)
    (match result
      [(? string? text) (values 'text-available text #f)]
      [(Disk-Missing) (values 'file-not-found #f #f)]
      [(Disk-Read-Failure reason) (values 'unknown #f (Source-Failure reason))]))
  (source-store-observe! (Source-Controller-store controller) started-view content-state text)
  (define next-view (source-controller-view controller source))
  (unless (eq? started-view next-view)
    (cancel-source-analysis! (Source-Controller-scheduler controller) source))
  (values (current-result controller source error) next-view))

(define (cancel-read! controller source)
  (cancel-disk-read! (Source-Controller-reader controller) source (current-result controller source)))

;; Availability and freshness cross buffers, closed storage, and scheduling;
;; that policy belongs here rather than in the scheduler.
(define (available-analysis controller snapshot)
  (define buffer (source-buffer controller (Source-Snapshot-source snapshot)))
  (if buffer
      (with-read-safedoc buffer SafeDoc-available)
      (find-closed-analysis! (Source-Controller-closed-analysis controller) snapshot)))

(define (reusable-completion controller snapshot)
  (define available (available-analysis controller snapshot))
  (and available
       (eq? (Source-Snapshot-revision snapshot)
            (Source-Snapshot-revision (Available-Analysis-snapshot available)))
       (Available-Analysis-completion-evt available)))

(define (source-controller-ensure-analysis! controller snapshot #:refresh? [refresh? #f])
  (cond
    [(or (source-controller-stopped? controller) (not (snapshot-current? controller snapshot)))
     (ready-evt (Analysis-Outcome 'stopped 'ineligible))]
    [else
     (define source (Source-Snapshot-source snapshot))
     (define scheduler (Source-Controller-scheduler controller))
     (define active (scheduled-analysis-event scheduler source))
     (define reused (and (not refresh?) (not active) (reusable-completion controller snapshot)))
     (define event (or reused (request-analysis! scheduler snapshot)))
     (define buffer (source-buffer controller source))
     (when buffer
       (with-write-safedoc buffer (lambda (sd) (set-SafeDoc-analysis-completion! sd event))))
     (source-controller-pump! controller)
     event]))

(define (source-controller-acquire-analysis! controller snapshot)
  (define event (source-controller-ensure-analysis! controller snapshot))
  (define available (and (not (evt-ready? event)) (available-analysis controller snapshot)))
  (Analysis-Handle
    (and available (Analysis-Product-summary (Available-Analysis-product available))) event))

(define (source-controller-refresh-analysis! controller source)
  (define snapshot (source-controller-snapshot controller source))
  (when snapshot (source-controller-ensure-analysis! controller snapshot #:refresh? #t))
  (void))

(define (discard-analysis! controller source)
  (discard-closed-analysis! (Source-Controller-closed-analysis controller) source)
  (define buffer (source-buffer controller source))
  (when buffer
    (with-write-safedoc buffer (lambda (sd) (set-SafeDoc-available! sd #f)))))

(define (record-analysis! controller snapshot product completion)
  (define source (Source-Snapshot-source snapshot))
  (define succeeded? (Analysis-Product-succeeded? product))
  (define contribution (Analysis-Product-contribution product))
  ;; Validate injected executor products at acceptance for both destinations.
  (when (and succeeded?
             (not (and (Analysis-Product-summary product)
                       (if (source-file-path source) contribution (not contribution)))))
    (error 'record-analysis! "successful analysis requires facts and a file-only contribution"))
  (define buffer (source-buffer controller source))
  (cond
    [buffer
     (with-write-safedoc buffer
       (lambda (sd)
         (publish-document-analysis! (Source-Controller-publisher controller)
                                     sd snapshot product completion)))]
    [(and succeeded? (eq? 'disk (Source-Snapshot-origin snapshot)))
     (store-closed-analysis! (Source-Controller-closed-analysis controller)
                             (Available-Analysis snapshot product completion))])
  (Analysis-Outcome (if succeeded? 'succeeded 'errors) (Analysis-Product-summary product)))

;; A handled acceptance failure clears exact eligibility before the scheduler
;; broadcasts failure. Mutable traces are never returned to request threads.
(define (accept-analysis-result! controller snapshot kind completion parent result)
  (define source (Source-Snapshot-source snapshot))
  (define outcome
    (with-handlers ([(lambda (_) #t) (lambda (reason) (Analysis-Outcome 'failed reason))])
      (cond
        [(not (snapshot-current? controller snapshot)) (Analysis-Outcome 'stopped 'source-changed)]
        [(Analysis-Outcome? result) result]
        [(eq? kind 'analysis) (record-analysis! controller snapshot result completion)]
        [else
         (define buffer (source-buffer controller source))
         (when buffer
           (with-write-safedoc buffer
             (lambda (sd)
               (publish-document-resyntax! (Source-Controller-publisher controller) sd parent result))))
         (Analysis-Outcome 'succeeded #f)])))
  (define succeeded? (eq? 'succeeded (Analysis-Outcome-state outcome)))
  (case kind
    [(analysis)
     (unless succeeded? (discard-analysis! controller source))
     (when (and succeeded? (source-buffer controller source) (get-resyntax-enabled))
       (schedule-resyntax! (Source-Controller-scheduler controller) snapshot completion))]
    [(resyntax)
     (unless succeeded? (log-racket-langserver-info "Resyntax discarded: ~v" outcome))])
  outcome)

;; Register the buffer as a new revision. Matching closed-file text may restore
;; its trace and diagnostics; restoration does not supply a completion event
;; and does not restore Resyntax. Rebind the contribution to this open's source
;; so publication uses the incoming URI. Always schedule a fresh attempt.
(define (source-controller-open! controller source safe-doc)
  (with-write-safedoc safe-doc
    (lambda (sd)
      (set-SafeDoc-source-view! sd
                                (source-store-open! (Source-Controller-store controller)
                                                    source sd))))
  (cancel-source-analysis! (Source-Controller-scheduler controller) source)
  (cancel-read! controller source)
  ;; Take ownership even on a mismatch: an older text must not remain eligible
  ;; after this buffer starts adjusting its trace.
  (define available (take-closed-analysis! (Source-Controller-closed-analysis controller) source))
  (define snapshot (and available (source-controller-snapshot controller source)))
  (when (and available
             (equal? (Source-Snapshot-text snapshot)
                     (Source-Snapshot-text (Available-Analysis-snapshot available))))
    (define product (Available-Analysis-product available))
    (define restored-product
      (struct-copy Analysis-Product product
        [contribution (send (Analysis-Product-trace product) get-contribution
                            #:source (Source-Snapshot-source snapshot))]))
    (with-write-safedoc safe-doc
      (lambda (sd)
        (publish-document-analysis! (Source-Controller-publisher controller) sd snapshot restored-product))))
  (source-controller-refresh-analysis! controller source))

;; Clear the buffer's fresh completion and available result. The document
;; trace stays in place.
(define (clear-analysis-projections! sd)
  (set-SafeDoc-analysis-completion! sd #f)
  (set-SafeDoc-available! sd #f))

;; `safe-doc` is the already-registered buffer; this does not look up by URI.
;; Invalidate eligibility before adjusting the mutable trace. The controller
;; owns the lock and schedules the successor only after the whole edit is
;; visible. `mutate-doc` receives the Doc, must update its text and client
;; version, and must not acquire the same buffer lock.
(define (source-controller-edit! controller safe-doc mutate-doc)
  (define source
    (with-write-safedoc safe-doc
      (lambda (sd)
        (define source (Doc-source (SafeDoc-doc sd)))
        (set-SafeDoc-source-view!
          sd (source-store-edit! (Source-Controller-store controller) source))
        (clear-analysis-projections! sd)
        (discard-closed-analysis! (Source-Controller-closed-analysis controller) source)
        (cancel-source-analysis! (Source-Controller-scheduler controller) source)
        (mutate-doc (SafeDoc-doc sd))
        source)))
  (source-controller-refresh-analysis! controller source))

;; Drop buffer authority without a read. File content becomes unknown; virtual
;; sources are removed because they have no disk fallback. Retain a completed
;; product only when it still matches the revision being closed. Capture the
;; opened URI before clearing projections so empty diagnostics keep that
;; spelling.
(define (source-controller-close! controller source safe-doc)
  (define uri
    (with-write-safedoc safe-doc
      (lambda (sd)
        (define available (SafeDoc-available sd))
        (define closed-view (source-store-close! (Source-Controller-store controller) source))
        (when (and closed-view available
                   (eq? (Source-Snapshot-revision (Available-Analysis-snapshot available))
                        (Source-View-revision (SafeDoc-source-view sd))))
          (doc-detach-analysis! (SafeDoc-doc sd))
          (store-closed-analysis! (Source-Controller-closed-analysis controller) available))
        (set-SafeDoc-source-view! sd #f)
        (clear-analysis-projections! sd)
        (Doc-uri (SafeDoc-doc sd)))))
  (cancel-source-analysis! (Source-Controller-scheduler controller) source)
  (withdraw-document-analysis! (Source-Controller-publisher controller) source uri)
  (cancel-read! controller source)
  ;; Invalidation freed slots; start queued attempts for other sources.
  (source-controller-pump! controller))

;; Files only. Drop retained data and cached disk text. An open buffer keeps
;; its document, gets a new revision, and is re-analyzed. A file without a
;; buffer returns to unknown disk without reading.
(define (source-controller-invalidate! controller source)
  (when (source-file-path source)
    (discard-closed-analysis! (Source-Controller-closed-analysis controller) source)
    (define store (Source-Controller-store controller))
    (define buffer (source-store-buffer store source))
    (if buffer
        (with-write-safedoc buffer
          (lambda (sd)
            (set-SafeDoc-source-view! sd (source-store-invalidate! store source))
            (clear-analysis-projections! sd)))
        (source-store-invalidate! store source))
    (cancel-source-analysis! (Source-Controller-scheduler controller) source)
    (cancel-read! controller source)
    (when buffer (source-controller-refresh-analysis! controller source))
    ;; Invalidation freed slots; start queued attempts for other sources.
    (source-controller-pump! controller)))

;; Subsystems settle their own pending work; shutdown does not inspect tables,
;; queues, lanes, attempts, workers, or completion-event representations.
(define (source-controller-shutdown! controller)
  (unless (source-controller-stopped? controller)
    (semaphore-post (Source-Controller-shutdown controller))
    (stop-analysis-scheduler! (Source-Controller-scheduler controller))
    (stop-disk-reader! (Source-Controller-reader controller) (Source-Failure 'shutdown))
    (clear-closed-analysis! (Source-Controller-closed-analysis controller))))
