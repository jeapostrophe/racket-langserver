#lang racket/base

;; Controller records, the job table, and read-only queries over them. Only the
;; session consumer reads or mutates these tables.
(require racket/async-channel
         racket/contract
         "../../common/source.rkt"
         "../../doclib/doc.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt"
         "../safedoc.rkt")

(provide (struct-out Source-Failure)
         (struct-out Source-Result)
         (struct-out Disk-Missing)
         (struct-out Disk-Reads)
         (struct-out Runner)
         (struct-out Retained-Cache)
         (struct-out Source-Controller)
         (struct-out Job)
         source-controller-shutdown-evt
         stopped?
         source-buffer
         controller-lanes
         controller-jobs
         publish-diagnostics!
         source-controller-view
         source-controller-snapshot
         current-result
         job-source
         job-key
         current-job?
         job-current-source?
         forget-job!)

(struct/contract Source-Failure
  ([reason any/c])
  #:transparent)

;; Refresh of an absent virtual source has no view or snapshot and reports
;; Source-Failure 'closed. File results always have a view, even on read failure.
(struct/contract Source-Result
  ([view (or/c #f Source-View?)]
   [snapshot (or/c #f Source-Snapshot?)]
   [error (or/c #f Source-Failure?)])
  #:transparent)

(struct/contract Disk-Missing () #:transparent)

;; `lanes` maps a source id to its ordered disk reads.
(struct/contract Disk-Reads
  ([read (-> path? any/c)]
   [timeout (and/c real? (>=/c 0))]
   [lanes (and/c hash? (not/c immutable?))]))

;; `jobs` maps (source-id . kind) to the current attempt; `queue` holds queued
;; attempts in FIFO order.
(struct/contract Runner
  ([analyze (-> Source-Snapshot? Analysis-Product?)]
   [resyntax (or/c #f (-> string? string? list?))]
   [timeout (and/c real? (>=/c 0))]
   [slots exact-positive-integer?]
   [jobs (and/c hash? (not/c immutable?))]
   [queue list?])
  #:mutable)

;; Only closed files occupy `entries`, oldest first. Live traces belong to
;; SafeDoc; eviction never removes active projections or changes authority.
(struct/contract Retained-Cache
  ([limit exact-nonnegative-integer?]
   [entries (listof (cons/c Source-Id? Available-Analysis?))])
  #:mutable)

(struct/contract Source-Controller
  ([store Source-Store?]
   [events async-channel?]
   [shutdown semaphore?]
   [notify (-> string? any/c any/c)]
   [reads Disk-Reads?]
   [runner Runner?]
   [retained Retained-Cache?])
  #:mutable)

;; Identity is the attempt object, not just its source revision. A retry of
;; the same revision cannot accept a terminal attempt's late completion.
;; `state` is 'queued, 'running or 'terminal. A resyntax job's `parent` is the
;; analysis job whose trace it refines.
(struct Job (snapshot kind parent completion state custodian) #:mutable)

(define (source-controller-shutdown-evt controller)
  (semaphore-peek-evt (Source-Controller-shutdown controller)))

(define (stopped? controller)
  (and (sync/timeout 0 (source-controller-shutdown-evt controller)) #t))

(define (source-buffer controller source)
  (source-store-buffer (Source-Controller-store controller) source))

(define (controller-lanes controller)
  (Disk-Reads-lanes (Source-Controller-reads controller)))

(define (controller-jobs controller)
  (Runner-jobs (Source-Controller-runner controller)))

(define (publish-diagnostics! controller uri diagnostics)
  ((Source-Controller-notify controller) "textDocument/publishDiagnostics"
                                         (hasheq 'uri uri 'diagnostics diagnostics)))

(define (source-controller-view controller source)
  (source-store-view (Source-Controller-store controller) source))

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

(define (job-source job)
  (Source-Snapshot-source (Job-snapshot job)))

(define (job-key job)
  (cons (source-id (job-source job)) (Job-kind job)))

(define (current-job? controller job)
  (eq? job (hash-ref (controller-jobs controller) (job-key job) #f)))

(define (job-current-source? controller job)
  (define view (source-controller-view controller (job-source job)))
  (and view (eq? (Source-Snapshot-revision (Job-snapshot job))
                 (Source-View-revision view))))

(define (forget-job! controller job)
  (when (current-job? controller job)
    (hash-remove! (controller-jobs controller) (job-key job))))
