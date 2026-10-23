#lang racket/base

;; Bounded LRU storage for completed analysis of closed files. The controller's
;; consumer serializes every operation, including lookups that refresh recency.
;; Callers establish file-only eligibility and detach live traces before insertion.
(require racket/contract
         "../../common/source.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt")

(provide Closed-Analysis-Cache?
         (contract-out
           [make-closed-analysis-cache (-> exact-nonnegative-integer? Closed-Analysis-Cache?)]
           [store-closed-analysis! (-> Closed-Analysis-Cache? Available-Analysis? void?)]
           [find-closed-analysis!
            (-> Closed-Analysis-Cache? Source-Snapshot? (or/c Available-Analysis? #f))]
           [take-closed-analysis! (-> Closed-Analysis-Cache? Source? (or/c Available-Analysis? #f))]
           [discard-closed-analysis! (-> Closed-Analysis-Cache? Source? void?)]
           [clear-closed-analysis! (-> Closed-Analysis-Cache? void?)]))

(struct/contract Cached-Analysis
  ([use-order exact-nonnegative-integer?]
   [analysis Available-Analysis?]))

;; The hash is the only collection of retained results. Each insertion or
;; successful match gets a unique use-order. Eviction scans this small bounded
;; table rather than maintaining a separate eviction queue.
(struct/contract Closed-Analysis-Cache
  ([limit exact-nonnegative-integer?]
   [(next-use-order #:mutable) exact-nonnegative-integer?]
   [entries (hash/c Source-Id? Cached-Analysis? #:immutable #f)]))

(define (make-closed-analysis-cache limit)
  (Closed-Analysis-Cache limit 0 (make-hash)))

(define (record-use! cache analysis)
  (define order (Closed-Analysis-Cache-next-use-order cache))
  (hash-set! (Closed-Analysis-Cache-entries cache)
             (Source-Snapshot-id (Available-Analysis-snapshot analysis))
             (Cached-Analysis order analysis))
  (set-Closed-Analysis-Cache-next-use-order! cache (add1 order)))

(define (store-closed-analysis! cache analysis)
  (unless (zero? (Closed-Analysis-Cache-limit cache))
    (record-use! cache analysis)
    (define entries (Closed-Analysis-Cache-entries cache))
    (when (> (hash-count entries) (Closed-Analysis-Cache-limit cache))
      (define-values (oldest-id _oldest-order)
        (for/fold ([oldest-id #f] [oldest-order +inf.0])
                  ([(id entry) (in-hash entries)])
          (define order (Cached-Analysis-use-order entry))
          (if (< order oldest-order)
              (values id order)
              (values oldest-id oldest-order))))
      (hash-remove! entries oldest-id))))

;; Close creates a new revision, so reuse compares exact text, not revisions.
;; Matching text does not establish freshness of imported dependencies. A miss
;; or a text mismatch leaves recency unchanged.
(define (find-closed-analysis! cache snapshot)
  (define entry (hash-ref (Closed-Analysis-Cache-entries cache) (Source-Snapshot-id snapshot) #f))
  (define analysis (and entry (Cached-Analysis-analysis entry)))
  (cond
    [(and analysis
          (equal? (Source-Snapshot-text snapshot)
                  (Source-Snapshot-text (Available-Analysis-snapshot analysis))))
     (record-use! cache analysis)
     analysis]
    [else #f]))

;; Remove before handing the mutable trace to a live buffer. Reopening takes
;; the entry even when its text will not match the new buffer.
(define (take-closed-analysis! cache source)
  (define entries (Closed-Analysis-Cache-entries cache))
  (define id (source-id source))
  (define entry (hash-ref entries id #f))
  (hash-remove! entries id)
  (and entry (Cached-Analysis-analysis entry)))

;; Discarding or evicting a result does not change source authority, live
;; documents, or completion events already held by waiters.
(define (discard-closed-analysis! cache source)
  (hash-remove! (Closed-Analysis-Cache-entries cache) (source-id source)))

(define (clear-closed-analysis! cache)
  (hash-clear! (Closed-Analysis-Cache-entries cache))
  (set-Closed-Analysis-Cache-next-use-order! cache 0))
