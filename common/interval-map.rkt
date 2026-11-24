#lang racket/base

;; Mutable interval-map abstraction backed by an immutable AVL interval rope.
;;
;; Provides an opaque mutable interval-map interface with logarithmic search and
;; edit performance, O(1) snapshot capture for multi-pass iteration and concurrency,
;; and compact flat-vector serialization for place and thread boundaries.
;;
;; Architecture & Invariants:
;; - Encapsulation: interval-map wraps an immutable IntervalRope. Mutations
;;   (set!, remove!, expand!, contract!, update*!) rebind the container's
;;   internal rope reference with the result of a pure rope transformation.
;; - Safe Concurrency & Stable Iteration: Point and range queries read the current
;;   rope reference without locking. Iteration via in-interval-map snapshots the
;;   rope at call time, allowing multi-pass traversal unaffected by subsequent
;;   mutations to the container.
;; - Value Identity Preservation: Snapshots deduplicate values using pointer
;;   identity (hasheq) so adjacent intervals sharing identical values preserve
;;   coalescing behavior across serialization and deserialization.

(require (prefix-in rope: "interval-rope.rkt")
         racket/match)

(provide make-interval-map
         interval-map?
         interval-map-count
         interval-map-empty?
         ;; Point and range queries
         interval-map-ref
         interval-map-ref/bounds
         interval-map-overlap-values
         ;; In-place mutations
         interval-map-set!
         interval-map-remove!
         interval-map-expand!
         interval-map-contract!
         interval-map-update*!
         ;; Conversion and iteration
         interval-map->list
         in-interval-map
         ;; Snapshot serialization
         interval-map->snapshot
         snapshot->interval-map)

(struct interval-map ([rope #:mutable]))

(define (check-interval-map! who v)
  (unless (interval-map? v)
    (raise-argument-error who "interval-map?" v)))

;; -----------------------------------------------------------------------------
;; Constructors & Predicates
;; -----------------------------------------------------------------------------

;; (-> interval-map?)
(define (make-interval-map)
  (interval-map rope:empty-interval-rope))

;; (-> interval-map? exact-nonnegative-integer?)
(define (interval-map-count im)
  (check-interval-map! 'interval-map-count im)
  (rope:interval-rope-count (interval-map-rope im)))

;; (-> interval-map? boolean?)
(define (interval-map-empty? im)
  (check-interval-map! 'interval-map-empty? im)
  (rope:interval-rope-empty? (interval-map-rope im)))

;; -----------------------------------------------------------------------------
;; Point and Range Queries
;; -----------------------------------------------------------------------------

;; (->* (interval-map? exact-integer?) (any/c) any/c)
(define interval-map-ref
  (case-lambda
    [(im pos)
     (check-interval-map! 'interval-map-ref im)
     (rope:interval-rope-ref (interval-map-rope im) pos)]
    [(im pos default)
     (check-interval-map! 'interval-map-ref im)
     (rope:interval-rope-ref (interval-map-rope im) pos default)]))

;; (->* (interval-map? exact-integer?) (any/c) (values exact-integer? exact-integer? any/c))
(define interval-map-ref/bounds
  (case-lambda
    [(im pos)
     (check-interval-map! 'interval-map-ref/bounds im)
     (rope:interval-rope-ref/bounds (interval-map-rope im) pos)]
    [(im pos default)
     (check-interval-map! 'interval-map-ref/bounds im)
     (rope:interval-rope-ref/bounds (interval-map-rope im) pos default)]))

;; (-> interval-map? exact-integer? exact-integer? (listof any/c))
(define (interval-map-overlap-values im start end)
  (check-interval-map! 'interval-map-overlap-values im)
  (rope:interval-rope-overlap-values (interval-map-rope im) start end))

;; -----------------------------------------------------------------------------
;; In-Place Mutations
;; -----------------------------------------------------------------------------

;; (-> interval-map? exact-integer? exact-integer? any/c void?)
(define (interval-map-set! im start end val)
  (check-interval-map! 'interval-map-set! im)
  (set-interval-map-rope! im (rope:interval-rope-set (interval-map-rope im) start end val)))

;; (-> interval-map? exact-integer? exact-integer? void?)
(define (interval-map-remove! im start end)
  (check-interval-map! 'interval-map-remove! im)
  (set-interval-map-rope! im (rope:interval-rope-remove (interval-map-rope im) start end)))

;; (->* (interval-map? exact-integer? exact-integer?) (#:interior (or/c 'split 'extend)) void?)
(define (interval-map-expand! im start end #:interior [interior 'split])
  (check-interval-map! 'interval-map-expand! im)
  (set-interval-map-rope! im (rope:interval-rope-expand (interval-map-rope im) start end #:interior interior)))

;; (-> interval-map? exact-integer? exact-integer? void?)
(define (interval-map-contract! im start end)
  (check-interval-map! 'interval-map-contract! im)
  (set-interval-map-rope! im (rope:interval-rope-contract (interval-map-rope im) start end)))

;; (->* (interval-map? exact-integer? exact-integer? (-> any/c any/c)) (any/c) void?)
(define (interval-map-update*! im start end updater [default null])
  (check-interval-map! 'interval-map-update*! im)
  (set-interval-map-rope! im (rope:interval-rope-update (interval-map-rope im) start end updater default)))

;; -----------------------------------------------------------------------------
;; Conversion & Iteration
;; -----------------------------------------------------------------------------

;; (-> interval-map? (listof (cons/c (cons/c exact-integer? exact-integer?) any/c)))
(define (interval-map->list im)
  (check-interval-map! 'interval-map->list im)
  (rope:interval-rope->list (interval-map-rope im)))

;; (-> interval-map? (sequence/c exact-integer? exact-integer? any/c))
(define (in-interval-map im)
  (check-interval-map! 'in-interval-map im)
  (rope:in-interval-rope (interval-map-rope im)))

;; -----------------------------------------------------------------------------
;; Snapshot Serialization & Deserialization
;; -----------------------------------------------------------------------------

;; Serializes the interval-map into a flat [start, end, id] entries vector and
;; an array of deduplicated encoded values.
;;
;; Value identity rationale:
;; Deduplication uses hasheq to preserve value identity (eq?) across serialization.
;; This ensures that adjacent intervals sharing the same value identity continue
;; to coalesce on subsequent edits after deserialization.
;;
;; (->* (interval-map?) ((-> any/c any/c)) (list/c (vectorof exact-integer?) (vectorof any/c)))
(define (interval-map->snapshot im [encode values])
  (check-interval-map! 'interval-map->snapshot im)
  (unless (procedure? encode)
    (raise-argument-error 'interval-map->snapshot "procedure?" encode))

  (define r (interval-map-rope im))
  (define count (rope:interval-rope-count r))
  (define entries (make-vector (* 3 count)))
  (define ids (make-hasheq))
  (define encoded '())
  (define offset 0)

  (rope:interval-rope-for-each
    r
    (lambda (start end value)
      (define id
        (hash-ref! ids value
                   (lambda ()
                     (define new-id (hash-count ids))
                     (set! encoded (cons (encode value) encoded))
                     new-id)))
      (vector-set! entries offset start)
      (vector-set! entries (+ offset 1) end)
      (vector-set! entries (+ offset 2) id)
      (set! offset (+ offset 3))))

  (list entries (list->vector (reverse encoded))))

;; Reconstructs an interval-map from a snapshot pair (list entries encoded-values).
;;
;; (->* ((list/c (vectorof exact-integer?) (vectorof any/c))) ((-> any/c any/c)) interval-map?)
(define (snapshot->interval-map snapshot [decode values])
  (unless (procedure? decode)
    (raise-argument-error 'snapshot->interval-map "procedure?" decode))

  (match snapshot
    [(list (? vector? entries) (? vector? encoded))
     (define decoded
       (if (eq? decode values)
           encoded
           (for/vector ([value (in-vector encoded)])
             (decode value))))
     (interval-map (rope:adopt-interval-snapshot entries decoded))]
    [_
     (raise-argument-error 'snapshot->interval-map "(list/c vector? vector?)" snapshot)]))
