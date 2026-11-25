#lang racket/base

;; Mutable interval-map abstraction backed by an immutable AVL interval rope.
;;
;; Provides an opaque mutable interval-map interface with logarithmic search and
;; edit performance and O(1) snapshot capture for multi-pass iteration and
;; read concurrency.
;;
;; Architecture & Invariants:
;; - Encapsulation: interval-map wraps an immutable IntervalRope. Mutations
;;   (set!, remove!, expand!, contract!, update*!) rebind the container's
;;   internal rope reference with the result of a pure rope transformation.
;; - Safe Concurrency & Stable Iteration: Point and range queries read the current
;;   rope reference without locking. Iteration via in-interval-map snapshots the
;;   rope at call time, allowing multi-pass traversal unaffected by subsequent
;;   mutations to the container. Mutations are single-writer only (no internal
;;   locking around set-interval-map-rope!), matching the single-owner Doc model.

(require (prefix-in rope: "interval-rope.rkt"))

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
         in-interval-map)

(struct interval-map ([rope #:mutable]) #:authentic)

(define (check-interval-map! who v)
  (unless (interval-map? v)
    (raise-argument-error who "interval-map?" v)))

;; -----------------------------------------------------------------------------
;; Constructors & Predicates
;; -----------------------------------------------------------------------------

;; (->* () ((listof (cons/c (cons/c exact-integer? exact-integer?) any/c))) interval-map?)
(define (make-interval-map [entries '()])
  (interval-map (rope:interval-rope entries)))

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

;; (->* (interval-map? exact-integer?) (any/c) (values (or/c exact-integer? #f) (or/c exact-integer? #f) any/c))
(define interval-map-ref/bounds
  (case-lambda
    [(im pos)
     (check-interval-map! 'interval-map-ref/bounds im)
     (rope:interval-rope-ref/bounds (interval-map-rope im) pos)]
    [(im pos default)
     (check-interval-map! 'interval-map-ref/bounds im)
     (rope:interval-rope-ref/bounds (interval-map-rope im) pos default)]))

;; Returns all mapped values overlapping [start, end) in order.
;; When start = end = pos (cursor-only query), selects strictly containing
;; intervals (interval-start < pos < interval-end), excluding boundary starts.
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
(define interval-map-update*!
  (case-lambda
    [(im start end updater)
     (check-interval-map! 'interval-map-update*! im)
     (set-interval-map-rope! im (rope:interval-rope-update (interval-map-rope im) start end updater))]
    [(im start end updater default)
     (check-interval-map! 'interval-map-update*! im)
     (set-interval-map-rope! im (rope:interval-rope-update (interval-map-rope im) start end updater default))]))

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
