#lang racket/base

;; A small lazy cache for cheap derived data. The caller is responsible for
;; ensuring the source state is stable while computing a cache miss.
;; Recursive self-fills are a known deadlock mode on the cache semaphore; we
;; intentionally leave that as caller discipline instead of detecting it here.

(require racket/contract)

(struct lazy-cache
  (lock [value #:mutable])
  #:transparent)

(define lazy-cache-empty
  (gensym 'lazy-cache-empty))

(define (lazy-cache-of value/c)
  (define value-contract
    (coerce-contract 'lazy-cache-of value/c))
  (rename-contract
    (struct/c lazy-cache
      semaphore?
      (or/c (one-of/c lazy-cache-empty) value-contract))
    `(lazy-cache-of ,(contract-name value-contract))))

(define/contract (make-lazy-cache)
  (-> lazy-cache?)
  (lazy-cache (make-semaphore 1) lazy-cache-empty))

(define/contract (lazy-cache-invalidate! cache)
  (-> lazy-cache? void?)
  (call-with-semaphore
    (lazy-cache-lock cache)
    (lambda ()
      (set-lazy-cache-value! cache lazy-cache-empty))))

(define/contract (call-with-lazy-cache! cache compute)
  (-> lazy-cache? (-> any/c) any/c)
  (call-with-semaphore
    (lazy-cache-lock cache)
    (lambda ()
      (define cached-value (lazy-cache-value cache))
      (cond
        [(eq? cached-value lazy-cache-empty)
         (define computed-value (compute))
         (set-lazy-cache-value! cache computed-value)
         computed-value]
        [else cached-value]))))

(provide lazy-cache?
         lazy-cache-of
         make-lazy-cache
         lazy-cache-invalidate!
         call-with-lazy-cache!)
