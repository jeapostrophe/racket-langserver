#lang racket/base

(require racket/class
         drracket/check-syntax)

(provide service<%>
         base-service%)

(define service<%>
  (interface (syncheck-annotations<%>)
    get
    expand
    contract
    reset
    walk))

(define base-service%
  (class (annotations-mixin object%)
    (super-new)

    (define/public (get)
      #f)

    (define/public (expand start end)
      (void))

    (define/public (contract start end)
      (void))

    (define/public (reset)
      (void))

    (define/public (walk-stx stx expanded-stx)
      (void))))

