#lang racket/base

(require "interface.rkt"
         racket/class
         data/interval-map
         drracket/check-syntax)

(provide hover%)

(define hover%
  (class base-service%
    (super-new)
    (define hovers (make-interval-map))

    (define/override (get)
      hovers)

    (define/override (reset)
      (set! hovers (make-interval-map)))

    (define/override (expand start end)
      (interval-map-expand! hovers start end))

    (define/override (contract start end)
      (interval-map-contract! hovers start end))

    (define/override (syncheck:add-mouse-over-status src-obj start finish text)
      ;; Infer a length of 1 for zero-length ranges in the document.
      ;; XXX This might not exactly match the behavior in DrRacket.
      (when (= start finish)
        (set! finish (add1 finish)))
      (interval-map-set! hovers start finish text))))

