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

    (define/override (syncheck:add-mouse-over-status _src start end text)
      ;; When start = end, it means the identifier is not found in the source file,
      ;; but exists in expanded syntax. So we shouldn't add an item for it.
      (when (< start end)
        (interval-map-set! hovers start end text)))))

