#lang racket/base

(require "interface.rkt"
         racket/class
         data/interval-map
         drracket/check-syntax)

(provide require%)

(define require%
  (class base-service%
    (super-new)
    (define requires (make-interval-map))

    (define/override (get)
      requires)

    (define/override (reset)
      (set! requires (make-interval-map)))

    (define/override (expand start end)
      (interval-map-expand! requires start end))

    (define/override (contract start end)
      (interval-map-contract! requires start end))

    (define/override (syncheck:add-require-open-menu _text start finish file)
      (interval-map-set! requires start finish file))))

