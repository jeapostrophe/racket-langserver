#lang racket/base

(require "interface.rkt"
         "../autocomplete.rkt"
         racket/class
         racket/set)

(provide completion%)

(define completion%
  (class base-service%
    (super-new)
    (define completions (list))

    (define/override (get)
      completions)

    (define/override (reset)
      (set! completions (list)))

    (define/override (walk-stx stx expanded-stx)
      (define c (append (set->list (walk expanded-stx))
                        (set->list (walk-module expanded-stx))))
      (set! completions c))

    ))

