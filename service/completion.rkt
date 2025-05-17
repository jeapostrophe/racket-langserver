#lang racket/base

(require "interface.rkt"
         "../autocomplete.rkt"
         racket/class)

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
      (define c (walk expanded-stx))
      (set! completions c))

    (define/public (get-online-completions str-before-cursor)
      (walk-online str-before-cursor))

    ))

