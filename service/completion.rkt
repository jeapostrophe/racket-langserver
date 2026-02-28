#lang racket/base

(require "interface.rkt"
         "../autocomplete.rkt"
         "../internal-types.rkt"
         racket/class)

(provide completion%)

(define completion%
  (class base-service%
    (super-new)
    (define completions (list))

    (define/override (get)
      completions)

    (define/override (reset)
      ;; Some LSP clients send full-text text changes (rather than incremental deltas)
      ;; even during incremental typing.
      ;; Theses full-text events replace the entire document, and trigger this method.
      ;; In such cases, preserve existing completions instead of clearing them.
      (void))

    (define/override (walk-stx expand-result)
      (define expanded-stx (ExpandResult-post-stx expand-result))
      (when (syntax? expanded-stx)
        (set! completions (walk expanded-stx))))

    (define/public (get-online-completions str-before-cursor)
      (walk-online str-before-cursor))

    ))

