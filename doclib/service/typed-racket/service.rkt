#lang racket/base

;; Typed Racket analysis from the online Check Syntax log stream.
;;
;; This service owns both products of that channel: inferred types and
;; type-error diagnostics. Keep them here so diag% stays language-independent
;; (see get-warn-diags in doc-trace.rkt).

(require "../interface.rkt"
         "decoder.rkt"
         "../../../common/interfaces.rkt"
         racket/class
         racket/match
         racket/set
         data/interval-map)

(provide typed-racket%)

(define typed-racket%
  (class base-service%
    (init-field src
                doc-text)
    (super-new)

    ;; Absolute character range -> inferred type text.
    (define type-by-range
      (make-interval-map))
    (define diagnostics
      (mutable-seteq))

    ;; Return start/end/text only. Callers must not hold or mutate the
    ;; interval-map while expand/contract shifts ranges during an edit.
    (define/public (inferred-type-at pos)
      (interval-map-ref/bounds type-by-range pos #f))

    (define/public (get-diagnostics)
      diagnostics)

    (define/override (reset)
      (set! type-by-range (make-interval-map))
      (set-clear! diagnostics))

    (define/override (expand start end)
      (interval-map-expand! type-by-range start end))

    (define/override (contract start end)
      (interval-map-contract! type-by-range start end))

    (define/override (walk-log log)
      (for ([record (in-list (typed-racket-log-annotations log src))])
        (match record
          [(? Inferred-Type?)
           (record-inferred-type! record)]
          [(? Type-Error?)
           (record-type-error! record)])))

    ;; Provider offsets must fit the document snapshot. Invalid LSP ranges are
    ;; worse than omitting one malformed tooltip.
    (define/private (document-range? start end)
      (and (< start end)
           (<= end (send doc-text end-pos))))

    (define/private (record-inferred-type! inferred-type)
      (match-define (struct* Inferred-Type
                      ([start start]
                       [end end]
                       [text text]))
        inferred-type)
      (when (document-range? start end)
        (interval-map-set! type-by-range start end text)))

    (define/private (record-type-error! type-error)
      (match-define (struct* Type-Error
                      ([start start]
                       [end end]
                       [message message]))
        type-error)
      (when (document-range? start end)
        (set-add!
          diagnostics
          (Diagnostic #:range
                      (Range #:start (abs-pos->Pos doc-text start)
                             #:end (abs-pos->Pos doc-text end))
                      #:severity DiagnosticSeverity-Error
                      #:source "Typed Racket"
                      #:message message))))))
