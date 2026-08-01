#lang racket/base

;; Typed Racket analysis from the online Check Syntax log stream.
;;
;; Generic log decoding and message classification live in `tooltip-log.rkt`.
;; This service owns only the two Typed Racket products: inferred types and
;; type-error diagnostics. Keep them together so diag% stays
;; language-independent (see get-warn-diags in doc-trace.rkt).

(require "../interface.rkt"
         "../tooltip-log.rkt"
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

    (define/public (consume-inferred-tooltips tooltips)
      (for ([tooltip (in-list tooltips)])
        (record-inferred-type! tooltip)))

    ;; One log record may describe a compound error with equal-message endpoint
    ;; tooltips. Keep the record boundary so those endpoints become one range.
    (define/public (consume-type-error-tooltips tooltips)
      (match tooltips
        ['() (void)]
        [(cons first-tooltip rest-tooltips)
         (define shared-message?
           (for/and ([tooltip (in-list rest-tooltips)])
             (and (equal? (Tooltip-source tooltip)
                          (Tooltip-source first-tooltip))
                  (string=? (Tooltip-text tooltip)
                            (Tooltip-text first-tooltip)))))
         (cond
           [shared-message?
            (record-type-error!
              (apply min (map Tooltip-start tooltips))
              (apply max (map Tooltip-end tooltips))
              (Tooltip-text first-tooltip))]
           [else
            (for ([tooltip (in-list tooltips)])
              (record-type-error! (Tooltip-start tooltip)
                                  (Tooltip-end tooltip)
                                  (Tooltip-text tooltip)))])]))

    ;; Provider offsets must fit the document snapshot. Invalid LSP ranges are
    ;; worse than omitting one malformed tooltip.
    (define/private (document-range? start end)
      (and (< start end)
           (<= end (send doc-text end-pos))))

    (define/private (record-inferred-type! tooltip)
      (match-define (struct* Tooltip
                      ([start start]
                       [end end]
                       [text text]))
        tooltip)
      (when (document-range? start end)
        (interval-map-set! type-by-range start end text)))

    (define/private (record-type-error! start end message)
      (when (document-range? start end)
        (set-add!
          diagnostics
          (Diagnostic #:range
                      (Range #:start (abs-pos->Pos doc-text start)
                             #:end (abs-pos->Pos doc-text end))
                      #:severity DiagnosticSeverity-Error
                      #:source "Typed Racket"
                      #:message message))))))
