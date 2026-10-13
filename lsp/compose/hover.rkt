#lang racket/base

(require "../../common/interfaces.rkt"
         "../../common/time.rkt"
         "../../doclib/doc.rkt"
         "../../doclib/hover.rkt"
         "../../doclib/internal-types.rkt"
         "../analysis.rkt"
         "../analysis-query.rkt"
         "../responses.rkt"
         "../safedoc.rkt"
         "../source-controller.rkt"
         "target.rkt")

(provide hover-query)

(define (hover-query safe-doc id pos)
  (define deadline (+ (current-deadline-milliseconds)
                      (* 1000 (current-analysis-request-timeout))))
  (define controller (current-source-controller))
  (define-values (view target card range)
    (with-read-safedoc safe-doc
      (lambda (sd)
        (define doc (SafeDoc-doc sd))
        (define-values (card range) (doc-hover-data doc pos))
        (values (SafeDoc-source-view sd)
                (and (not (Hover-Card-documentation card))
                     (doc-definition doc (Doc-uri doc) pos))
                card range))))
  (define (compose summary)
    (define joined
      (if summary
          (struct-copy Hover-Card card [definition (Hover-Definition 'source summary)])
          card))
    (and (hover-card-opens? joined)
         (Hover #:contents (render-hover-card joined) #:range range)))
  (if (and (Module-Binding? target) (shared-binding? target))
      (target-query id controller deadline view target "Hover"
                    (lambda (facts binding)
                      (hash-ref (Analysis-Facts-hover-summaries facts) binding #f))
                    #f (lambda (_snapshot summary) summary) compose)
      (success/enc id (compose #f))))
