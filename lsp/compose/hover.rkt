#lang racket/base

;; textDocument/hover. The card comes from the requesting document. A module
;; binding may add a source summary from the target file. A hover is returned
;; only when that card opens.
(require racket/contract/base
         "../../common/interfaces.rkt"
         "../../common/time.rkt"
         "../../doclib/doc.rkt"
         "../../doclib/hover.rkt"
         "../../doclib/internal-types.rkt"
         "../analysis.rkt"
         "../responses.rkt"
         "../safedoc.rkt"
         "../source-controller.rkt"
         "target.rkt")

;; Seconds for the entire target lookup, including deferred dispatch and queues.
(define current-hover-target-timeout (make-parameter 0.2))

(provide hover-query
         (contract-out
           [current-hover-target-timeout (parameter/c (and/c real? (>=/c 0)))]))

(define (hover-query safe-doc id pos)
  (define deadline (deadline-after (current-hover-target-timeout)))
  (define controller (current-source-controller))
  (define-values (view target card range)
    (with-read-safedoc safe-doc
      (lambda (sd)
        (define doc (SafeDoc-doc sd))
        (define-values (card range) (doc-hover-data doc pos))
        (values (SafeDoc-source-view sd)
                ;; Documentation excludes a source snippet, so do not resolve
                ;; another file when the card already has documentation.
                (and (not (Hover-Card-documentation card))
                     (doc-definition doc (Doc-uri doc) pos))
                card range))))
  ;; Use definition kind `'source`. `hover-card-opens?` does not open a card
  ;; whose only definition is a signature.
  (define (compose _snapshot summary)
    (define joined
      (if summary
          (struct-copy Hover-Card card [definition (Hover-Definition 'source summary)])
          card))
    (and (hover-card-opens? joined)
         (Hover #:contents (render-hover-card joined) #:range range)))
  (if (Module-Binding? target)
      (target-query id controller deadline view target "Hover"
                    (lambda (facts binding)
                      (hash-ref (Analysis-Summary-hover-summaries facts) binding #f))
                    compose #:fallback (lambda () (compose #f #f)))
      (success/enc id (compose #f #f))))
