#lang racket/base

;; textDocument/definition. A local location, or no binding, is returned
;; immediately. A shared module binding is resolved by `target-query`. Any
;; other module binding has no location.
(require "../../common/interfaces.rkt"
         "../../common/time.rkt"
         "../../doclib/doc.rkt"
         "target.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt"
         "../analysis-query.rkt"
         "../responses.rkt"
         "../safedoc.rkt"
         "../source-controller.rkt")

(provide definition-query)

(define (definition-query safe-doc id uri pos)
  (define deadline (+ (current-deadline-milliseconds)
                      (* 1000 (current-analysis-request-timeout))))
  (define controller (current-source-controller))
  (define-values (view target)
    (with-read-safedoc safe-doc
      (lambda (sd)
        (values (SafeDoc-source-view sd) (doc-definition (SafeDoc-doc sd) uri pos)))))
  (cond
    [(and (Module-Binding? target) (shared-binding? target))
     (target-query id controller deadline view target "Definition"
                   (lambda (facts binding)
                     (hash-ref (Analysis-Summary-definition-ranges facts) binding #f))
                   (lambda (snapshot range)
                     (and range (Location (Source-Snapshot-uri snapshot) range))))]
    [(Module-Binding? target) (success/enc id #f)]
    [else (success/enc id target)]))
