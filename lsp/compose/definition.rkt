#lang racket/base

;; textDocument/definition. A local location, or no binding, is returned
;; immediately. A module binding is resolved by `target-query`.
(require racket/contract/base
         "../../common/interfaces.rkt"
         "../../common/time.rkt"
         "../../doclib/doc.rkt"
         "target.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt"
         "../responses.rkt"
         "../safedoc.rkt"
         "../source-controller.rkt")

;; Seconds for the entire target lookup, including deferred dispatch and queues.
(define current-definition-target-timeout (make-parameter 10.0))

(provide definition-query
         (contract-out
           [current-definition-target-timeout (parameter/c (and/c real? (>=/c 0)))]))

(define (definition-query safe-doc id uri pos)
  (define deadline (deadline-after (current-definition-target-timeout)))
  (define controller (current-source-controller))
  (define-values (view target)
    (with-read-safedoc safe-doc
      (lambda (sd)
        (values (SafeDoc-source-view sd) (doc-definition (SafeDoc-doc sd) uri pos)))))
  (cond
    [(Module-Binding? target)
     (target-query id controller deadline view target "Definition"
                   (lambda (facts binding)
                     (hash-ref (Analysis-Summary-definition-ranges facts) binding #f))
                   (lambda (snapshot range)
                     (and range (Location (Source-Snapshot-uri snapshot) range))))]
    [else (success/enc id target)]))
