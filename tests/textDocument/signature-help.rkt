#lang racket/base

(module+ test
  (require rackunit
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../json-dsl.rkt"
           "with-document.rkt")

  (define uri "file:///test.rkt")
  (define code "#lang racket/base\n\n(list )\n")

  (test-case "textDocument/signatureHelp returns non-empty signatures inside call"
    (with-document uri code
      (lambda (lsp)
        (define result
          (lsp-result! lsp
                       "textDocument/signatureHelp"
                       uri
                       (json {#:position (->jsexpr (Pos 2 6))})))
        (check-true (hash-has-key? result 'signatures))
        (define signatures (hash-ref result 'signatures))
        (check-pred list? signatures)
        (check-false (null? signatures))))))
