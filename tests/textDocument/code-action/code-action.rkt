#lang racket/base

(module+ test
  (require rackunit
           "../../../common/interfaces.rkt"
           "../../../common/json-util.rkt"
           "../../json-dsl.rkt"
           "../with-document.rkt")

  (define uri "file:///test.rkt")
  (define code "#lang racket/base\n\n(define x 0)\n")

  (test-case "textDocument/codeAction suggests prefixing unused variable with _"
    (with-document uri code
      (lambda (lsp)
        (define target-range (Range (Pos 2 8) (Pos 2 9)))
        (define result
          (lsp-result! lsp
                       "textDocument/codeAction"
                       uri
                       (json {#:range (->jsexpr target-range)
                              #:context {#:diagnostics []}})))
        (check-equal?
          (map jsexpr->CodeAction result)
          (list
            (CodeAction
              "Add prefix `_` to ignore"
              "quickfix"
              (list (Diagnostic target-range DiagnosticSeverity-Information uri "unused variable"))
              #f
              (WorkspaceEdit
                (hasheq (string->symbol uri)
                        (list (TextEdit (Range (Pos 2 8) (Pos 2 8)) "_")))))))))))
