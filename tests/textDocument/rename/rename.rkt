#lang racket/base

(module+ test
  (require rackunit
           "../../../common/interfaces.rkt"
           "../../../common/json-util.rkt"
           "../../json-dsl.rkt"
           "../with-document.rkt")

  (define uri "file:///test.rkt")
  (define code
#<<END
#lang racket/base

(define x 0)

(let ([x 1])
  x)
END
    )

  (test-case "rename and prepareRename update only the bound lexical variable"
    (with-document uri code
      (lambda (lsp)
        (define rename-result
          (lsp-result! lsp
                       "textDocument/rename"
                       uri
                       (json {#:position (->jsexpr (Pos 5 2))
                              #:newName "new-x"})))
        (check-equal?
          (jsexpr->WorkspaceEdit rename-result)
          (WorkspaceEdit
            (hasheq (string->symbol uri)
                    (list (TextEdit (Range (Pos 4 7) (Pos 4 8)) "new-x")
                          (TextEdit (Range (Pos 5 2) (Pos 5 3)) "new-x")))))

        (define prepare-result
          (lsp-result! lsp
                       "textDocument/prepareRename"
                       uri
                       (json {#:position (->jsexpr (Pos 5 2))})))
        (check-equal? (jsexpr->Range prepare-result)
                      (Range (Pos 5 2) (Pos 5 3)))))))
