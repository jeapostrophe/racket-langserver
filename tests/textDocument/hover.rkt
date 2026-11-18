#lang racket/base

(module+ test
  (require rackunit
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../json-dsl.rkt"
           "with-document.rkt")

  (define uri "file:///test.rkt")
  (define code "#lang racket/base\n\n(list 1 2)\n")

  (test-case "textDocument/hover returns contents and target range"
    (with-document uri code
      (lambda (lsp)
        (define hover
          (jsexpr->Hover
            (lsp-result! lsp "textDocument/hover" uri (json {#:position (->jsexpr (Pos 2 1))}))))
        (check-false (string=? "" (Hover-contents hover)))
        (check-equal? (Hover-range hover) (Range (Pos 2 1) (Pos 2 5)))))))
