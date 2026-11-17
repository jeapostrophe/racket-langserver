#lang racket/base

(module+ test
  (require rackunit
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../json-dsl.rkt"
           "with-document.rkt")

  (define uri "file:///test.rkt")
  (define code "#lang racket/base\n\n(define (bob)\n  (+ 1 2))\n")

  (test-case "textDocument/onTypeFormatting indents a newly inserted newline"
    (with-document uri code
      (lambda (lsp)
        (notify lsp "textDocument/didChange"
                (json {#:textDocument (->jsexpr (DocIdentifier 1 uri))
                       #:contentChanges [(->jsexpr
                                           (ContentChangeEvent-Incremental
                                             (Range (Pos 2 13) (Pos 2 13))
                                             0
                                             "\n"))]}))
        (client-wait-notification lsp)

        (define result
          (lsp-result! lsp
                       "textDocument/onTypeFormatting"
                       uri
                       (json {#:position (->jsexpr (Pos 3 0))
                              #:ch "\n"
                              #:options {#:tabSize 4 #:insertSpaces #t}})))
        (check-equal? (map jsexpr->TextEdit result)
                      (list (TextEdit (Range (Pos 3 0) (Pos 3 0)) "  ")))))))
