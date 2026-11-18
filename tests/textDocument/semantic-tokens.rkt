#lang racket/base

(module+ test
  (require rackunit
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../json-dsl.rkt"
           "with-document.rkt")

  (define uri "file:///semantic-tokens-test.rkt")
  (define failing-code "#lang racket/base\n#; (define ignored 1)\n(define x\n")

  (test-case "semantic tokens retain current lexer comments through unsuccessful analysis"
    (with-document uri failing-code
      (lambda (lsp)
        (check-equal?
          (lsp-result! lsp "textDocument/semanticTokens/full" uri (hasheq) 1.0)
          (json {#:data '(1 0 21 5 0)}))

        (define range-params
          (json {#:range (->jsexpr (Range (Pos 0 0) (Pos 3 0)))}))
        (check-equal?
          (lsp-result! lsp "textDocument/semanticTokens/range" uri range-params)
          (json {#:data '(1 0 21 5 0)}))

        (change! lsp uri "#lang racket/base\n(define x 1)\nx\n" 1)
        (client-wait-notification lsp)

        (for ([method (in-list '("textDocument/semanticTokens/full"
                                 "textDocument/semanticTokens/range"))])
          (check-true (pair? (hash-ref (lsp-result! lsp method uri range-params) 'data))))))))
