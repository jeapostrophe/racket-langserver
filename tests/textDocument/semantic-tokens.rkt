#lang racket

(define uri "file:///semantic-tokens-test.rkt")

(define failing-code
#<<END
#lang racket/base
#; (define ignored 1)
(define x
END
  )

(module+ test
  (require rackunit
           racket/async-channel
           "../../common/json-util.rkt"
           "with-document.rkt")

  (test-case
    "semantic tokens return empty data for unsuccessful current analysis"
    (with-document uri failing-code
      (lambda (lsp)
        (define semantic-tokens-req
          (make-request lsp
                        "textDocument/semanticTokens/full"
                        (hasheq 'textDocument
                                (hasheq 'uri uri))))
        (client-send lsp semantic-tokens-req)

        (define response-channel (make-async-channel))
        (thread
          (lambda ()
            (async-channel-put response-channel
                               (client-wait-response semantic-tokens-req))))
        (define response-ready (sync/timeout 1.0 response-channel))

        (check-not-false response-ready)
        (check-equal? (hash-ref response-ready 'result) (hasheq 'data '()))

        (define range-params
          (hasheq 'textDocument (hasheq 'uri uri)
                  'range (hasheq 'start (hasheq 'line 0 'character 0)
                                 'end (hasheq 'line 3 'character 0))))
        (define range-req
          (make-request lsp "textDocument/semanticTokens/range" range-params))
        (client-send lsp range-req)
        (check-equal? (hash-ref (client-wait-response range-req) 'result) (hasheq 'data '()))

        (client-send lsp
                     (make-notification
                       "textDocument/didChange"
                       (hasheq 'textDocument (hasheq 'uri uri 'version 1)
                               'contentChanges
                               (list (hasheq 'text "#lang racket/base\n(define x 1)\nx\n")))))
        (client-wait-notification lsp)
        (for ([method (in-list '("textDocument/semanticTokens/full"
                                 "textDocument/semanticTokens/range"))])
          (define req (make-request lsp method range-params))
          (client-send lsp req)
          (check-true (pair? (jsexpr-ref (client-wait-response req) '(result data)))))))))
