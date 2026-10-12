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
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "with-document.rkt")

  (test-case
    "semantic tokens report unsuccessful current analysis"
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
        (check-equal? (jsexpr-ref response-ready '(error code))
                      (->jsexpr ErrorCode-RequestFailed))))))
