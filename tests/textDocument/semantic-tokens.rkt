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
           racket/list
           racket/async-channel
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "with-document.rkt")

  (define comment-token-length (string-length "#; (define ignored 1)"))
  (define comment-token-type (index-of *semantic-token-types* SemanticTokenType-comment))

  (define (semantic-token-chunks encoded-tokens)
    (cond
      [(empty? encoded-tokens) '()]
      [else
       (cons (take encoded-tokens 5)
             (semantic-token-chunks (drop encoded-tokens 5)))]))

  (define (comment-token-chunk? chunk)
    (match chunk
      [(list _delta-line _delta-start length type _modifiers)
       (and (= length comment-token-length)
            (= type comment-token-type))]
      [_ #f]))

  (test-case
    "semantic tokens return after failed check-syntax completion"
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
        (define response response-ready)
        (define chunks
          (semantic-token-chunks (jsexpr-ref response '(result data))))
        (check-true
          (ormap comment-token-chunk? chunks)
          "current lexer-derived #; comment token should be returned")))))
