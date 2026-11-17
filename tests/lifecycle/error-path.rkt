#lang racket/base

(module+ test
  (require "../silent-logger.rkt"
           rackunit
           "../client.rkt"
           "../json-dsl.rkt"
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt")

  (test-case "server error path: malformed request returns InvalidRequest"
    (with-racket-lsp
      (lambda (lsp)
        (define malformed-msg (json {#:jsonrpc "2.0" #:id 100 #:params {#:broken #t}}))
        (client-send lsp malformed-msg)
        (define resp (client-wait-response lsp malformed-msg))
        (define err (hash-ref resp 'error))
        (check-equal? (hash-ref resp 'id) 100)
        (check-equal? (hash-ref err 'code) (->jsexpr ErrorCode-InvalidRequest))
        (check-equal? (hash-ref err 'message)
                      "The JSON sent is not a valid request object.")))))
