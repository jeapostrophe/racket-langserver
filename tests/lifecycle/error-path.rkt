#lang racket/base

(module+ test
  (require rackunit
           "../client.rkt"
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt")

  (test-case "server error path: malformed request returns useful message"
    (with-racket-lsp
      (lambda (lsp)
        (define malformed-msg
          (hasheq 'jsonrpc "2.0"
                  'id 100
                  'params (hasheq 'broken #t)))

        (client-send lsp malformed-msg)
        (define resp (client-wait-response malformed-msg))
        (define err (hash-ref resp 'error))

        (check-equal? (hash-ref resp 'id) 100)
        (check-equal? (hash-ref err 'code) (->jsexpr ErrorCode-InvalidRequest))
        (check-equal? (hash-ref err 'message)
                      "The JSON sent is not a valid request object.")))))
