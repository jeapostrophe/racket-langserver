#lang racket

(require "../with-document.rkt")

(define uri "file:///test.rkt")

(define code
#<<END
#lang racket/base

(define x 0)
END
  )

(module+ test
  (require rackunit
           json)

  (with-document uri code
    (λ (lsp)
      (let ([req (read-json (open-input-file "req1.json"))]
            [resp (read-json (open-input-file "resp1.json"))])
        (client-send lsp req)

        (check-equal? (jsexpr->string (client-wait-response req))
                      (jsexpr->string resp))))))

