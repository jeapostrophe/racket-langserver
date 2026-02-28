#lang racket

(define uri "file:///test.rkt")

(define code
#<<END
#lang racket/base

(define x 0)

(let ([x 1])
  x)
END
  )

(module+ test
  (require rackunit
           json
           "../with-document.rkt")

  (with-document uri code
    (λ (lsp)
      (let ([req (read-json (open-input-file "req1.json"))]
            [resp (read-json (open-input-file "resp1.json"))])
        (client-send lsp req)
        (check-equal? (jsexpr->string (client-wait-response req))
                      (jsexpr->string resp)))

      (let ([req (read-json (open-input-file "req2.json"))]
            [resp (read-json (open-input-file "resp2.json"))])
        (client-send lsp req)
        (check-equal? (jsexpr->string (client-wait-response req))
                      (jsexpr->string resp))))))

