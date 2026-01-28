#lang racket

(require "../with-document.rkt")

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
           json)

  (with-document uri code
    (λ (lsp)

      ;; definition
      (let ([req (read-json (open-input-file "definition-req1.json"))]
            [resp (read-json (open-input-file "definition-resp1.json"))])
        (client-send lsp req)

        (check-equal? (jsexpr->string (client-wait-response lsp))
                      (jsexpr->string resp)))

      ;; documentHighlight
      (let ([req (read-json (open-input-file "highlight-req1.json"))]
            [resp (read-json (open-input-file "highlight-resp1.json"))])
        (client-send lsp req)

        (check-equal? (jsexpr->string (client-wait-response lsp))
                      (jsexpr->string resp)))

      ;; symbol
      (let ([req (read-json (open-input-file "symbol-req1.json"))]
            [resp (read-json (open-input-file "symbol-resp1.json"))])
        (client-send lsp req)

        (check-equal? (jsexpr->string (client-wait-response lsp))
                      (jsexpr->string resp)))

      ;; references
      (let ([req (read-json (open-input-file "ref-req1.json"))]
            [resp (read-json (open-input-file "ref-resp1.json"))])
        (client-send lsp req)

        (check-equal? (jsexpr->string (client-wait-response lsp))
                      (jsexpr->string resp))))))
