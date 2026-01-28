#lang racket

(require "../with-document.rkt"
         chk
         json)

(define uri "file:///test.rkt")

(define code
  #<<END
#lang racket/base

(define x 0)
END
  )

(module+ test
  (with-document uri code
    (λ (lsp)
      (let ([req (read-json (open-input-file "req1.json"))]
            [resp (read-json (open-input-file "resp1.json"))])
        (client-send lsp req)

        (chk #:= (client-wait-response lsp) resp)))))