#lang racket

(require "../with-document.rkt"
         chk
         json)

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
  (with-document "../../../main.rkt" uri code
    (λ (lsp)

      ;; definition
      (let ([req (read-json (open-input-file "definition-req1.json"))]
            [resp (read-json (open-input-file "definition-resp1.json"))])
        (client-send lsp req)

        (chk #:= (client-wait-response lsp) resp))

      ;; documentHighlight
      (let ([req (read-json (open-input-file "highlight-req1.json"))]
            [resp (read-json (open-input-file "highlight-resp1.json"))])
        (client-send lsp req)

        (chk #:= (client-wait-response lsp) resp))

      ;; symbol
      (let ([req (read-json (open-input-file "symbol-req1.json"))]
            [resp (read-json (open-input-file "symbol-resp1.json"))])
        (client-send lsp req)

        (chk #:= (client-wait-response lsp) resp))

      ;; references
      (let ([req (read-json (open-input-file "ref-req1.json"))]
            [resp (read-json (open-input-file "ref-resp1.json"))])
        (client-send lsp req)

        (chk #:= (client-wait-response lsp) resp)))))
