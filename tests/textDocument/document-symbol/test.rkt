#lang racket

(define uri "file:///test.rkt")

(define code
#<<END
#lang racket
(define x 1)
(define (f y)
  (define inner 2)
  inner)
END
  )

(module+ test
  (require rackunit
           json
           "../with-document.rkt")

  ;; A client that declares hierarchicalDocumentSymbolSupport receives a
  ;; DocumentSymbol tree instead of a flat SymbolInformation list.
  (with-document uri code
    #:capabilities (hasheq 'textDocument
                           (hasheq 'documentSymbol
                                   (hasheq 'hierarchicalDocumentSymbolSupport #t)))
    (λ (lsp)
      (let ([req (read-json (open-input-file "symbol-req.json"))]
            [resp (read-json (open-input-file "symbol-resp.json"))])
        (client-send lsp req)
        (check-equal? (client-wait-response req) resp)))))
