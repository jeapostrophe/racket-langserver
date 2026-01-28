#lang racket

(require "../with-document.rkt"
         "../../../service/dynamic-import.rkt"
         "../../../json-util.rkt"
         chk
         json)

(define uri "file:///test.rkt")

(define code
  #<<END
#lang racket/base

(format "")
END
  )

;; detect if resyntax is available
(define has-resyntax? #t)
(dynamic-imports ('resyntax
                  resyntax-analyze)
                 (λ () (set! has-resyntax? #f)))

(module+ test
  (when has-resyntax?
    (with-document uri code
      (λ (lsp)
        (define diag (client-wait-response lsp))
        (chk #:= (jsexpr-ref diag '(method)) "textDocument/publishDiagnostics")
        (let ([req (read-json (open-input-file "req.json"))]
              [resp (read-json (open-input-file "resp.json"))])
          (client-send lsp req)
          (chk #:= (client-wait-response lsp) resp))))))

