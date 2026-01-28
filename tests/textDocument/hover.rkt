#lang racket

(require "../../json-util.rkt"
         "with-document.rkt"
         chk)

(define uri "file:///test.rkt")

(define code
  #<<END
#lang racket/base

(list 1 2)
END
  )

(module+ test
  (with-document uri code
    (λ (lsp)

      (define hover-req
        (make-request lsp
                      "textDocument/hover"
                      (hasheq 'textDocument
                              (hasheq 'uri uri)
                              'position
                              (hasheq 'line 2 'character 1))))
      (client-send lsp hover-req)

      (let ([resp (client-wait-response lsp)])
        (chk (jsexpr-has-key? resp '(result contents)))
        (chk (not (string=? "" (jsexpr-ref resp '(result contents)))))

        (chk (jsexpr-has-key? resp '(result range start line)))
        (chk (jsexpr-has-key? resp '(result range start character)))
        (chk (jsexpr-has-key? resp '(result range end line)))
        (chk (jsexpr-has-key? resp '(result range end character)))
        (chk #:= (jsexpr-ref resp '(result range start line)) 2)
        (chk #:= (jsexpr-ref resp '(result range start character)) 1)
        (chk #:= (jsexpr-ref resp '(result range end line)) 2)
        (chk #:= (jsexpr-ref resp '(result range end character)) 5)))))
