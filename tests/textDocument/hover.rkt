#lang racket

(require "../../json-util.rkt"
         "with-document.rkt")

(define uri "file:///test.rkt")

(define code
#<<END
#lang racket/base

(list 1 2)
END
  )

(module+ test
  (require rackunit)

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

      (let ([resp (client-wait-response hover-req)])
        (check-true (jsexpr-has-key? resp '(result contents)))
        (check-false (string=? "" (jsexpr-ref resp '(result contents))))

        (check-true (jsexpr-has-key? resp '(result range start line)))
        (check-true (jsexpr-has-key? resp '(result range start character)))
        (check-true (jsexpr-has-key? resp '(result range end line)))
        (check-true (jsexpr-has-key? resp '(result range end character)))
        (check-equal? (jsexpr-ref resp '(result range start line)) 2)
        (check-equal? (jsexpr-ref resp '(result range start character)) 1)
        (check-equal? (jsexpr-ref resp '(result range end line)) 2)
        (check-equal? (jsexpr-ref resp '(result range end character)) 5)))))

