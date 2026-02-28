#lang racket

(define uri "file:///test.rkt")

(define code
#<<END
#lang racket/base

(list )
END
  )

(module+ test
  (require rackunit
           "with-document.rkt"
           "../../json-util.rkt")

  (with-document uri code
    (λ (lsp)

      (define help-req
        (make-request lsp
                      "textDocument/signatureHelp"
                      (hasheq 'textDocument
                              (hasheq 'uri uri)
                              'position
                              (hasheq 'line 2 'character 6))))
      (client-send lsp help-req)

      (let ([resp (client-wait-response help-req)])
        (check-true (jsexpr-has-key? resp '(result signatures)))
        (check-true (list? (jsexpr-ref resp '(result signatures))))
        (check-true (not (null? (jsexpr-ref resp '(result signatures)))))))))

