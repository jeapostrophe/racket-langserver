#lang racket

(require "with-document.rkt"
         "../../json-util.rkt"
         chk)

(define uri "file:///test.rkt")

(define code
  #<<END
#lang racket/base

(list )
END
  )

(module+ test
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
        (chk (jsexpr-has-key? resp '(result signatures)))
        (chk (list? (jsexpr-ref resp '(result signatures))))
        (chk (not (null? (jsexpr-ref resp '(result signatures)))))))))
