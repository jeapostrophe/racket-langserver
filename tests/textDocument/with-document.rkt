#lang racket

(provide with-document
         client-send
         client-wait-response
         client-wait-notification
         make-request
         make-expected-response
         make-notification)

(require json
         "../client.rkt")

(define/contract (with-document uri text proc #:capabilities [capabilities (hasheq)])
  (->* (string? string? (-> any/c any/c)) (#:capabilities jsexpr?) any/c)

  (with-racket-lsp
    #:capabilities capabilities
    (λ (lsp)
      (define didopen-req
        (make-notification "textDocument/didOpen"
                           (hasheq 'textDocument
                                   (hasheq 'uri uri
                                           'languageId "racket"
                                           'version 0
                                           'text text))))
      (client-send lsp didopen-req)
      (client-wait-notification lsp)

      (proc lsp)

      ;; no response for didClose request
      (define didclose-req
        (make-notification "textDocument/didClose"
                           (hasheq 'textDocument
                                   (hasheq 'uri uri))))
      (client-send lsp didclose-req))))

