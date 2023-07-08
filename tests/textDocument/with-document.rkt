#lang racket

(provide with-document
         client-send
         client-wait-response
         make-request
         make-expected-response
         make-notification)

(require "../client.rkt")

(define/contract (with-document path uri text proc)
  (-> string? string? string? (-> Lsp? any/c) any/c)

  (with-racket-lsp path
    (λ (lsp)
      (define didopen-req
        (make-notification "textDocument/didOpen"
                           (hasheq 'textDocument
                                   (hasheq 'uri uri
                                           'languageId "racket"
                                           'version 0
                                           'text text))))
      (client-send lsp didopen-req)
      (client-wait-response lsp)

      (proc lsp)

      ;; no response for didClose request
      (define didclose-req
        (make-notification "textDocument/didClose"
                           (hasheq 'textDocument
                                   (hasheq 'uri uri))))
      (client-send lsp didclose-req))))
