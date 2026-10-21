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
      (open! lsp uri text)
      (client-wait-notification lsp)

      (proc lsp)

      (close! lsp uri))))
