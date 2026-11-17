#lang racket/base

(provide with-document
         change!
         notify
         client-wait-notification
         lsp-result!)

(require json
         racket/contract
         "../client.rkt")

(define/contract (with-document uri text proc #:capabilities [capabilities (hasheq)])
  (->* (string? string? (-> Session? any/c)) (#:capabilities jsexpr?) void?)
  (with-racket-lsp
    #:capabilities capabilities
    (lambda (lsp)
      (open! lsp uri text)
      (client-wait-notification lsp)
      (proc lsp)
      (close! lsp uri))))

(define (lsp-result! lsp method uri [extra (hasheq)] [timeout 90])
  (define req (make-request lsp method (hash-set extra 'textDocument (hasheq 'uri uri))))
  (client-send lsp req)
  (define resp (client-wait-response lsp req timeout))
  (unless (equal? (hash-ref resp 'jsonrpc #f) "2.0")
    (error 'lsp-result! "expected jsonrpc 2.0 in response: ~s" resp))
  (when (hash-has-key? resp 'error)
    (error 'lsp-result! "unexpected error response: ~s" resp))
  (hash-ref resp 'result))
