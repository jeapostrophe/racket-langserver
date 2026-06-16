#lang racket/base

(module+ test
  (require rackunit
           "../client.rkt"
           "../../common/json-util.rkt")

  (define (has-racket-collection-not-found-diagnostic? notification)
    (for/or ([diagnostic (in-list (jsexpr-ref notification '(params diagnostics)))])
      (and (equal? "Racket" (jsexpr-ref diagnostic '(source)))
           (regexp-match?
             #rx"^standard-module-name-resolver: collection not found"
             (jsexpr-ref diagnostic '(message))))))

  (with-racket-lsp
    (λ (lsp)

      ;; didopen
      (define didopen-req
        (make-notification "textDocument/didOpen"
                           (hasheq 'textDocument
                                   (hasheq 'uri "file:///test.rkt"
                                           'languageId "racket"
                                           'version 0
                                           'text "#lang racke"))))
      ;; should report "collection not found" diagnostic error
      (client-send lsp didopen-req)
      (let ([resp (client-wait-notification lsp)])
        (check-true (jsexpr-has-key? resp '(params diagnostics)))
        (check-false (null? (jsexpr-ref resp '(params diagnostics))))
        (check-true (has-racket-collection-not-found-diagnostic? resp)))


      (define didchange-req
        (make-notification "textDocument/didChange"
                           (hasheq 'textDocument
                                   (hasheq 'uri "file:///test.rkt"
                                           'version 1)
                                   'contentChanges
                                   (list (hasheq 'text "#lang racket")))))
      ;; should not report any error
      (client-send lsp didchange-req)
      (let ([resp (client-wait-notification lsp)])
        (check-true (jsexpr-has-key? resp '(params diagnostics)))
        (check-true (null? (jsexpr-ref resp '(params diagnostics)))))


      ;; no response for didClose request
      (define didclose-req
        (make-notification "textDocument/didClose"
                           (hasheq 'textDocument
                                   (hasheq 'uri "file:///test.rkt"))))
      (client-send lsp didclose-req))))

