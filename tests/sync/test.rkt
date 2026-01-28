#lang racket/base

(require "../client.rkt"
         "../../json-util.rkt")

(module+ test
  (require rackunit
           json
           racket/port)

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
          (define diagnostics-msg (jsexpr-ref resp '(params diagnostics)))
          (check-false (null? diagnostics-msg))
          (define dm (with-input-from-string
                         (jsexpr->string (car diagnostics-msg))
                       (lambda () (read-json))))
          (define resp-no-message (hash-remove dm 'message))
          (check-equal? (jsexpr->string resp-no-message)
                        (jsexpr->string (read-json (open-input-file "diagnostics.json")))))


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
