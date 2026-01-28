#lang racket/base

(require "../client.rkt"
         "../../json-util.rkt"
         chk
         json)

(module+ test
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
        (let ([resp (client-wait-response lsp)])
          (chk*
           (chk (jsexpr-has-key? resp '(params diagnostics)))
           (define diagnostics-msg (jsexpr-ref resp '(params diagnostics)))
           (chk (not (null? diagnostics-msg)))
           (define resp-no-message (hash-remove (car diagnostics-msg) 'message))
           (chk #:= resp-no-message (read-json (open-input-file "diagnostics.json")))))


        (define didchange-req
          (make-notification "textDocument/didChange"
                             (hasheq 'textDocument
                                     (hasheq 'uri "file:///test.rkt"
                                             'version 1)
                                     'contentChanges
                                     (list (hasheq 'text "#lang racket")))))
        ;; should not report any error
        (client-send lsp didchange-req)
        (let ([resp (client-wait-response lsp)])
          (chk*
           (chk (jsexpr-has-key? resp '(params diagnostics)))
           (chk (null? (jsexpr-ref resp '(params diagnostics))))))


        ;; no response for didClose request
        (define didclose-req
          (make-notification "textDocument/didClose"
                             (hasheq 'textDocument
                                     (hasheq 'uri "file:///test.rkt"))))
        (client-send lsp didclose-req))))
