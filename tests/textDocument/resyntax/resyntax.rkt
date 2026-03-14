#lang racket

(require "../../../common/dynamic-import.rkt")

(define uri "file:///test.rkt")

(define code
#<<END
#lang racket/base

(format "")
END
  )

;; detect if resyntax is available
(define has-resyntax? #t)
(dynamic-imports ('resyntax
                   resyntax-analyze)
                 (λ () (set! has-resyntax? #f)))

(module+ test
  (require rackunit
           json
           "../with-document.rkt"
           (only-in "../../client.rkt" with-racket-lsp)
           "../../../common/settings.rkt"
           "../../../common/json-util.rkt")

  (when has-resyntax?
    (with-racket-lsp
      (λ (lsp)
        (define did-open
          (make-notification
            "textDocument/didOpen"
            (hasheq 'textDocument
                    (hasheq 'uri uri
                            'languageId "racket"
                            'version 0
                            'text code))))
        (client-send lsp did-open)

        (define initial-diag (client-wait-notification lsp))
        (check-equal? (jsexpr-ref initial-diag '(method)) "textDocument/publishDiagnostics")

        (define resyntax-diag (client-wait-notification lsp))
        (check-equal? (jsexpr-ref resyntax-diag '(method)) "textDocument/publishDiagnostics")
        (check-false (empty? (jsexpr-ref resyntax-diag '(params diagnostics))))

        (let ([req (read-json (open-input-file "req.json"))]
              [resp (read-json (open-input-file "resp.json"))])
          (client-send lsp req)
          (check-equal? (jsexpr->string (client-wait-response req))
                        (jsexpr->string resp)))

        (client-send lsp
                     (make-notification
                       "textDocument/didClose"
                       (hasheq 'textDocument (hasheq 'uri uri)))))))

  (test-case
    "resyntax disabled: no resyntax code action"
    (when has-resyntax?
      (define previous-enabled (get-resyntax-enabled))
      (dynamic-wind
        (λ () (set-resyntax-enabled! #f))
        (λ ()
          (with-racket-lsp
            (λ (lsp)
              (define did-open
                (make-notification
                  "textDocument/didOpen"
                  (hasheq 'textDocument
                          (hasheq 'uri uri
                                  'languageId "racket"
                                  'version 0
                                  'text code))))
              (client-send lsp did-open)
              (client-wait-notification lsp)

              (define req
                (make-request
                  lsp
                  "textDocument/codeAction"
                  (hasheq 'context (hasheq 'diagnostics (list))
                          'range (hasheq 'start (hasheq 'line 2 'character 8)
                                         'end (hasheq 'line 2 'character 9))
                          'textDocument (hasheq 'uri uri))))

              (client-send lsp req)
              (check-equal? (jsexpr-ref (client-wait-response req) '(result)) (list))

              (client-send lsp
                           (make-notification
                             "textDocument/didClose"
                             (hasheq 'textDocument (hasheq 'uri uri)))))))
        (λ () (set-resyntax-enabled! previous-enabled))))))

