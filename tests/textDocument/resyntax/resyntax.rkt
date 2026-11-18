#lang racket/base

(module+ test
  (require "../../silent-logger.rkt"
           rackunit
           "../../../common/interfaces.rkt"
           "../../../common/json-util.rkt"
           "../../../common/settings.rkt"
           (only-in "../../../doclib/doc.rkt" doc-resyntax-available?)
           "../../client.rkt"
           "../../json-dsl.rkt"
           "../with-document.rkt")

  (define uri "file:///test.rkt")
  (define code "#lang racket/base\n\n(format \"\")\n")

  (define code-action-params
    (json {#:range (->jsexpr (Range (Pos 2 8) (Pos 2 9)))
           #:context {#:diagnostics []}}))

  (when (doc-resyntax-available?)
    (test-case "resyntax publishes diagnostics and quickfix code actions"
      (with-racket-lsp
        (lambda (lsp)
          (open! lsp uri code 0)

          (define initial-diag (client-wait-notification lsp))
          (check-equal? (jsexpr-ref initial-diag '(method)) "textDocument/publishDiagnostics")

          (define resyntax-diag (client-wait-notification lsp))
          (check-equal? (jsexpr-ref resyntax-diag '(method)) "textDocument/publishDiagnostics")
          (check-false (null? (jsexpr-ref resyntax-diag '(params diagnostics))))

          (define action-span (Range (Pos 2 0) (Pos 2 11)))
          (check-equal?
            (map jsexpr->CodeAction
                 (lsp-result! lsp "textDocument/codeAction" uri code-action-params))
            (list
              (CodeAction
                "Apply rule [format-identity]"
                "quickfix"
                (list (Diagnostic action-span
                                  DiagnosticSeverity-Information
                                  "Resyntax"
                                  "[format-identity] This use of `format` does nothing."))
                #f
                (WorkspaceEdit
                  (hasheq (string->symbol uri)
                          (list (TextEdit action-span "\"\"")))))))

          (close! lsp uri))))

    (test-case "resyntax disabled: no resyntax code action"
      (define previous-enabled (get-resyntax-enabled))
      (dynamic-wind
        (lambda () (set-resyntax-enabled! #f))
        (lambda ()
          (with-document uri code
            (lambda (lsp)
              (check-equal?
                (lsp-result! lsp "textDocument/codeAction" uri code-action-params)
                '()))))
        (lambda () (set-resyntax-enabled! previous-enabled))))))
