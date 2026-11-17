#lang racket/base

(module+ test
  (require rackunit
           "../client.rkt"
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt")

  (define expected-diagnostic
    (->jsexpr
      (Diagnostic
        (Range (Pos 0 0) (Pos 0 11))
        DiagnosticSeverity-Error
        "Racket"
        (string-append
          "Cannot find language \"racke\".\n"
          "  module path: racke/lang/reader\n"
          "Check that the language name is correct and the package is installed."))))

  (test-case "document sync publishes and clears diagnostics across open, edit, and close"
    (define uri "file:///test.rkt")
    (with-racket-lsp
      (lambda (lsp)
        (open! lsp uri "#lang racke" 0)
        (define open-diag (client-wait-notification lsp))
        (check-not-false (member expected-diagnostic (jsexpr-ref open-diag '(params diagnostics))))

        (change! lsp uri "#lang racket" 1)
        (define change-diag (client-wait-notification lsp))
        (check-equal? (jsexpr-ref change-diag '(params diagnostics)) '())

        (close! lsp uri)))))
