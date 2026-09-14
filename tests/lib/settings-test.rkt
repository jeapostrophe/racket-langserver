#lang racket/base

(require json
         rackunit
         rackunit/text-ui
         "../../common/interfaces.rkt"
         "../../common/settings.rkt"
         "../../lsp/workspace.rkt")

(define empty-fmt-settings
  (Formatting-Settings-fmt-settings default-formatting-settings))

(define settings-tests
  (test-suite
    "settings"

    (test-case
      "formatters default to fixw"
      (set-formatting-settings! default-formatting-settings)
      (check-equal? current-formatting-settings
                    (Formatting-Settings 'fixw 'fixw empty-fmt-settings)))

    (test-case
      "configuration updates apply process-wide"
      (set-formatting-settings! default-formatting-settings)
      (update-configuration
        (list (hasheq 'formatting
                      (hasheq 'documentFormatter "fmt"
                              'indentationFormatter "drracket"))))
      (check-equal? current-formatting-settings
                    (Formatting-Settings 'fmt 'drracket empty-fmt-settings))
      (set-formatting-settings! default-formatting-settings))

    (test-case
      "invalid formatter values return to safe defaults"
      (set-formatting-settings! default-formatting-settings)
      (update-configuration
        (list (hasheq 'formatting
                      (hasheq 'documentFormatter "unknown"
                              'indentationFormatter "fmt"))))
      (check-equal? current-formatting-settings
                    default-formatting-settings)
      (check-exn exn:fail:contract?
                 (lambda () (Formatting-Settings 'unknown 'fixw empty-fmt-settings)))
      (set-formatting-settings! default-formatting-settings))

    (test-case
      "pushed settings update the process-wide formatters"
      (set-formatting-settings! default-formatting-settings)
      (update-configuration
        (hasheq 'formatting
                (hasheq 'documentFormatter "drracket"
                        'indentationFormatter "drracket")))
      (check-equal? current-formatting-settings
                    (Formatting-Settings 'drracket 'drracket empty-fmt-settings))
      (set-formatting-settings! default-formatting-settings))

    (test-case
      "omitted formatter fields use shipped defaults"
      (set-formatting-settings! (Formatting-Settings 'fmt 'drracket empty-fmt-settings))
      (update-configuration
        (hasheq 'formatting (hasheq 'documentFormatter "drracket")))
      (check-equal? current-formatting-settings
                    (Formatting-Settings 'drracket 'fixw empty-fmt-settings))
      (set-formatting-settings! default-formatting-settings))

    (test-case
      "unrelated pushed settings preserve formatter choices"
      (set-formatting-settings! default-formatting-settings)
      (update-configuration
        (hasheq 'formatting
                (hasheq 'documentFormatter "fmt"
                        'indentationFormatter "drracket")))
      (update-configuration (hasheq 'resyntax (hasheq 'enable #f)))
      (check-equal? current-formatting-settings
                    (Formatting-Settings 'fmt 'drracket empty-fmt-settings))
      (set-resyntax-enabled! #t)
      (set-formatting-settings! default-formatting-settings))

    (test-case
      "malformed resyntax settings do not escape configuration parsing"
      (define previous-enabled? (get-resyntax-enabled))
      (check-not-exn
        (lambda () (update-configuration (hasheq 'resyntax (json-null)))))
      (check-equal? (get-resyntax-enabled) previous-enabled?)
      (set-formatting-settings! default-formatting-settings))

    (test-case
      "fmtSettings are stored process-wide"
      (define fmt-settings
        (jsexpr->Fmt-Settings (hasheq 'width 91
                                      'indent 3
                                      'maxBlankLines 2)))
      (set-formatting-settings! default-formatting-settings)
      (update-configuration
        (hasheq 'formatting
                (hasheq 'documentFormatter "fmt"
                        'fmtSettings (hasheq 'width 91
                                             'indent 3
                                             'maxBlankLines 2))))
      (check-equal? current-formatting-settings
                    (Formatting-Settings 'fmt 'fixw fmt-settings))
      (set-formatting-settings! default-formatting-settings))

    (test-case
      "omitted fmtSettings use shipped defaults"
      (define fmt-settings
        (jsexpr->Fmt-Settings (hasheq 'width 80)))
      (set-formatting-settings!
        (Formatting-Settings 'fmt 'drracket fmt-settings))
      (update-configuration
        (hasheq 'formatting (hasheq 'documentFormatter "fmt")))
      (check-equal? current-formatting-settings
                    (Formatting-Settings 'fmt 'fixw empty-fmt-settings))
      (set-formatting-settings! default-formatting-settings))

    (test-case
      "malformed fmtSettings do not escape configuration parsing"
      (set-formatting-settings!
        (Formatting-Settings 'fmt 'drracket empty-fmt-settings))
      (check-not-exn
        (lambda ()
          (update-configuration
            (hasheq 'formatting
                    (hasheq 'documentFormatter "fmt"
                            'fmtSettings (hasheq 'width "91"))))))
      (check-equal? current-formatting-settings
                    (Formatting-Settings 'fmt 'drracket empty-fmt-settings))
      (set-formatting-settings! default-formatting-settings))))

(module+ test
  (run-tests settings-tests))
