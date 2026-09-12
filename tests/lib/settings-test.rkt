#lang racket/base

(require json
         rackunit
         rackunit/text-ui
         "../../common/settings.rkt"
         "../../lsp/workspace.rkt")

(define settings-tests
  (test-suite
    "settings"

    (test-case
      "formatters default to fixw"
      (set-formatting-settings! default-formatting-settings)
      (check-equal? current-formatting-settings
                    (Formatting-Settings 'fixw 'fixw)))

    (test-case
      "configuration updates apply process-wide"
      (set-formatting-settings! default-formatting-settings)
      (update-configuration
        (list (hasheq 'formatting
                      (hasheq 'documentFormatter "fmt"
                              'indentationFormatter "drracket"))))
      (check-equal? current-formatting-settings
                    (Formatting-Settings 'fmt 'drracket))
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
                 (lambda () (Formatting-Settings 'unknown 'fixw)))
      (set-formatting-settings! default-formatting-settings))

    (test-case
      "pushed settings update the process-wide formatters"
      (set-formatting-settings! default-formatting-settings)
      (update-configuration
        (hasheq 'formatting
                (hasheq 'documentFormatter "drracket"
                        'indentationFormatter "drracket")))
      (check-equal? current-formatting-settings
                    (Formatting-Settings 'drracket 'drracket))
      (set-formatting-settings! default-formatting-settings))

    (test-case
      "omitted formatter fields use shipped defaults"
      (set-formatting-settings! (Formatting-Settings 'fmt 'drracket))
      (update-configuration
        (hasheq 'formatting (hasheq 'documentFormatter "drracket")))
      (check-equal? current-formatting-settings
                    (Formatting-Settings 'drracket 'fixw))
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
                    (Formatting-Settings 'fmt 'drracket))
      (set-resyntax-enabled! #t)
      (set-formatting-settings! default-formatting-settings))

    (test-case
      "malformed resyntax settings do not escape configuration parsing"
      (define previous-enabled? (get-resyntax-enabled))
      (check-not-exn
        (lambda () (update-configuration (hasheq 'resyntax (json-null)))))
      (check-equal? (get-resyntax-enabled) previous-enabled?)
      (set-formatting-settings! default-formatting-settings))))

(module+ test
  (run-tests settings-tests))
