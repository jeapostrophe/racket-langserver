#lang racket/base

(module+ test
  (require "../silent-logger.rkt"
           "../json-dsl.rkt"
           rackunit
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../../common/settings.rkt"
           "../../lsp/workspace.rkt")

  (define empty-fmt-settings
    (Formatting-Settings-fmt-settings default-formatting-settings))

  (define (with-clean-settings proc)
    (dynamic-wind
      (lambda ()
        (set-formatting-settings! default-formatting-settings)
        (set-resyntax-enabled! default-resyntax-enabled))
      proc
      (lambda ()
        (set-formatting-settings! default-formatting-settings)
        (set-resyntax-enabled! default-resyntax-enabled))))

  (test-case "formatters default to fixw"
    (with-clean-settings
      (lambda ()
        (check-equal? current-formatting-settings
                      (Formatting-Settings 'fixw 'fixw empty-fmt-settings)))))

  (test-case "configuration updates apply process-wide"
    (with-clean-settings
      (lambda ()
        (update-configuration
          (json [{#:formatting {#:documentFormatter "fmt"
                                #:indentationFormatter "drracket"}}]))
        (check-equal? current-formatting-settings
                      (Formatting-Settings 'fmt 'drracket empty-fmt-settings)))))

  (test-case "invalid formatter values return to safe defaults"
    (with-clean-settings
      (lambda ()
        (update-configuration
          (json [{#:formatting {#:documentFormatter "unknown"
                                #:indentationFormatter "fmt"}}]))
        (check-equal? current-formatting-settings default-formatting-settings)
        (check-exn exn:fail:contract?
                   (lambda () (Formatting-Settings 'unknown 'fixw empty-fmt-settings))))))

  (test-case "pushed settings update the process-wide formatters"
    (with-clean-settings
      (lambda ()
        (update-configuration
          (json {#:formatting {#:documentFormatter "drracket"
                               #:indentationFormatter "drracket"}}))
        (check-equal? current-formatting-settings
                      (Formatting-Settings 'drracket 'drracket empty-fmt-settings)))))

  (test-case "omitted formatter fields use shipped defaults"
    (with-clean-settings
      (lambda ()
        (set-formatting-settings! (Formatting-Settings 'fmt 'drracket empty-fmt-settings))
        (update-configuration
          (json {#:formatting {#:documentFormatter "drracket"}}))
        (check-equal? current-formatting-settings
                      (Formatting-Settings 'drracket 'fixw empty-fmt-settings)))))

  (test-case "removed formatting configuration returns to fixw"
    (with-clean-settings
      (lambda ()
        (set-formatting-settings! (Formatting-Settings 'fmt 'drracket empty-fmt-settings))
        (update-configuration (json {#:resyntax {#:enable #f}}))
        (check-equal? current-formatting-settings
                      (Formatting-Settings 'fixw 'fixw empty-fmt-settings)))))

  (test-case "null and empty configuration return to shipped defaults"
    (with-clean-settings
      (lambda ()
        (for ([payload (in-list (json [null [null] {}]))])
          (set-formatting-settings! (Formatting-Settings 'fmt 'drracket empty-fmt-settings))
          (set-resyntax-enabled! #f)
          (update-configuration payload)
          (check-equal? current-formatting-settings
                        (Formatting-Settings 'fixw 'fixw empty-fmt-settings))
          (check-equal? (get-resyntax-enabled) default-resyntax-enabled)))))

  (test-case "malformed resyntax settings do not escape configuration parsing"
    (with-clean-settings
      (lambda ()
        (define previous-enabled? (get-resyntax-enabled))
        (check-not-exn
          (lambda () (update-configuration (json {#:resyntax null}))))
        (check-equal? (get-resyntax-enabled) previous-enabled?))))

  (test-case "fmtSettings are stored process-wide"
    (with-clean-settings
      (lambda ()
        (define fmt-settings (Fmt-Settings 3 2 91))
        (update-configuration
          (json {#:formatting {#:documentFormatter "fmt"
                               #:fmtSettings (->jsexpr fmt-settings)}}))
        (check-equal? current-formatting-settings
                      (Formatting-Settings 'fmt 'fixw fmt-settings)))))

  (test-case "omitted fmtSettings use shipped defaults"
    (with-clean-settings
      (lambda ()
        (define fmt-settings (Fmt-Settings omitted omitted 80))
        (set-formatting-settings! (Formatting-Settings 'fmt 'drracket fmt-settings))
        (update-configuration
          (json {#:formatting {#:documentFormatter "fmt"}}))
        (check-equal? current-formatting-settings
                      (Formatting-Settings 'fmt 'fixw empty-fmt-settings)))))

  (test-case "malformed fmtSettings do not escape configuration parsing"
    (with-clean-settings
      (lambda ()
        (set-formatting-settings! (Formatting-Settings 'fmt 'drracket empty-fmt-settings))
        (check-not-exn
          (lambda ()
            (update-configuration
              (json {#:formatting {#:documentFormatter "fmt"
                                   #:fmtSettings {#:width "91"}}}))))
        (check-equal? current-formatting-settings
                      (Formatting-Settings 'fmt 'drracket empty-fmt-settings)))))

  (test-case "didChangeConfiguration applies a pushed racket-langserver section"
    (with-clean-settings
      (lambda ()
        (define requested '())
        (define (record-request method _params _handler)
          (set! requested (cons method requested)))
        (didChangeConfiguration
          record-request
          (json {#:settings {#:racket-langserver {#:formatting {#:documentFormatter "fmt"}}}}))
        (check-equal? current-formatting-settings
                      (Formatting-Settings 'fmt 'fixw empty-fmt-settings))
        (check-equal? requested '()))))

  (test-case "didChangeConfiguration pulls when there is no racket-langserver section"
    (with-clean-settings
      (lambda ()
        (for ([settings (in-list (json [null {} {#:pylsp {#:x 1}}]))])
          (set-formatting-settings! (Formatting-Settings 'fmt 'drracket empty-fmt-settings))
          (define requested '())
          (define (record-request method _params _handler)
            (set! requested (cons method requested)))
          (parameterize ([client-capability-workspace/configuration? #t])
            (check-not-exn
              (lambda () (didChangeConfiguration record-request (json {#:settings settings})))))
          (check-equal? requested '("workspace/configuration"))
          (check-equal? current-formatting-settings
                        (Formatting-Settings 'fmt 'drracket empty-fmt-settings))))))

  (test-case "didChangeConfiguration does not pull when the client cannot answer"
    (with-clean-settings
      (lambda ()
        (define requested '())
        (define (record-request method _params _handler)
          (set! requested (cons method requested)))
        (parameterize ([client-capability-workspace/configuration? #f])
          (check-not-exn
            (lambda () (didChangeConfiguration record-request (json {#:settings null})))))
        (check-equal? requested '())))))
