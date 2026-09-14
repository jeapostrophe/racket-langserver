#lang racket/base

(require racket/contract
         racket/port
         racket/system
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt")

;; `fmt` is an optional package. Invoke only its stable `raco fmt`
;; command-line interface so the language server does not depend on
;; `fmt`'s internal library API.

(provide fmt-format-document
         current-fmt-runner
         (struct-out exn:fail:fmt)
         (struct-out exn:fail:fmt-unavailable))

(struct exn:fail:fmt exn:fail ())
(struct exn:fail:fmt-unavailable exn:fail:fmt ())

(define (raise-fmt-unavailable detail)
  (raise
    (exn:fail:fmt-unavailable
      (string-append
        "The fmt formatter is unavailable; install the optional Racket package "
        "with `raco pkg install fmt` and retry: "
        detail)
      (current-continuation-marks))))

(define (run-fmt arguments text)
  (define raco (find-executable-path "raco"))
  (unless raco
    (raise-fmt-unavailable "could not find the raco executable"))
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define status
    (parameterize ([current-input-port (open-input-string text)]
                   [current-output-port stdout]
                   [current-error-port stderr])
      (apply system*/exit-code raco "fmt" arguments)))
  (values status (get-output-string stdout) (get-output-string stderr)))

(define current-fmt-runner (make-parameter run-fmt))

;; LSP extra properties stay raw jsexprs on FormattingOptions. This backend
;; parses only the keys it consumes; unknown extras are ignored by the decoder.
(define-json-struct Fmt-Extra-Options
  [indent (optional exact-nonnegative-integer?)]
  [max-blank-lines (optional exact-nonnegative-integer?) #:json maxBlankLines]
  [width (optional exact-nonnegative-integer?)])

(define (fmt-flag flag value)
  (if (Nothing? value)
      '()
      (list flag (number->string value))))

(define (fmt-command-options options)
  ;; Missing fields preserve fmt's defaults. Standard LSP options and unknown
  ;; extension fields are ignored.
  (define parsed
    (jsexpr->Fmt-Extra-Options (FormattingOptions-extras options)))
  (append (fmt-flag "--indent" (Fmt-Extra-Options-indent parsed))
          (fmt-flag "--max-blank-lines" (Fmt-Extra-Options-max-blank-lines parsed))
          (fmt-flag "--width" (Fmt-Extra-Options-width parsed))))

(define (fmt-command-unavailable? output)
  (regexp-match? #rx"(?i:unrecognized command:[^\n]*fmt)" output))

(define (raise-fmt-failed status stdout stderr)
  (define output (string-append stderr stdout))
  (if (fmt-command-unavailable? output)
      (raise-fmt-unavailable output)
      (raise
        (exn:fail:fmt
          (format "raco fmt command failed (exit status ~a): ~a" status output)
          (current-continuation-marks)))))

(define/contract (fmt-format-document text options)
  (-> string? FormattingOptions? (or/c string? #f))
  (define-values (status formatted stderr)
    ((current-fmt-runner) (fmt-command-options options) text))
  (unless (zero? status)
    (raise-fmt-failed status formatted stderr))
  (and (not (string=? text formatted)) formatted))
