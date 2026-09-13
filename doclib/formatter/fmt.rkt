#lang racket/base

(require racket/contract
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt")

;; `fmt` is an optional package. Keep its only reference behind dynamic-require
;; so the language server can load when the formatter is not installed.

(provide fmt-format-document
         current-fmt-program-format-loader
         (struct-out exn:fail:fmt-unavailable))

(struct exn:fail:fmt-unavailable exn:fail ())

(define current-fmt-program-format-loader
  (make-parameter
    (lambda ()
      (dynamic-require 'fmt 'program-format))))

(define (raise-fmt-unavailable cause)
  (raise
    (exn:fail:fmt-unavailable
      (string-append
        "The fmt formatter is unavailable; install the optional Racket package "
        "with `raco pkg install fmt` and retry: "
        (exn-message cause))
      (current-continuation-marks))))

(define (load-fmt-program-format)
  (with-handlers ([exn:fail? raise-fmt-unavailable])
    ((current-fmt-program-format-loader))))

;; LSP extra properties stay raw jsexprs on FormattingOptions. This backend
;; parses only the keys it consumes; unknown extras are ignored by the decoder.
(define-json-struct Fmt-Extra-Options
  [indent (optional exact-nonnegative-integer?)]
  [limit (optional exact-nonnegative-integer?)]
  [max-blank-lines (optional exact-nonnegative-integer?) #:json maxBlankLines]
  [width (optional exact-nonnegative-integer?)])

(define (fmt-keyword-options options)
  ;; keyword-apply requires sorted keywords. Missing fields preserve fmt's own
  ;; defaults; standard LSP options and unknown extension fields are ignored.
  (define parsed
    (jsexpr->Fmt-Extra-Options (FormattingOptions-extras options)))
  (define supported-options
    (list (cons '#:indent (Fmt-Extra-Options-indent parsed))
          (cons '#:limit (Fmt-Extra-Options-limit parsed))
          (cons '#:max-blank-lines (Fmt-Extra-Options-max-blank-lines parsed))
          (cons '#:width (Fmt-Extra-Options-width parsed))))
  (for/lists (keywords keyword-values)
             ([option (in-list supported-options)]
              #:unless (Nothing? (cdr option)))
    (values (car option) (cdr option))))

(define/contract (fmt-format-document text options)
  (-> string? FormattingOptions? (or/c string? #f))
  (define-values (keywords keyword-values) (fmt-keyword-options options))
  (define formatted
    (keyword-apply (load-fmt-program-format) keywords keyword-values (list text)))
  (and (not (string=? text formatted)) formatted))
