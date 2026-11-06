#lang racket/base

;; Backend engine for Resyntax integration. Loaded dynamically on first use by
;; doclib/external/resyntax.rkt to defer loading heavy macro and refactoring
;; libraries until needed.

(provide run-resyntax
         resyntax-available?)

(require "../../common/dynamic-import.rkt"
         "../../common/interfaces.rkt"
         "../../common/path-util.rkt")

(define has-resyntax? #t)

(define (resyntax-available?)
  has-resyntax?)

(define (disable-resyntax!)
  (set! has-resyntax? #f))

(dynamic-imports ('resyntax/grimoire/source
                   file-source
                   modified-source
                   string-source
                   source->string)
                 ('resyntax/grimoire/string-replacement
                   string-replacement-start
                   string-replacement-original-end
                   string-replacement-new-text)
                 ('resyntax/private/refactoring-result
                   refactoring-result-string-replacement
                   refactoring-result-message
                   refactoring-result-rule-name
                   refactoring-result-set-results)
                 ('resyntax
                   resyntax-analyze)
                 disable-resyntax!)

(define (run-resyntax text uri)
  (if has-resyntax?
      (run-resyntax-impl text uri)
      (list)))

(define (run-resyntax-impl text uri)
  (define file-path (and (string? uri) (file-uri? uri) (uri->path uri)))
  (define text-source
    (if file-path
        (modified-source (file-source file-path) text)
        (string-source text)))
  (define result-set (resyntax-analyze text-source))

  (for/list ([result (in-list (refactoring-result-set-results result-set))])
    (define sr (refactoring-result-string-replacement result))
    (define char-start (string-replacement-start sr))
    (define char-end (string-replacement-original-end sr))
    (define message (refactoring-result-message result))
    (define new-text (string-replacement-new-text sr (source->string text-source)))
    (define rule-name (refactoring-result-rule-name result))
    (Resyntax-Result char-start char-end message rule-name new-text)))
