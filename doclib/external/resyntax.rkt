#lang racket/base

(provide run-resyntax
         resyntax-available?)

(require "../../common/dynamic-import.rkt"
         "../../common/interfaces.rkt")

(define has-resyntax? #t)

(define (resyntax-available?)
  has-resyntax?)

(define (disable-resyntax!)
  (set! has-resyntax? #f))

(dynamic-imports ('resyntax/private/source
                   string-source
                   source->string)
                 ('rebellion/base/range
                   unbounded-range)
                 ('rebellion/base/comparator
                   natural<=>)
                 ('rebellion/collection/range-set
                   range-set)
                 ('resyntax/private/string-replacement
                   string-replacement-start
                   string-replacement-original-end
                   string-replacement-render)
                 ('resyntax/private/refactoring-result
                   refactoring-result-string-replacement
                   refactoring-result-message
                   refactoring-result-rule-name
                   refactoring-result-set-results)
                 ('resyntax/default-recommendations
                   default-recommendations)
                 ('resyntax
                   resyntax-analyze)
                 disable-resyntax!)

(define (run-resyntax text _src)
  (if has-resyntax?
      (run-resyntax-impl text)
      (list)))

(define (run-resyntax-impl text)
  (define text-source (string-source text))
  (define all-lines (range-set (unbounded-range #:comparator natural<=>)))
  (define result-set
    (resyntax-analyze
      text-source
      #:suite default-recommendations
      #:lines all-lines))

  (for/list ([result (in-list (refactoring-result-set-results result-set))])
    (define sr (refactoring-result-string-replacement result))
    (define char-start (string-replacement-start sr))
    (define char-end (string-replacement-original-end sr))
    (define message (refactoring-result-message result))
    (define new-text (string-replacement-render sr (source->string text-source)))
    (define rule-name (refactoring-result-rule-name result))
    (Resyntax-Result char-start char-end message rule-name new-text)))
