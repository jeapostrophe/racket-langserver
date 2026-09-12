#lang racket/base

(require rackunit
         "../../common/interfaces.rkt"
         "../../doclib/formatting.rkt")

(define (formatting-options tab-size insert-spaces key)
  (FormattingOptions #:tab-size tab-size
                     #:insert-spaces insert-spaces
                     #:trim-trailing-whitespace #f
                     #:insert-final-newline #f
                     #:trim-final-newlines #f
                     #:key key))

(module+ test
  (test-case
    "fixw ignores unsupported LSP formatting options"
    (define text "#lang racket/base\n(define x\n1)")
    (define conservative-options
      (formatting-options 2 #t #f))
    (define unrelated-options
      (formatting-options 8 #f (hasheq 'unsupported "value")))
    (define expected
      (list (TextEdit (Range (Pos 2 0) (Pos 2 2)) "  1)")))
    (check-equal?
      (formatting text
                  0
                  2
                  #:backend 'fixw
                  #:formatting-options conservative-options)
      expected)
    (check-equal?
      (formatting text
                  0
                  2
                  #:backend 'fixw
                  #:formatting-options unrelated-options)
      expected)))
