#lang racket/base

(require rackunit
         "../../common/interfaces.rkt"
         "../../doclib/formatter/fmt.rkt")

(define options
  (FormattingOptions #:tab-size 2
                     #:insert-spaces #t
                     #:trim-trailing-whitespace #f
                     #:insert-final-newline #f
                     #:trim-final-newlines #f
                     #:extras (hasheq)))

(define racket-text "#lang racket/base\n(define x 1)")

(module+ test
  (test-case
    "unchanged fmt output produces no replacement"
    (parameterize ([current-fmt-runner
                    (lambda (_arguments text) (values 0 text ""))])
      (check-false
        (fmt-format-document racket-text options))))

  (test-case
    "fmt replacement may change the document line count"
    (define replacement "#lang racket/base\n(define\n  x\n  1)\n")
    (parameterize ([current-fmt-runner
                    (lambda (_arguments _text) (values 0 replacement ""))])
      (check-equal?
        (fmt-format-document racket-text options)
        replacement))))
