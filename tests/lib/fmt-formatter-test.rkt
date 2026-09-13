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

(module+ test
  (test-case
    "unchanged fmt output produces no replacement"
    (parameterize ([current-fmt-program-format-loader
                    (lambda () (lambda (text) text))])
      (check-false
        (fmt-format-document "(define x 1)" options))))

  (test-case
    "fmt replacement may change the document line count"
    (define replacement "(define\n  x\n  1)\n")
    (parameterize ([current-fmt-program-format-loader
                    (lambda () (lambda (_text) replacement))])
      (check-equal?
        (fmt-format-document "(define x 1)" options)
        replacement))))
