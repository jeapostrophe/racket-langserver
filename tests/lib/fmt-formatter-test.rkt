#lang racket/base

(require rackunit
         "../../common/interfaces.rkt"
         "../../doclib/formatter/fmt.rkt")

(define racket-text "#lang racket/base\n(define x 1)")

(module+ test
  (test-case
    "unchanged fmt output produces no replacement"
    (parameterize ([current-fmt-runner
                    (lambda (_arguments text) (values 0 text ""))])
      (check-false
        (fmt-format-document racket-text empty-fmt-settings))))

  (test-case
    "fmt replacement may change the document line count"
    (define replacement "#lang racket/base\n(define\n  x\n  1)\n")
    (parameterize ([current-fmt-runner
                    (lambda (_arguments _text) (values 0 replacement ""))])
      (check-equal?
        (fmt-format-document racket-text empty-fmt-settings)
        replacement))))
