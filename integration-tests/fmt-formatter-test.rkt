#lang racket/base

(require rackunit
         "../common/interfaces.rkt"
         "../doclib/doc.rkt")

(define source
  "#lang racket/base\n(define (f x)\n(+ x 1))")
(define formatted
  "#lang racket/base\n(define (f x)\n  (+ x 1))")
(define options
  (FormattingOptions #:tab-size 2
                     #:insert-spaces #t
                     #:trim-trailing-whitespace #f
                     #:insert-final-newline #f
                     #:trim-final-newlines #f
                     #:extras (hasheq)))

(module+ test
  (test-case
    "installed fmt formats through the document backend"
    (define doc (make-doc "file:///fmt-integration.rkt" source))
    (check-equal?
      (doc-format-edits doc
                        (Range (Pos 0 0) (Pos 2 8))
                        #:backend 'fmt
                        #:formatting-options options)
      (list (TextEdit (Range (Pos 0 0) (Pos 2 8)) formatted)))))
