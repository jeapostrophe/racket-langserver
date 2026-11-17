#lang racket/base

(module+ test
  (require "../silent-logger.rkt"
           rackunit
           "../../common/interfaces.rkt"
           "../../doclib/doc.rkt")

  (define (options [tab-size 2])
    (FormattingOptions #:tab-size tab-size
                       #:insert-spaces #t
                       #:trim-trailing-whitespace #t
                       #:insert-final-newline #f
                       #:trim-final-newlines #f
                       #:extras (hasheq)))

  (test-case "Formatting modes"
    (define normal-doc
      (make-doc "file:///test.rkt" "#lang racket/base\n\n(define (bob)\n  \n  (+ 1 2))\n"))
    (check-equal?
      (doc-format-edits normal-doc (Range (Pos 3 0) (Pos 4 0)) #:formatting-options (options))
      (list (TextEdit (Range (Pos 3 0) (Pos 3 2)) "")))

    (define interactive-doc
      (make-doc "file:///test.rkt" "#lang racket/base\n\n(define (bob)\n\n  (+ 1 2))\n"))
    (check-equal?
      (doc-format-edits interactive-doc
                        (Range (Pos 3 0) (Pos 3 0))
                        #:on-type? #t
                        #:formatting-options (options))
      (list (TextEdit (Range (Pos 3 0) (Pos 3 0)) "  "))))

  (test-case "Non-sexp and unconfigured languages follow doclib formatting policy"
    (define scribble-doc
      (make-doc "file:///test.scrbl" "#lang scribble/base\n@itemlist[\n@item{one}\n]"))
    (define scribble-range (Range (Pos 0 0) (Pos 3 0)))
    (check-equal?
      (doc-format-edits scribble-doc scribble-range #:formatting-options (options))
      (list (TextEdit (Range (Pos 2 0) (Pos 2 0)) " ")
            (TextEdit (Range (Pos 3 0) (Pos 3 0)) " ")))

    (define raw-doc (make-doc "file:///test.rkt" "(define x\n1)"))
    (check-equal?
      (doc-format-edits raw-doc (Range (Pos 0 0) (Pos 2 0)) #:formatting-options (options))
      '())

    (define rhombus-doc (make-doc "file:///test.rhm" "#lang rhombus\n  fun f():\n    1\n"))
    (check-equal?
      (doc-format-edits rhombus-doc (Range (Pos 0 0) (Pos 3 0)) #:formatting-options (options))
      '()))

  (test-case "On-type formatting delegates language policy to doclib"
    (define sexp-doc (make-doc "file:///test.rkt" "#lang racket/base\n(define x\n1)"))
    (check-equal?
      (doc-on-type-format-edits sexp-doc (Pos 2 2) ")" #:formatting-options (options))
      (list (TextEdit (Range (Pos 2 0) (Pos 2 2)) "  1)")))

    (define rhombus-doc (make-doc "file:///test.rhm" "#lang rhombus\n  fun f():\n    1)\n"))
    (check-equal?
      (doc-on-type-format-edits rhombus-doc (Pos 2 6) ")" #:formatting-options (options))
      '())

    (define unknown-doc
      (make-doc "file:///unknown.rkt" "#lang not-a-real-language\n(define x\n1)"))
    (check-equal?
      (doc-on-type-format-edits unknown-doc (Pos 2 2) ")" #:formatting-options (options))
      '())))
