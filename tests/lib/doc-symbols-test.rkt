#lang racket/base

(module+ test
  (require "../silent-logger.rkt"
           rackunit
           racket/list
           "../../common/interfaces.rkt"
           "../../doclib/doc.rkt")

  (test-case "doc-symbols-hierarchical returns nested DocumentSymbols"
    (define text
#<<END
#lang racket
(define x 1)
(define (f y)
  (define inner 2)
  inner)
(module+ test
  (check-equal? (f 1) 2))
(struct point (x y))
END
      )
    (define d (make-doc "file:///test.rkt" text))
    (define syms (doc-symbols-hierarchical d))
    (check-equal? (map DocumentSymbol-name syms) '("x" "f" "test" "point"))
    (check-equal? (map DocumentSymbol-kind syms)
                  (list SymbolKind-Variable
                        SymbolKind-Function
                        SymbolKind-Module
                        SymbolKind-Struct))

    (define x-sym (first syms))
    (check-equal? (DocumentSymbol-range x-sym) (Range (Pos 1 0) (Pos 1 12)))
    (check-equal? (DocumentSymbol-selectionRange x-sym) (Range (Pos 1 8) (Pos 1 9)))
    (check-equal? (DocumentSymbol-children x-sym) '())

    (define f-sym (second syms))
    (check-equal? (DocumentSymbol-range f-sym) (Range (Pos 2 0) (Pos 4 8)))
    (check-equal? (DocumentSymbol-selectionRange f-sym) (Range (Pos 2 9) (Pos 2 10)))
    (check-equal? (map DocumentSymbol-name (DocumentSymbol-children f-sym)) '("inner"))
    (define inner-sym (first (DocumentSymbol-children f-sym)))
    (check-equal? (DocumentSymbol-kind inner-sym) SymbolKind-Variable)
    (check-equal? (DocumentSymbol-range inner-sym) (Range (Pos 3 2) (Pos 3 18)))
    (check-equal? (DocumentSymbol-selectionRange inner-sym) (Range (Pos 3 10) (Pos 3 15)))

    (check-equal? (DocumentSymbol-children (third syms)) '())
    (define point-sym (fourth syms))
    (check-equal? (DocumentSymbol-range point-sym) (Range (Pos 7 0) (Pos 7 20)))
    (check-equal? (DocumentSymbol-selectionRange point-sym) (Range (Pos 7 8) (Pos 7 13))))

  (test-case "doc-symbols-hierarchical skips definitions in non-definition forms heads"
    (define d (make-doc "file:///test.rkt" "#lang racket\n(let ([x 1])\n  x)\n(displayln \"hello\")\n"))
    (check-equal? (doc-symbols-hierarchical d) '()))

  (test-case "doc-symbols-hierarchical handles container forms and class members"
    (define text
#<<END
#lang racket
(provide foo
         bar)
(define foo%
  (class object%
    (init-field x)
    (field [y 0] z)
    (define/public (m) x)))
(define (foo lst)
  (match lst
    [(list a) a]))
END
      )
    (define d (make-doc "file:///test.rkt" text))
    (define syms (doc-symbols-hierarchical d))
    (check-equal? (map DocumentSymbol-name syms) '("provide" "foo%" "foo"))

    (define provide-sym (first syms))
    (check-equal? (DocumentSymbol-kind provide-sym) SymbolKind-Namespace)
    (check-equal? (DocumentSymbol-range provide-sym) (Range (Pos 1 0) (Pos 2 13)))
    (check-equal? (DocumentSymbol-selectionRange provide-sym) (Range (Pos 1 1) (Pos 1 8)))
    (check-equal? (DocumentSymbol-children provide-sym) '())

    (define foo%-sym (second syms))
    (check-equal? (map DocumentSymbol-name (DocumentSymbol-children foo%-sym)) '("class"))
    (define class-sym (first (DocumentSymbol-children foo%-sym)))
    (check-equal? (DocumentSymbol-kind class-sym) SymbolKind-Class)
    (check-equal? (DocumentSymbol-selectionRange class-sym) (Range (Pos 4 3) (Pos 4 8)))
    (check-equal? (map DocumentSymbol-name (DocumentSymbol-children class-sym)) '("x" "y" "z" "m"))
    (check-equal? (map DocumentSymbol-kind (DocumentSymbol-children class-sym))
                  (list SymbolKind-Field SymbolKind-Field SymbolKind-Field SymbolKind-Function))
    (define y-sym (second (DocumentSymbol-children class-sym)))
    (check-equal? (DocumentSymbol-range y-sym) (Range (Pos 6 11) (Pos 6 16)))
    (check-equal? (DocumentSymbol-selectionRange y-sym) (Range (Pos 6 12) (Pos 6 13)))

    (define foo-sym (third syms))
    (check-equal? (map DocumentSymbol-name (DocumentSymbol-children foo-sym)) '("match"))
    (define match-sym (first (DocumentSymbol-children foo-sym)))
    (check-equal? (DocumentSymbol-kind match-sym) SymbolKind-Object)
    (check-equal? (DocumentSymbol-range match-sym) (Range (Pos 9 2) (Pos 10 17)))
    (check-equal? (DocumentSymbol-children match-sym) '()))

  (test-case "doc-symbols-hierarchical handles require forms"
    (define d (make-doc "file:///test.rkt" "#lang racket\n(require racket/match\n         racket/list)\n"))
    (define syms (doc-symbols-hierarchical d))
    (check-equal? (map DocumentSymbol-name syms) '("require"))
    (define require-sym (first syms))
    (check-equal? (DocumentSymbol-kind require-sym) SymbolKind-Namespace)
    (check-equal? (DocumentSymbol-range require-sym) (Range (Pos 1 0) (Pos 2 21)))
    (check-equal? (DocumentSymbol-selectionRange require-sym) (Range (Pos 1 1) (Pos 1 8)))
    (check-equal? (DocumentSymbol-children require-sym) '()))

  (test-case "doc-symbols-hierarchical tolerates incomplete code"
    (define d (make-doc "file:///test.rkt" "#lang racket\n(define (broken"))
    (define syms (doc-symbols-hierarchical d))
    (check-equal? (map DocumentSymbol-name syms) '("broken"))
    (check-equal? (DocumentSymbol-kind (first syms)) SymbolKind-Function)
    (check-equal? (DocumentSymbol-range (first syms)) (Range (Pos 1 0) (Pos 1 15))))

  (test-case "doc-symbols-hierarchical language guard"
    (define rhombus-doc
      (make-doc "file:///test.rhm"
                "#lang rhombus\ndef y = (match x\n         | 1: 2\n         | ~else: 3)\n"))
    (check-equal? (doc-symbols-hierarchical rhombus-doc) '())
    (define scribble-doc
      (make-doc "file:///test.scrbl" "#lang scribble/manual\n@(define foo 1)\n"))
    (check-equal? (doc-symbols-hierarchical scribble-doc) '()))

  (test-case "doc-symbols returns lexer-derived symbol information"
    (define uri "file:///tmp/doc-test.rkt")
    (define d (make-doc uri "#lang racket\n(define x 1)\nx\n"))
    (define result (doc-symbols d uri))
    (check-equal? (length result) 4)
    (define define-sym (findf (lambda (s) (equal? (SymbolInformation-name s) "define")) result))
    (check-not-false define-sym)
    (check-equal? (SymbolInformation-kind define-sym) SymbolKind-Variable)
    (define one-sym (findf (lambda (s) (equal? (SymbolInformation-name s) "1")) result))
    (check-not-false one-sym)
    (check-equal? (SymbolInformation-kind one-sym) SymbolKind-Constant)))
