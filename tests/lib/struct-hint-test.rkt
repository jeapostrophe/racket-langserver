#lang racket/base

(module+ test
  (require rackunit)

  (require/expose "../../doclib/struct-hint.rkt"
                  (Field-Spec
                    Field-Spec-name
                    Field-Spec-auto?
                    racket-field-spec
                    typed-racket-field-spec
                    parse-field-list
                    constructor-field-names))

  ;; A field list is a parenthesized form like any other, so reading one on its
  ;; own is enough to parse one.
  (define ((parse read-field-spec) field-list-text)
    (parse-field-list read-field-spec
                      (read-syntax 'test (open-input-string field-list-text))))

  (define ((specs read-field-spec) field-list-text)
    (define parsed ((parse read-field-spec) field-list-text))
    (and parsed
         (for/list ([spec (in-list parsed)])
           (list (Field-Spec-name spec) (Field-Spec-auto? spec)))))

  (define racket-specs (specs racket-field-spec))
  (define parse-racket (parse racket-field-spec))
  (define typed-specs (specs typed-racket-field-spec))
  (define parse-typed (parse typed-racket-field-spec))

  (test-case
    "Racket reads a plain name as a field spec of that name"
    (check-equal? (racket-specs "(x y)") '(("x" #f) ("y" #f)))
    (check-equal? (racket-specs "()") '()))

  (test-case
    "Racket names a [name option ...] spec by its first part"
    (check-equal? (racket-specs "([dx real?] [dy real?])") '(("dx" #f) ("dy" #f)))
    (check-equal? (racket-specs "(a [b #:mutable] c)")
                  '(("a" #f) ("b" #f) ("c" #f))))

  (test-case
    "Racket marks a field auto when #:auto is among its options"
    (check-equal? (racket-specs "(a [b #:auto])") '(("a" #f) ("b" #t)))
    (check-equal? (racket-specs "([b #:auto #:mutable])") '(("b" #t))))

  (test-case
    "whitespace and comments between specs are not specs"
    (check-equal? (racket-specs "(x ;; the first one\n   y)")
                  '(("x" #f) ("y" #f)))
    (check-equal? (racket-specs "(x #;(y) z)") '(("x" #f) ("z" #f)))
    (check-equal? (typed-specs "([x : Integer] ;; across\n [y : Integer])")
                  '(("x" #f) ("y" #f))))

  (test-case
    "Racket rejects a spec that is neither a name nor [name option ...]"
    (check-false (parse-racket "(x 1)"))
    (check-false (parse-racket "([(x) real?])"))
    (check-false (parse-racket "(x #:mutable)")))

  (test-case
    "Typed Racket reads the name of a [name : Type] spec"
    (check-equal? (typed-specs "([x : Integer] [y : Integer])")
                  '(("x" #f) ("y" #f)))
    (check-equal? (typed-specs "([v : (Listof Integer)])") '(("v" #f)))
    (check-equal? (typed-specs "()") '()))

  (test-case
    "Typed Racket rejects a spec written without its annotation"
    (check-false (parse-typed "(x y)"))
    (check-false (parse-typed "([x : Integer] y)"))
    (check-false (parse-typed "([x real?])"))
    (check-false (parse-typed "([x : Integer #:auto])")))

  (test-case
    "constructor arguments are the declared fields, minus the auto ones"
    (define (names . specs)
      (constructor-field-names specs))
    (check-equal? (names) '())
    (check-equal? (names (Field-Spec "x" #f) (Field-Spec "y" #f))
                  '("x" "y"))
    (check-equal? (names (Field-Spec "a" #f) (Field-Spec "b" #t))
                  '("a"))
    (check-equal? (names (Field-Spec "a" #t) (Field-Spec "b" #t))
                  '())))
