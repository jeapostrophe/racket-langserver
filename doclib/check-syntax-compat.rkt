#lang racket/base

(require "../common/version.rkt"
         racket/class
         racket/function
         drracket/check-syntax)

(provide check-syntax-annotations-mixin
         phase+space-shift?
         phase+space-callbacks?
         unused-binder-callbacks?)

;; racket/phase+space is unavailable on supported old Racket versions.
(define (legacy-phase+space-shift? value)
  (define (phase? value)
    (or (not value)
        (exact-integer? value)))
  (or (phase? value)
      (and (pair? value)
           (phase? (car value))
           (or (not (cdr value))
               (and (symbol? (cdr value))
                    (symbol-interned? (cdr value)))))))

(define phase+space-callbacks?
  (and support/check-syntax-phase-level+space-callback?
       support/racket/phase+space?))

(define unused-binder-callbacks?
  support/check-syntax-unused-binder-callback?)

(define phase+space-shift?
  (if phase+space-callbacks?
      (dynamic-require 'racket/phase+space 'phase+space-shift?)
      legacy-phase+space-shift?))

;; Old Check Syntax calls the legacy methods. Route them into the canonical
;; `/phase-level+space` methods with phase 0 so collectors use one callback path.
(define (legacy-phase+space-annotations-mixin %)
  (class %
    (define/override (syncheck:add-definition-target
                       source start end id submods)
      (send this
            syncheck:add-definition-target/phase-level+space
            source start end id submods 0))

    (define/public (syncheck:add-definition-target/phase-level+space
                     _source _start _end _id _submods _phase+space)
      (void))

    (define/override (syncheck:add-jump-to-definition
                       source start end id filename submods)
      (send this
            syncheck:add-jump-to-definition/phase-level+space
            source start end id filename submods 0))

    (define/public (syncheck:add-jump-to-definition/phase-level+space
                     _source _start _end _id _filename _submods _phase+space)
      (void))

    (super-new)))

(define (identity-mixin %)
  %)

(define phase+space-compat-mixin
  (if phase+space-callbacks?
      identity-mixin
      legacy-phase+space-annotations-mixin))

;; Check Syntax before Racket 8.11 has no unused-binder method. Add a no-op
;; method so downstream collectors can override one callback surface.
(define (legacy-unused-binder-annotations-mixin %)
  (class %
    (define/public (syncheck:unused-binder _source _left _right)
      (void))

    (super-new)))

(define unused-binder-compat-mixin
  (if unused-binder-callbacks?
      identity-mixin
      legacy-unused-binder-annotations-mixin))

;; Inheritance order, inner to outer: upstream annotations, phase+space
;; compatibility, then unused-binder compatibility.
(define check-syntax-annotations-mixin
  (compose unused-binder-compat-mixin
           phase+space-compat-mixin
           annotations-mixin))
