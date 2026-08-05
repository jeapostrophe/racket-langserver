#lang racket/base

(require "../common/version.rkt"
         racket/class
         drracket/check-syntax)

(provide phase+space-callbacks?
         phase+space-annotations-mixin
         phase+space-shift?)

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
  (version>=8.8?))

(define phase+space-shift?
  (if phase+space-callbacks?
      (dynamic-require 'racket/phase+space 'phase+space-shift?)
      legacy-phase+space-shift?))

;; Old Check Syntax has only the legacy callbacks. Route them through the new
;; callback shape with phase 0, since the old API cannot report another phase.
(define (legacy-phase+space-annotations-mixin %)
  (class (annotations-mixin %)
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

(define phase+space-annotations-mixin
  (if phase+space-callbacks?
      annotations-mixin
      legacy-phase+space-annotations-mixin))
