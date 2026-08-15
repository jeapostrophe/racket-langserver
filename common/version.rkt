#lang racket/base

(require version/utils)

(provide support/check-syntax-phase-level+space-callback?
         support/check-syntax-unused-binder-callback?
         support/racket/phase+space?)

(define (version>=? target-version)
  (not (version<? (version) target-version)))

(define support/check-syntax-phase-level+space-callback?
  (version>=? "8.8"))

(define support/check-syntax-unused-binder-callback?
  (version>=? "8.11"))

(define support/racket/phase+space?
  (version>=? "8.3"))
