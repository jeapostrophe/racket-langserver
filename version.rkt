#lang racket

(provide version>=9.0?)

(require version/utils)

(define (version>=? target-version)
  (not (version<? (version) target-version)))

(define (version>=9.0?)
  (version>=? "9.0"))


