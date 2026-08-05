#lang racket/base

(provide version>=8.8?
         version>=9.0?)

(require version/utils)

(define (version>=? target-version)
  (not (version<? (version) target-version)))

(define (version>=8.8?)
  (version>=? "8.8"))

(define (version>=9.0?)
  (version>=? "9.0"))

