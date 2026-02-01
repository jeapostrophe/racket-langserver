#lang racket/base
(provide get-resyntax-enabled
         set-resyntax-enabled!)

(define resyntax-enabled? #t)

(define (get-resyntax-enabled)
  resyntax-enabled?)

(define (set-resyntax-enabled! val)
  (set! resyntax-enabled? val))

