#lang racket/base

(provide current-deadline-milliseconds deadline-evt)

;; The clock and alarm must use the same time base. Older Racket versions
;; lack the monotonic clock or the two-argument alarm-evt.
(define monotonic-milliseconds
  (and (procedure-arity-includes? alarm-evt 2)
       (dynamic-require 'racket/base 'current-inexact-monotonic-milliseconds
                        (lambda () #f))))

(define current-deadline-milliseconds
  (or monotonic-milliseconds current-inexact-milliseconds))

(define (deadline-evt deadline)
  (if monotonic-milliseconds
      (alarm-evt deadline #t)
      (alarm-evt deadline)))
