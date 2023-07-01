#lang racket/base

(define-syntax-rule (report-time expr)
  (let-values ([(result t real-t gc-t) (time-apply (λ () expr) '())])
    (eprintf "~as: ~a~ncpu time: ~v real time: ~v gc time: ~v~n" (current-seconds) (quote expr) t real-t gc-t)
    (car result)))

(provide report-time)
