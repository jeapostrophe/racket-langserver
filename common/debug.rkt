#lang racket/base
(require racket/file
         racket/runtime-path
         racket/match
         racket/string
         racket/format)

(provide
  maybe-debug-log
  maybe-debug-file
  D
  T)

(define debug? #f)

(define-runtime-path df "debug.out.rkt")
(define (maybe-debug-file t)
  (when debug?
    (display-to-file t df #:exists 'replace)))

(define-runtime-path dp "debug.log")
(define (maybe-debug-log m)
  (when debug?
    (with-output-to-file dp
      #:exists 'append
      (lambda ()
        (writeln m)))))

(define (err-log tag name msg)
  (eprintf "[~a] ~a:\n~a\n" tag name msg))

;; DEBUG macro: evaluates the expression, logs the result, and returns the result.
(define-syntax-rule (D expr)
  (call-with-values
    (lambda () expr)
    (lambda results
      (err-log 'DEBUG (quote expr)
               (match results
                 [(list) (format "void")]
                 [(list x) (~v x)]
                 [_ (string-join (map ~v results) "\n")]))
      (apply values results))))

;; TIME macro: evaluates the expression, logs the time taken, and returns the result.
(define-syntax-rule (T expr)
  (let-values ([(results cpu-time real-time gc-time)
                (time-apply (lambda () expr) '())])
    (err-log 'TIME (quote expr)
             (format "cpu time: ~a real time: ~a gc time: ~a"
                     cpu-time real-time gc-time))
    (apply values results)))

