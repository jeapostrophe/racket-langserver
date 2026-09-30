#lang racket/base
(require racket/file
         racket/runtime-path
         racket/match
         racket/string
         racket/format)

(provide
  racket-langserver-logger
  log-racket-langserver-error
  log-racket-langserver-info
  maybe-debug-log
  maybe-debug-file
  D
  T)

(define debug? #f)

(define-logger racket-langserver)

(define-runtime-path df "debug.out.rkt")
(define (maybe-debug-file t)
  (when debug?
    (display-to-file t df #:exists 'replace)))

(define (maybe-debug-log m)
  (log-racket-langserver-debug "~s" m))

(define (err-log tag name msg)
  (log-racket-langserver-debug "[~a] ~a:\n~a" tag name msg))

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
