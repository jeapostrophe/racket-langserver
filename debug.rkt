#lang racket/base
(require racket/file
         racket/runtime-path)

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

(provide
  maybe-debug-log
  maybe-debug-file)
