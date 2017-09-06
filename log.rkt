#lang racket/base
(require racket/port
         syntax/parse/define)

(define (log-port-guard arg)
  (unless (output-port? arg)
    (raise-argument-error 'current-log-port "output-port?" arg))
  arg)

(define current-log-port (make-parameter (open-output-nowhere) log-port-guard))

(define-simple-macro (with-log-port p b ...+)
  (parameterize ([current-log-port p])
    b ...))

(define-simple-macro (log! fmt:str args:expr ...)
  (displayln (format fmt args ...) (current-log-port)))

(provide current-log-port
         with-log-port
         log!)