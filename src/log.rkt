#lang racket/base
(require racket/port
         syntax/parse/define)

(define current-log-port (make-parameter (open-output-nowhere)))

(define-simple-macro (log! fmt:str args:expr ...)
  (displayln (format fmt args ...) (current-log-port)))

(provide current-log-port
         log!)