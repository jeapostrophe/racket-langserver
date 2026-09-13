#lang s-exp syntax/module-reader
racket/base

#:info formatter-info

(define (formatter-info key default default-filter)
  (default-filter key default))
