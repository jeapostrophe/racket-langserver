#lang racket/base
(provide open-docs)

; The shared resource for methods
(define open-docs (make-hasheq))
