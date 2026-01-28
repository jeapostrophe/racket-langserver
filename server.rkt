#lang racket/base
(provide current-server
         set-current-server!)

(define current-server #f)
(define (set-current-server! s)
  (set! current-server s))
