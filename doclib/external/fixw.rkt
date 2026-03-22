#lang racket/base

(provide get-formatted-lines)

(require fixw/fixw
         fixw/cli)

(define (get-formatted-lines text src-dir #:interactive? [interactive? #f])
  (define config
    (if src-dir
        (read-config/rec src-dir)
        #f))
  (fixw/lines (open-input-string text) config #:interactive? interactive?))

