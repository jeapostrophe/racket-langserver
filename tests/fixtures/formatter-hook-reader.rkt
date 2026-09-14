#lang s-exp syntax/module-reader
racket/base

#:info formatter-info

(require racket/class)

(define (range-indent textoid _start-position _end-position)
  (define text
    (send textoid get-text 0 (send textoid last-position)))
  (when (regexp-match? #rx"fail-hook" text)
    (error 'range-indent "fixture hook failure"))
  '((0 ">>>")))

(define (formatter-info key default default-filter)
  (case key
    [(drracket:indentation)
     (lambda (_textoid _position) 3)]
    [(drracket:range-indentation) range-indent]
    [else (default-filter key default)]))
