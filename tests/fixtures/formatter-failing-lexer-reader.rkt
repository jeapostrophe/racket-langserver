#lang s-exp syntax/module-reader
racket/base

#:info formatter-info

(define (failing-lexer _input)
  (error 'failing-lexer "fixture lexer failure"))

(define (range-indent _textoid _start-position _end-position)
  '((0 "must-not-run")))

(define (formatter-info key default default-filter)
  (case key
    [(color-lexer) failing-lexer]
    [(drracket:range-indentation) range-indent]
    [else (default-filter key default)]))
