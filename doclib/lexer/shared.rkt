#lang racket/base

(require racket/contract
         racket/match
         racket/string)

(struct/contract LexerTokenSpan
  ([start exact-nonnegative-integer?]
   [end exact-nonnegative-integer?]
   [type symbol?])
  #:transparent)

(define (make-lexer-span start end type)
  (and (< start end)
       (LexerTokenSpan start end type)))

(define (lang-directive? txt)
  (and (string? txt)
       (string-prefix? txt "#lang ")))

;; Normalize tokens types to more meaningful.
(define (normalize-token type text)
  (match* (type text)
    [('parenthesis (or "(" "[" "{"))
     'open-paren]
    [('parenthesis (or ")" "]" "}"))
     'close-paren]
    [(_ "'") 'quote]
    [(_ "`") 'quasiquote]
    [(_ ",") 'unquote]
    [(_ "#;") 'sexp-comment]
    [(_ "#'") 'syntax-quote]
    [(_ "#`") 'syntax-quasiquote]
    [(_ "#,@") 'syntax-unquote-splicing]
    [(_ "#,") 'syntax-unquote]
    [(_ ",@") 'unquote-splicing]
    [(_ "#reader") 'reader-directive]
    ;; lexer uses `read-language` to detect lang directives.
    ;; Correct lang line are assigned `other` type, incorrect ones are assigned `error` type.
    ;; We only handle correct ones here.
    [('other (? lang-directive?)) 'lang-directive]
    [(_ _) type]))

(define (span-at spans idx)
  (and (<= 0 idx)
       (< idx (vector-length spans))
       (vector-ref spans idx)))

(provide (struct-out LexerTokenSpan)
         make-lexer-span
         normalize-token
         span-at)
