#lang racket/base
(provide (struct-out Inlay-Hint-Context)
         inlay-hint-source/c)
(require racket/contract
         (only-in racket/class object?)
         "../common/interfaces.rkt"
         (only-in "lexer/token-tree.rkt" Token-Forest?))

(struct/contract Inlay-Hint-Context
  ([text string?]
   [forest Token-Forest?]
   ; check-syntax trace
   [trace object?]
   [abs-pos->pos (-> exact-nonnegative-integer? Pos?)])
  #:transparent)

#|
An inlay hint source is a function such that takes

1. context
2. query range start
3. query range end

returns a list of inlay hints
|#
(define inlay-hint-source/c
  (-> Inlay-Hint-Context?
      exact-nonnegative-integer?
      exact-nonnegative-integer?
      (listof InlayHint?)))
