#lang racket/base

(require racket/contract)

;; Shared lexer data shapes. Keep this module independent from scanning and
;; token-tree parsing so the rest of the lexer stack can depend on common data
;; without forming cycles.

(struct/contract LexerTokenSpan
  ([start exact-nonnegative-integer?]
   [end exact-nonnegative-integer?]
   [type symbol?])
  #:transparent)

(struct/contract LexerSnapshot
  ([text string?]
   [tokens (vectorof LexerTokenSpan?)])
  #:transparent)

(define (make-lexer-span start end type)
  (and (< start end)
       (LexerTokenSpan start end type)))

(define (span-at spans idx)
  (and (<= 0 idx)
       (< idx (vector-length spans))
       (vector-ref spans idx)))

(provide (struct-out LexerTokenSpan)
         (struct-out LexerSnapshot)
         make-lexer-span
         span-at)
