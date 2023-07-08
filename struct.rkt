#lang racket/base

;; internal structs ;;

(require racket/contract/base)

(provide
  (contract-out
[struct Decl ([filename any/c]
                        [id any/c]
                        [left exact-nonnegative-integer?]
                        [right exact-nonnegative-integer?])]))

(struct Decl (filename id left right) #:transparent)
