#lang racket/base

;; internal structs ;;

(require racket/contract)

(provide
 (contract-out
  [struct Decl ([filename any/c]
                [id any/c]
                [left exact-nonnegative-integer?]
                [right exact-nonnegative-integer?])]))

(struct Decl (filename id left right) #:transparent)

;; uinteger ;;

(define uinteger-upper-limit (sub1 (expt 2 31)))

(define (uinteger? x)
  (and (integer? x) (<= 0 x uinteger-upper-limit)))

(provide uinteger?)

;; Position ;;

(struct Position
  (line character))

(define/contract (make-Position #:line line #:character char)
  (-> #:line uinteger? #:character uinteger? Position?)

  (Position line char))

(define/contract (Position->hash pos)
  (-> Position? hash?)

  (hash 'line (Position-line pos)
        'character (Position-character pos)))

(provide Position
         make-Position
         Position->hash)

;; Range ;;

;; `*Range` is a workaround for existing `Range` json expander currently
(struct *Range
  (start end))

(define/contract (make-Range #:start start #:end end)
  (-> #:start Position? #:end Position? *Range?)

  (*Range start end))

(define/contract (Range->hash range)
  (-> *Range? hash?)

  (hash 'start (Position->hash (*Range-start range))
        'end (Position->hash (*Range-end range))))

(provide *Range
         make-Range
         Range->hash)
