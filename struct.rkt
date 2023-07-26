#lang racket/base

;; internal structs ;;

(require racket/contract
         racket/match
         (for-syntax racket/base
                     syntax/parse))

(provide
 (contract-out
  [struct Decl ([filename any/c]
                [id any/c]
                [left exact-nonnegative-integer?]
                [right exact-nonnegative-integer?])]))

(struct Decl (filename id left right) #:transparent)

;; optional arguments

(define undef-object (gensym 'undef))

(define (undef? x)
  (eq? x undef-object))

(define (undef/c pred?)
  (λ (x)
    (or/c (undef? x) (pred? x))))

(provide undef?
         undef/c
         undef-object)

;; request error

(define request-err-object (gensym 'request-error))

(define (request-err? x)
  (eq? x request-err-object))

(provide request-err-object
         request-err?)

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

(struct FormattingOptions
  (tabSize
   insertSpaces
   trimTrailingWhitespace
   insertFinalNewline
   trimFinalNewlines
   key))

(define/contract (make-FormattingOptions #:tabSize tabSize
                                         #:insertSpaces insertSpaces
                                         #:trimTrailingWhitespace [trimTrailingWhitespace undef-object]
                                         #:insertFinalNewline [insertFinalNewline undef-object]
                                         #:trimFinalNewlines [trimFinalNewlines undef-object]
                                         #:key [key undef-object])
  (-> #:tabSize uinteger?
      #:insertSpaces boolean?
      #:trimTrailingWhitespace (undef/c boolean?)
      #:insertFinalNewline (undef/c boolean?)
      #:trimFinalNewlines (undef/c boolean?)
      #:key (undef/c hash?)
      FormattingOptions?)

  (FormattingOptions tabSize
                     insertSpaces
                     trimTrailingWhitespace
                     insertFinalNewline
                     trimFinalNewlines
                     key))

(define (jsexpr->FormattingOptions jsexpr)
  (with-handlers ([exn:fail? (λ (_) request-err-object)])
    (make-FormattingOptions #:tabSize (hash-ref jsexpr 'tabSize)
                            #:insertSpaces (hash-ref jsexpr 'insertSpaces)
                            #:trimTrailingWhitespace (hash-ref jsexpr 'trimTrailingWhitespace undef-object)
                            #:insertFinalNewline (hash-ref jsexpr 'insertFinalNewline undef-object)
                            #:trimFinalNewlines (hash-ref jsexpr 'trimFinalNewlines undef-object)
                            #:key (hash-ref jsexpr 'key undef-object))))

;; usage:
;; (jsexpr? jsexpr) ;; #t
;; (match jsexpr
;;   [(as-FormattingOptions opts)
;;    (FormattingOptions? opts) ;; #t
;;    ])
;; It's a convenient macro to verify jsexpr and convert it
;; to struct.
(define-match-expander as-FormattingOptions
  (lambda (stx)
    (syntax-parse stx
      [(_ name)
       #'(and (? hash?)
              (app jsexpr->FormattingOptions (and name (not (? request-err?)))))])))

(provide FormattingOptions
         FormattingOptions-tabSize
         FormattingOptions-trimTrailingWhitespace
         as-FormattingOptions)
