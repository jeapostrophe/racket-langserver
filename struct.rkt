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
  (tab-size
   insert-spaces
   trim-trailing-whitespace
   insert-final-newline
   trim-final-newlines
   key))

(define/contract (make-FormattingOptions #:tab-size tab-size
                                         #:insert-spaces insert-spaces
                                         #:trim-trailing-whitespace [trim-trailing-whitespace undef-object]
                                         #:insert-final-newline [insert-final-newline undef-object]
                                         #:trim-final-newlines [trim-final-newlines undef-object]
                                         #:key [key undef-object])
  (-> #:tab-size uinteger?
      #:insert-spaces boolean?
      #:trim-trailing-whitespace (undef/c boolean?)
      #:insert-final-newline (undef/c boolean?)
      #:trim-final-newlines (undef/c boolean?)
      #:key (undef/c hash?)
      FormattingOptions?)

  (FormattingOptions tab-size
                     insert-spaces
                     trim-trailing-whitespace
                     insert-final-newline
                     trim-final-newlines
                     key))

(define (jsexpr->FormattingOptions jsexpr)
  (with-handlers ([exn:fail? (λ (_) request-err-object)])
    (make-FormattingOptions #:tab-size (hash-ref jsexpr 'tabSize)
                            #:insert-spaces (hash-ref jsexpr 'insertSpaces)
                            #:trim-trailing-whitespace (hash-ref jsexpr 'trimTrailingWhitespace undef-object)
                            #:insert-final-newline (hash-ref jsexpr 'insertFinalNewline undef-object)
                            #:trim-final-newlines (hash-ref jsexpr 'trimFinalNewlines undef-object)
                            #:key (hash-ref jsexpr 'key undef-object))))

(struct SemanticToken
  (start end type modifiers)
  #:transparent)

(define *semantic-token-types*
  '(variable
     function
     string
     number
     regexp))

(define *semantic-token-modifiers*
  '(definition))

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
         FormattingOptions-tab-size
         FormattingOptions-trim-trailing-whitespace
         as-FormattingOptions
         (struct-out SemanticToken)
         *semantic-token-types*
         *semantic-token-modifiers*)
