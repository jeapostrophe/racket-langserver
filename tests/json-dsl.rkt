#lang racket/base

;; Shape-tagged JSON literal DSL for untyped test payloads:
;;   {#:k v ...}   -> (hasheq 'k (json v) ...)
;;   {"k" v ...}   -> (hasheq 'k (json v) ...)
;;   {,k-expr v}   -> (hasheq k-expr (json v))
;;   [e ...]       -> (list (json e) ...)
;;   null          -> (json-null)
;;   expr          -> evaluated Racket expression
(provide json)

(require (for-syntax racket/base
                     syntax/parse)
         json)

(begin-for-syntax
  (define (paren-shape? stx shape)
    (eqv? (syntax-property stx 'paren-shape) shape))

  (define-splicing-syntax-class json-pair
    (pattern (~seq k:keyword v:expr)
             #:with key #`(quote #,(datum->syntax #'k (string->symbol (keyword->string (syntax-e #'k)))))
             #:with val #'(json v))
    (pattern (~seq k:str v:expr)
             #:with key #`(quote #,(datum->syntax #'k (string->symbol (syntax-e #'k))))
             #:with val #'(json v))
    (pattern (~seq ((~literal unquote) k:expr) v:expr)
             #:with key #'k
             #:with val #'(json v))))

(define-syntax (json stx)
  (syntax-parse stx
    [(_ (~and obj (pair:json-pair ...)))
     #:when (paren-shape? #'obj #\{)
     #'(hasheq (~@ pair.key pair.val) ...)]
    [(_ (~and arr (elem ...)))
     #:when (paren-shape? #'arr #\[)
     #'(list (json elem) ...)]
    [(_ (~literal null))
     #'(json-null)]
    [(_ expr)
     #'expr]))

(module+ test
  (require rackunit)

  (test-case "json builds nested objects, arrays, null, computed keys, and expressions"
    (define uri "file:///example.rkt")
    (define key 'dynamicKey)
    (check-equal?
      (json {#:uri uri
             #:empty []
             #:nothing null
             "a/b" 1
             ,key [{#:flag #t #:count (+ 1 2)}]})
      (hasheq 'uri uri
              'empty '()
              'nothing (json-null)
              'a/b 1
              'dynamicKey (list (hasheq 'flag #t 'count 3)))))

  (test-case "json ignores ordinary parenthesized Racket expressions"
    (check-equal? (json {#:sum (+ 2 3) #:items (list 1 2)})
                  (hasheq 'sum 5 'items (list 1 2)))))
