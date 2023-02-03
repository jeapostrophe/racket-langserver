#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template)
         racket/match
         syntax/parse)

(define-syntax (define-json-expander stx)
  (syntax-parse stx
    [(_ name:id [key:id ctc:expr] ...+)
     (with-syntax ([(key_ ...) (generate-temporaries #'(key ...))]
                   [(keyword ...)
                    (for/list ([k (syntax->datum #'(key ...))])
                      (string->keyword (symbol->string k)))]
                   [??-id (quote-syntax ??)])
       (syntax/loc stx
         (define-match-expander name
           (λ (stx)
             (syntax-parse stx
               [(_ (~optional (~seq keyword key_)) ...)
                (quasitemplate/loc stx (hash-table (??-id ['key (? ctc key_)]) ...))]))
           (λ (stx)
             (syntax-parse stx
               [(_ (~optional (~seq keyword key_)) ...)
                (syntax/loc stx
                  (make-hasheq (list (cons 'key key_) ...)))])))))]))

(define (jsexpr-has-key? jsexpr keys)
  (cond [(null? keys) #t]
        [else (and (hash-has-key? jsexpr (car keys))
                   (jsexpr-has-key? (hash-ref jsexpr (car keys)) (cdr keys)))]))

(define (jsexpr-ref jsexpr keys)
  (cond [(null? keys) jsexpr]
        [else (jsexpr-ref (hash-ref jsexpr (car keys)) (cdr keys))]))

(define (jsexpr-set jsexpr keys v)
  (cond [(null? keys) jsexpr]
        [(null? (cdr keys)) (hash-set jsexpr (car keys) v)]
        [else (hash-set jsexpr (car keys)
                        (jsexpr-set (hash-ref jsexpr (car keys)) (cdr keys) v))]))

(define (jsexpr-remove jsexpr keys)
  (cond [(null? keys) jsexpr]
        [(null? (cdr keys)) (hash-remove jsexpr (car keys))]
        [else (hash-set jsexpr (car keys)
                        (jsexpr-remove (hash-ref jsexpr (car keys)) (cdr keys)))]))

(provide define-json-expander
         jsexpr-has-key?
         jsexpr-ref
         jsexpr-set
         jsexpr-remove)
