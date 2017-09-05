#lang racket/base
(require json
         racket/contract/base)

;; Constructor for a response object representing success.
(define (success-response id result)
  (hasheq 'id id 'result result))

;; Constructor for a response object representing failure.
(define (error-response id code message [data (void)])
  (define ht
    (hasheq 'id id
            'code code
            'message message))
  (if (void? data)
      ht
      (hash-set ht 'data data)))

(provide
 (contract-out
  [success-response
   ((or/c number? string?) jsexpr? . -> . jsexpr?)]
  [error-response
   (->* ((or/c number? string? (json-null)) number? string?)
        ((or/c jsexpr? void?))
        jsexpr?)]))