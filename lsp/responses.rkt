#lang racket/base
(require json
         racket/contract/base
         "../common/interfaces.rkt"
         "../common/json-util.rkt")

(define not-given (gensym 'not-given))

;; Constructor for a response object representing success.
(define (success-response id result)
  (hasheq 'jsonrpc "2.0"
          'id id
          'result result))

;; Encode protocol values, treating an absent result as JSON null.
(define (success/enc id result)
  (success-response id (if result (->jsexpr result) (json-null))))

;; Constructor for a response object representing failure.
(define (error-response id code message [data not-given])
  (define err (hasheq 'code (->jsexpr code)
                      'message message))
  (define err* (if (eq? data not-given)
                   err
                   (hash-set err 'data data)))
  (hasheq 'jsonrpc "2.0"
          'id id
          'error err*))

(provide
  (contract-out
    [success/enc
     ((or/c number? string?) any/c . -> . jsexpr?)]
    [success-response
     ((or/c number? string?) jsexpr? . -> . jsexpr?)]
    [error-response
     (->* ((or/c number? string? (json-null)) (or/c number? ErrorCode?) string?)
          (any/c)
          jsexpr?)]))

