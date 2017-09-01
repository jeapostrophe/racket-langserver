#lang racket/base
(require json
         racket/contract/base
         racket/match
         "error-codes.rkt"
         "responses.rkt")

(define already-initialized? (make-parameter #f))
(define already-shutdown? (make-parameter #f))

;;
;; Dispatch
;;;;;;;;;;;;;

;; Processes a message. This procedure can return a jsexpr in the case
;; of a single request, a list of jsexprs in the case of a batch request,
;; or void in the case where msg is a response object or notification. 
(define (process-message msg)
  (match msg
    ;; Request
    [(hash-table ['id (? (or/c number? string?) id)]
                 ['method (? string? method)]
                 ['params (? jsexpr? params)])
     (process-request id method params)]
    ;; Notification
    [(hash-table ['method (? string? method)])
     (define params (hash-ref msg 'params hasheq))
     (process-notification method params)]
    ;; Batch Request
    [(? (non-empty-listof (and/c (not/c list?) jsexpr?)))
     (filter (not/c void?) (map process-message msg))]
    ;; Invalid Message
    [_
     (define id-ref (hash-ref msg 'id void))
     (define id (if ((or/c number? string?) id-ref)
                    id-ref
                    (json-null)))
     (define err-msg "The JSON sent is not a valid request object")
     (error-response id INVALID-REQUEST err-msg)]))

;; Processes a request. This procedure should always return a jsexpr
;; which is a suitable response object.
(define (process-request id method params)
  (match method
    ["initialize"
     (initialize id params)]
    ["shutdown"
     (shutdown id)]
    [_
     (define err-msg (format "The method ~v was not found" method))
     (error-response id METHOD-NOT-FOUND err-msg)]))

;; Processes a notification. Because notifications do not require
;; a response, this procedure always returns void.
(define (process-notification method params)
  (match method
    ["exit"
     (exit (if (already-shutdown?) 0 1))]
    [_
     (void)]))

;;
;; Requests
;;;;;;;;;;;;;

(define (initialize id params)
  (match params
    [(hash-table ['processId (? (or/c number? (json-null)) process-id)]
                 ['capabilities (? jsexpr? capabilities)])
     (already-initialized? #t)
     (define result (hasheq))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "initialize failed")]))

(define (shutdown id)
  (already-shutdown? #t)
  (success-response id (json-null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [process-message
   (jsexpr? . -> . (or/c jsexpr? void?))]
  [process-request
   ((or/c number? string?) string? jsexpr? . -> . jsexpr?)]
  [process-notification
   (string? jsexpr? . -> . (or/c jsexpr? void?))]))