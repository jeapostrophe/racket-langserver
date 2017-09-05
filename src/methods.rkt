#lang racket/base
(require json
         racket/contract/base
         racket/match
         racket/port
         "error-codes.rkt"
         "exns.rkt"
         "responses.rkt")

(define already-initialized? (make-parameter #f))
(define already-shutdown? (make-parameter #f))
(define open-docs (make-weak-hash))

;;
;; Dispatch
;;;;;;;;;;;;;

;; Processes a message. This procedure can return a jsexpr in the case
;; of a single request, a list of jsexprs in the case of a batch request,
;; or void in the case where msg is a response object or notification. 
(define (process-message msg)
  (define (langserver-handler exn)
    (error-response (exn:fail:langserver-id exn)
                    (exn:fail:langserver-code exn)
                    (exn-message exn)))
  (define (fail-handler exn)
    (error-response (json-null) INTERNAL-ERROR (exn-message exn)))
  ;; TODO: this isn't right, only requests should returns responses
  ;; from handlers - notifications should just log the problem and return void.
  (with-handlers ([exn:fail:langserver? langserver-handler]
                  [exn:fail? fail-handler])
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
      [(? (non-empty-listof (and/c hash? jsexpr?)))
       (filter (not/c void?) (map process-message msg))]
      ;; Invalid Message
      [_
       (define id-ref (hash-ref msg 'id void))
       (define id (if ((or/c number? string?) id-ref)
                      id-ref
                      (json-null)))
       (define err-msg "The JSON sent is not a valid request object")
       (error-response id INVALID-REQUEST err-msg)])))

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
    ["textDocument/didOpen"
     (did-open-text-document params)]
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

;;
;; Notifications
;;;;;;;;;;;;;;;;;;

(define (did-open-text-document params)
  (match params
    [(hash-table
      ['textDocument (hash-table ['uri (? string? uri)] ;; TODO: URI isn't exactly a string...
                                 ['languageId (? string? language-id)]
                                 ['version (? number? version)]
                                 ['text (? string? text)])])
     ;; TODO: Filter by language ID? Will we get textDocument/didOpen notifications for
     ;; *all* files, or just racket files?
     ;; ------------------------------------
     ;; TODO: Should the keys in this table contain version information? When will we need
     ;; to distinguish between versions of a document?
     (hash-set! open-docs uri text)]
    [_
     (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [process-message
   (jsexpr? . -> . (or/c jsexpr? void?))]
  [process-request
   ((or/c number? string?) string? jsexpr? . -> . jsexpr?)]
  [process-notification
   (string? jsexpr? . -> . (or/c jsexpr? void?))]))