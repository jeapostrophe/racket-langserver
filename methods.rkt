#lang racket/base
(require json
         racket/contract/base
         racket/exn
         racket/function
         racket/match
         "error-codes.rkt"
         "msg-io.rkt"
         "responses.rkt"
         (prefix-in client/ "client.rkt")
         (prefix-in text-document/ "text-document.rkt"))

(define already-initialized? #f)
(define already-shutdown? #f)
(define open-docs (hasheq))

(require syntax/parse/define)

(define-simple-macro (init-guard body ...+)
  (cond [(not already-initialized?)
         (error-response (json-null) SERVER-NOT-INITIALIZED
                         "The server has not been initialized")]
        [already-shutdown?
         (error "already shutdown")]
        [else body ...]))

;;
;; Dispatch
;;;;;;;;;;;;;

;; Processes a message. This displays any repsonse it generates
;; and should always return void.
(define (process-message msg)
  (match msg
    ;; Request
    [(hash-table ['id (? (or/c number? string?) id)]
                 ['method (? string? method)]
                 ['params (? jsexpr? params)])
     (define response (process-request id method params))
     (display-message/flush response)]
    ;; Notification
    [(hash-table ['method (? string? method)])
     (define params (hash-ref msg 'params hasheq))
     (process-notification method params)]
    ;; Batch Request
    [(? (non-empty-listof (and/c hash? jsexpr?)))
     (for-each process-message msg)]
    ;; Invalid Message
    [_
     (define id-ref (hash-ref msg 'id void))
     (define id (if ((or/c number? string?) id-ref) id-ref (json-null)))
     (define err "The JSON sent is not a valid request object.")
     (display-message/flush (error-response id INVALID-REQUEST err))]))

(define (report-request-error id method exn)
  ;; TODO: get rid of string alloc from exn->string
  (eprintf "Caught exn in request ~v\n~a\n" method (exn->string exn))
  (define err (format "internal error in method ~v" method))
  (error-response id INTERNAL-ERROR err))

(define report-request-error* (curry report-request-error))

;; Processes a request. This procedure should always return a jsexpr
;; which is a suitable response object.
(define (process-request id method params)
  (with-handlers ([exn:fail? (report-request-error* id method)])
    (match method
      ["initialize"
       (eprintf "Got initialize message\n")
       (initialize id params)]
      ["shutdown"
       (eprintf "Got shutdown message\n")
       (shutdown id)]
      #;
      ["client/registerCapability"
       (client/register-capability params)]
      [_
       (eprintf "invalid request for method ~v\n" method)
       (define err (format "The method ~v was not found" method))
       (error-response id METHOD-NOT-FOUND err)])))

;; Processes a notification. Because notifications do not require
;; a response, this procedure always returns void.
(define (process-notification method params)
  (match method
    ["exit"
     (eprintf "Got exit message\n")
     (exit (if already-shutdown? 0 1))]
    ["textDocument/didOpen"
     (set! open-docs (text-document/did-open open-docs params))]
    ["textDocument/didClose"
     (set! open-docs (text-document/did-close open-docs params))]
    ["textDocument/didChange"
     (set! open-docs (text-document/did-change open-docs params))]
    [_
     (eprintf "Ignoring notification ~v\n" method)]))

;;
;; Requests
;;;;;;;;;;;;;

(define (initialize id params)
  (match params
    [(hash-table ['processId (? (or/c number? (json-null)) process-id)]
                 ['capabilities (? jsexpr? capabilities)])
     (define sync-options
       (hasheq 'openClose #t
               'change 2 ;; 2 = incremental
               'willSave #f
               'willSaveWaitUntil #f))
     (define completion-provider
       (hasheq 'resolveProvider #t
               'triggerCharacters '(")" "]" "}")))
     (define server-capabilities
       (hasheq 'textDocumentSync sync-options
               'completionProvider completion-provider))
     (define resp (success-response id (hasheq 'capabilities server-capabilities)))
     (set! already-initialized? #t)
     resp]
    [_
     (error-response id INVALID-PARAMS "initialize failed")]))

(define (shutdown id)
  (set! already-shutdown? #t)
  (success-response id (json-null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [process-message
   (jsexpr? . -> . void?)]
  [process-request
   ((or/c number? string?) string? jsexpr? . -> . jsexpr?)]
  [process-notification
   (string? jsexpr? . -> . void?)]))
