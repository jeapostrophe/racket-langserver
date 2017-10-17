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
     (define id (if ((or/c number? string?) id) id (json-null)))
     (define err "The JSON sent is not a valid request object.")
     (display-message/flush (error-response id INVALID-REQUEST err))]))

(define (report-request-error id method exn)
  (log-error "Caught exn in request ~v\n~a" method (exn->string exn))
  (error-response id INTERNAL-ERROR (format "internal error in method ~v" method)))

(define report-request-error* (curry report-request-error))

;; Processes a request. This procedure should always return a jsexpr
;; which is a suitable response object.
(define (process-request id method params)
  (with-handlers ([exn:fail? (report-request-error* id method)])
    (match method
      ["initialize"
       (log-info "Got initialize message")
       (initialize id params)]
      ["shutdown"
       (log-info "Got shutdown message")
       (shutdown id)]
      #;
      ["client/registerCapability"
       (client/register-capability params)]
      [_
       (log-warning "invalid request for method ~v" method)
       (define err-msg (format "The method ~v was not found" method))
       (error-response id METHOD-NOT-FOUND err-msg)])))

;; Processes a notification. Because notifications do not require
;; a response, this procedure always returns void.
(define (process-notification method params)
  (match method
    ["initialized"
     (log-info "Ignoring initialized message...")]
    ["exit"
     (log-info "Got exit message")
     (exit (if already-shutdown? 0 1))]
    ["textDocument/didOpen"
     (set! open-docs (text-document/did-open open-docs params))]
    ["textDocument/didClose"
     (set! open-docs (text-document/did-close open-docs params))]
    ["textDocument/didChange"
     (set! open-docs (text-document/did-change open-docs params))]))

;;
;; Requests
;;;;;;;;;;;;;

(define (initialize id params)
  (match params
    [(hash-table ['processId (? (or/c number? (json-null)) process-id)]
                 ['capabilities (? jsexpr? capabilities)])
     (set! already-initialized? #t)
     #;
     (define server-capabilities
       (hasheq 'textDocumentSync 2 ;; 2 = incremental
               'hoverProvider #f
               'completionProvider (hasheq 'resolveProvider  #f
                                           'triggerCharacters '())
               'signatureHelpProvider (hasheq 'triggerCharacters '())
               'definitionProvider #f
               'referencesProvider #f
               'documentHighlightProvider #f
               'documentSymbolProvider #f
               'workspaceSymbolProvider #f
               'codeActionProvider #f
               'codeLensProvider (hasheq 'resolveProvider #f)
               'documentFormattingProvider #f
               'documentRangeFormattingProvider #f
               ;'documentOnTypeFormattingProvider (hasheq)
               'renameProvider #f
               'documentLinkProvider (hasheq 'resolveProvider #f)
               'executeCommandProvider (hasheq 'commands '())
               ;'experimental (hasheq)
               ))
     (define server-capabilities (hasheq))
     (success-response id (hasheq 'capabilities server-capabilities))]
    [_
     (error-response id INVALID-PARAMS "initialize failed")]))

(define (shutdown id)
  (set! already-shutdown? #t)
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