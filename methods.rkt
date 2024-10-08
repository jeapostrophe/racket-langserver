#lang racket/base
(require json
         racket/contract/base
         racket/exn
         racket/match
         "error-codes.rkt"
         "msg-io.rkt"
         "responses.rkt"
         "struct.rkt"
         (prefix-in text-document/ "text-document.rkt"))

;; TextDocumentSynKind enumeration
(define TextDocSync-None 0)
(define TextDocSync-Full 1)
(define TextDocSync-Incremental 2)

;; Mutable variables
(define already-initialized? #f)
(define already-shutdown? #f)

;;
;; Dispatch
;;;;;;;;;;;;;

;; Processes a message. This displays any repsonse it generates
;; and should always return void.
(define (process-message msg)
  (match msg
    ;; Request
    [(hash-table ['id (? (or/c number? string?) id)]
                 ['method (? string? method)])
     (define params (hash-ref msg 'params hasheq))
     (define response (process-request id method params))
     ;; the result can be a response or a procedure which returns
     ;; a response. If it's a procedure, then it's expected to run
     ;; concurrently.
     (thread (λ ()
               (display-message/flush
                 (if (procedure? response)
                     (response)
                     response))))
     (void)]
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

(define ((report-request-error id method) exn)
  (eprintf "Caught exn in request ~v\n~a\n" method (exn->string exn))
  (define err (format "internal error in method ~v" method))
  (error-response id INTERNAL-ERROR err))

;; Processes a request. This procedure should always return a jsexpr
;; which is a suitable response object.
;; (-> (or/c integer? string?) string? jsexpr? jsexpr?)
(define (process-request id method params)
  (with-handlers ([exn:fail? (report-request-error id method)])
    (match method
      ["initialize"
       (initialize id params)]
      ["shutdown"
       (shutdown id)]
      ["textDocument/hover"
       (text-document/hover id params)]
      ["textDocument/codeAction"
       (text-document/code-action id params)]
      ["textDocument/completion"
       (text-document/completion id params)]
      ["textDocument/signatureHelp"
       (text-document/signatureHelp id params)]
      ["textDocument/definition"
       (text-document/definition id params)]
      ["textDocument/documentHighlight"
       (text-document/document-highlight id params)]
      ["textDocument/references"
       (text-document/references id params)]
      ["textDocument/documentSymbol"
       (text-document/document-symbol id params)]
      ["textDocument/inlayHint"
       (text-document/inlay-hint id params)]
      ["textDocument/rename"
       (text-document/rename id params)]
      ["textDocument/prepareRename"
       (text-document/prepareRename id params)]
      ["textDocument/formatting"
       (text-document/formatting! id params)]
      ["textDocument/rangeFormatting"
       (text-document/range-formatting! id params)]
      ["textDocument/onTypeFormatting"
       (text-document/on-type-formatting! id params)]
      ["textDocument/semanticTokens/full"
       (text-document/full-semantic-tokens id params)]
      ["textDocument/semanticTokens/range"
       (text-document/range-semantic-tokens id params)]
      [_
       (eprintf "invalid request for method ~v\n" method)
       (define err (format "The method ~v was not found" method))
       (error-response id METHOD-NOT-FOUND err)])))

;; Processes a notification. Because notifications do not require
;; a response, this procedure always returns void.
(define (process-notification method params)
  (match method
    ["exit"
     (exit (if already-shutdown? 0 1))]
    ["textDocument/didOpen"
     (text-document/did-open! params)]
    ["textDocument/didClose"
     (text-document/did-close! params)]
    ["textDocument/didChange"
     (text-document/did-change! params)]
    [_ (void)]))

;;
;; Requests
;;;;;;;;;;;;;

(define (initialize id params)
  (match params
    [(hash-table ['processId (? (or/c number? (json-null)) process-id)]
                 ['capabilities (? jsexpr? capabilities)])
     (define sync-options
       (hasheq 'openClose #t
               'change TextDocSync-Incremental
               'willSave #f
               'willSaveWaitUntil #f))
     (define renameProvider
       (match capabilities
         [(hash-table ['textDocument
                       (hash-table ['rename
                                    (hash-table ['prepareSupport #t])])])
          (hasheq 'prepareProvider #t)]
         [_ #t]))
     (define semantic-provider
       (hasheq 'legend (hasheq 'tokenTypes (map symbol->string *semantic-token-types*)
                               'tokenModifiers (map symbol->string *semantic-token-modifiers*))
               'full #t
               'range #t))
     (define server-capabilities
       (hasheq 'textDocumentSync sync-options
               'hoverProvider #t
               'codeActionProvider #t
               'definitionProvider #t
               'referencesProvider #t
               'completionProvider (hasheq 'triggerCharacters (list "("))
               'signatureHelpProvider (hasheq 'triggerCharacters (list " " ")" "]"))
               'inlayHintProvider #t
               'renameProvider renameProvider
               'semanticTokensProvider semantic-provider
               'documentHighlightProvider #t
               'documentSymbolProvider #t
               'documentFormattingProvider #t
               'documentRangeFormattingProvider #t
               'documentOnTypeFormattingProvider (hasheq 'firstTriggerCharacter ")" 'moreTriggerCharacter (list "\n" "]"))))

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
   (jsexpr? . -> . void?)]))

