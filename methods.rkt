#lang racket/base
(require json
         compiler/module-suffix
         racket/contract/base
         racket/exn
         racket/match
         racket/class
         racket/async-channel
         "error-codes.rkt"
         "interfaces.rkt"
         "json-util.rkt"
         "responses.rkt"
         "struct.rkt"
         (prefix-in workspace/ "workspace.rkt")
         (prefix-in text-document/ "text-document.rkt"))

;; Process a request or an notification.
;;
;; A request asks for a result. A notification can optionally
;; send some information to the client.
;;
;; A process can return result then send to client, or return
;; a procedure that is a async task and is expected to run in
;; a new thread.
;;
;; Most requests and notifications can be simplified into two types:
;;
;; Text change: for example, document change notification
;; Queries: Read only requests
;;
;; For a text change, we do a fast and simple adjustment synchronously to the data in
;; build-trace% which is defined in `doc-trace.rkt`. This fast adjustment is useful for
;; fast response, but can be incorrect. So we also runs an async task (check syntax)
;; to update the build-trace%, which can be very slow and sometimes failed, but it
;; is correct.
;;
;; For a query, if it's not important to use the latest data, we just run it synchronously
;; and use the fast adjusted build-trace%. Otherwise, it would be an async task. We choose
;; to run it right before the next text change event or after the current text corresponding
;; check syntax task completes.
;;
;; One important point is that check syntax task is computationally intensive, so don't run
;; too much. The current strategy is an newer check syntax task always replace
;; the old running task. So for any document, at most one check syntax task is running
;; at any time.

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

(define server%
  (class object%
    (super-new)

    (init-field
      response-channel
      request-channel
      notification-channel)
    (field
      ; Each request sent by server should register its response handler here
      [response-handlers (make-hash)]
      [server-request-id 0])

    (define/public (send-response msg)
      (async-channel-put response-channel msg))

    (define/public (send-request method params handler)
      ; register handler
      (hash-set! response-handlers server-request-id handler)
      ; send request to LSP client
      (async-channel-put request-channel
                         (hasheq 'id server-request-id
                                 'method method
                                 'params params))
      (set! server-request-id (add1 server-request-id)))

    (define/public (send-notification method params)
      (async-channel-put notification-channel
                         (hasheq 'jsonrpc "2.0"
                                 'method method
                                 'params params)))

    ;; Processes a message (a JSON). This displays any repsonse it generates
    ;; and should always return void.
    (define/public (process-message msg)
      (match msg
        ;; Request
        [(hash-table ['id (? (or/c number? string?) id)]
                     ['method (? string? method)])
         (define params (hash-ref msg 'params hasheq))
         (define response (handle-request id method params))
         (cond
           ;; result is a procedure which returns a response, hence we put it into a thread to postpone it
           [(procedure? response)
            (thread (λ ()
                      (send-response (response))))]
           ;; otherwise we just push response into the response channel
           [else (send-response response)])]
        ;; Notification
        [(hash-table ['method (? string? method)])
         (define params (hash-ref msg 'params hasheq))
         (handle-notification method params)]
        ;; Response
        [(hash-table ['jsonrpc "2.0"]
                     ['id id]
                     ['result result])
         (define handler (hash-ref response-handlers id))
         (handler result)
         (hash-remove! response-handlers id)]
        ;; Batch Request
        [(? (non-empty-listof (and/c hash? jsexpr?)))
         (for-each (λ (msg) (process-message msg)) msg)]
        ;; Invalid Message
        [_
         (define id-ref (hash-ref msg 'id void))
         (define id (if ((or/c number? string?) id-ref) id-ref (json-null)))
         (define err "The JSON sent is not a valid request object.")
         (send-response (error-response id INVALID-REQUEST err))]))

    ;; Handle a request. This procedure should always return a jsexpr
    ;; which is a suitable response object.
    ;; (-> (or/c integer? string?) string? jsexpr? jsexpr?)
    (define/public (handle-request id method params)
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

    ;; Handle a notification. Because notifications do not require
    ;; a response, this procedure always returns void.
    (define/public (handle-notification method params)
      (match method
        ["exit"
         (exit (if already-shutdown? 0 1))]
        ["workspace/didRenameFiles"
         (workspace/didRenameFiles params)]
        ["workspace/didChangeWorkspaceFolders"
         (workspace/didChangeWorkspaceFolders params)]
        ["workspace/didChangeWatchedFiles"
         (workspace/didChangeWatchedFiles params)]
        ["workspace/didChangeConfiguration"
         (workspace/didChangeConfiguration params)]
        ["textDocument/didOpen"
         (text-document/did-open! (λ (method params handler)
                                    (send-request method params handler))
                                  (λ (method params)
                                    (send-notification method params))
                                  params)]
        ["textDocument/didClose"
         (text-document/did-close! params)]
        ["textDocument/didChange"
         (text-document/did-change! (λ (method params)
                                      (send-notification method params))
                                    params)]
        [_ (void)]))
    ))

(define ((report-request-error id method) exn)
  (eprintf "Caught exn in request ~v\n~a\n" method (exn->string exn))
  (define err (format "internal error in method ~v" method))
  (error-response id INTERNAL-ERROR err))

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
       (hasheq 'legend (hasheq 'tokenTypes (map ->jsexpr semantic-token-types)
                               'tokenModifiers (map ->jsexpr semantic-token-modifiers))
               'full #t
               'range #t))

     (text-document/client-capability-workspace/configuration?
       (if (jsexpr-has-key? capabilities '(workspace configuration))
           (jsexpr-ref capabilities '(workspace configuration))
           #f))

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
               'documentOnTypeFormattingProvider (hasheq 'firstTriggerCharacter ")" 'moreTriggerCharacter (list "\n" "]"))
               'workspace
               (hasheq 'fileOperations
                       (hasheq 'didRename ; workspace.fileOperations.didRename
                               (hasheq 'filters
                                       (map (lambda (ext)
                                              (hasheq 'scheme "file" 'pattern (hasheq 'glob (format "**/*.~a" ext))))
                                            (get-module-suffixes))))
                       'workspaceFolders (hasheq 'changeNotifications #t))))

     (define resp (success-response id (hasheq 'capabilities server-capabilities)))
     (set! already-initialized? #t)
     resp]
    [_
     (error-response id INVALID-PARAMS "initialize failed")]))

(define (shutdown id)
  (set! already-shutdown? #t)
  (success-response id (json-null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide server%)

