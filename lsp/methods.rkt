#lang racket/base

(require compiler/module-suffix
         json
         racket/async-channel
         racket/class
         racket/contract/base
         racket/exn
         racket/match
         "../common/debug.rkt"
         "../common/interfaces.rkt"
         "../common/json-util.rkt"
         "../common/path-util.rkt"
         "lsp.rkt"
         "responses.rkt"
         "source-controller.rkt"
         (submod "source-controller.rkt" owner)
         (prefix-in text-document/ "text-document.rkt")
         (prefix-in workspace/ "workspace.rkt")
         "../workspace/current.rkt"
         "../workspace/state.rkt")

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
;; Fast queries read the adjusted projection under its lock. Features needing
;; current analysis wait on `SafeDoc-analysis-completion` or
;; `Analysis-Handle-completion-evt`. The consumer owns acceptance and shares
;; one analysis per revision, with four execution slots.

;; Mutable variables
(define already-initialized? #f)
(define already-shutdown? #f)

;;; Dispatch

(define server%
  (class object%
    (super-new)

    (init-field
      response-channel
      request-channel
      notification-channel
      [source-controller (make-source-controller)])
    (define response-handlers (make-hash))
    (define server-request-id 0)
    (define open-documents (make-hasheq))
    (source-controller-set-notifier! source-controller
                                     (λ (method params) (send-notification method params)))

    (define/public (get-source-controller) source-controller)

    ;; Owner operations run on the consumer; deferred response threads created
    ;; there inherit the session controller and registry.
    (define (call-with-session-params thunk)
      (parameterize ([current-source-controller source-controller]
                     [current-open-docs open-documents])
        (thunk)))

    (define/public (process-source-event event)
      (call-with-session-params
        (λ () (source-controller-handle! source-controller event))))

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

    (define/public (process-message msg)
      (call-with-session-params
        (λ () (process-message/owner msg))))

    (define/private (process-message/owner msg)
      (match msg
        ;; Request
        [(hash-table ['id (? (or/c number? string?) id)]
                     ['method (? string? method)])
         (define params (hash-ref msg 'params hasheq))
         (define response (handle-request id method params))
         (cond
           ;; A procedure is a deferred response; run it off the consumer.
           [(procedure? response)
            (thread (λ () (send-response (response))))]
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
        [(? (non-empty-listof (and/c hash? jsexpr?)) batch)
         (for ([msg (in-list batch)])
           (process-message msg))]
        ;; Invalid Message
        [_
         (define id-ref (hash-ref msg 'id void))
         (define id (if ((or/c number? string?) id-ref) id-ref (json-null)))
         (define err "The JSON sent is not a valid request object.")
         (send-response (error-response id ErrorCode-InvalidRequest err))]))

    ;; (-> (or/c number? string?) string? jsexpr? (or/c jsexpr? (-> jsexpr?)))
    (define/public (handle-request id method params)
      (with-handlers ([exn:fail? (report-request-error id method)])
        (define uri
          (match params
            [(hash-table ['textDocument (hash-table ['uri (? string? uri)])]) uri]
            [_ #f]))
        (cond
          ;; Untitled, git, and notebook URIs use the same textDocument
          ;; requests as files, including when they are not open. They have
          ;; no disk fallback, so fail here instead of InternalError from a
          ;; handler's `lsp-get-doc`.
          [(and uri (not (file-uri? uri)) (not (lsp-get-doc uri #f)))
           (error-response id ErrorCode-RequestFailed "Virtual document is not open")]
          [else
           (match method
             ["initialize"
              (initialize id params)]
             ["shutdown"
              (source-controller-shutdown! source-controller)
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
              (log-racket-langserver-error "invalid request for method ~v" method)
              (define err (format "The method ~v was not found" method))
              (error-response id ErrorCode-MethodNotFound err)])])))

    ;; Handle a notification. Because notifications do not require
    ;; a response, this procedure always returns void.
    (define/public (handle-notification method params)
      (define (request-client method params handler)
        (send-request method params handler))
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
         (workspace/didChangeConfiguration request-client params)]
        ["textDocument/didOpen"
         (text-document/did-open! request-client params)]
        ["textDocument/didClose"
         (text-document/did-close! params)]
        ["textDocument/didChange"
         (text-document/did-change! params)]
        [_ (maybe-debug-log (format "invalid notification for method ~v" method))]))))

(define ((report-request-error id method) exn)
  (log-racket-langserver-error "Caught exn in request ~v\n~a" method (exn->string exn))
  (define err (format "internal error in method ~v" method))
  (error-response id ErrorCode-InternalError err))

;;; Requests

(define (record-client-capabilities! capabilities)
  (workspace/client-capability-workspace/configuration?
    (and (jsexpr-has-key? capabilities '(workspace configuration))
         (jsexpr-ref capabilities '(workspace configuration))))
  (text-document/client-capability-hierarchical-document-symbol?
    (and (jsexpr-has-key? capabilities
                          '(textDocument documentSymbol hierarchicalDocumentSymbolSupport))
         (jsexpr-ref capabilities
                     '(textDocument documentSymbol hierarchicalDocumentSymbolSupport)))))

(define (initialize-workspace-root! root-uri root-path)
  ;; If both `rootPath` and `rootUri` are set, then `rootUri` wins.
  ;; JSON null means no folder is open. A non-file `rootUri` still wins
  ;; and does not fall through to `rootPath`.
  (define folder-path
    (cond
      [(not (equal? root-uri (json-null)))
       (and (file-uri? root-uri) (uri->path root-uri))]
      [(not (equal? root-path (json-null)))
       (string->path root-path)]
      [else #f]))
  (when folder-path
    (workspace-add-folder! current-workspace folder-path)))

(define (server-capabilities-for-client capabilities)
  (define rename-provider
    (match capabilities
      [(hash-table ['textDocument
                    (hash-table ['rename
                                 (hash-table ['prepareSupport #t])])])
       (hasheq 'prepareProvider #t)]
      [_ #t]))
  (define semantic-provider
    (hasheq 'legend (hasheq 'tokenTypes (map ->jsexpr *semantic-token-types*)
                            'tokenModifiers (map ->jsexpr *semantic-token-modifiers*))
            'full #t
            'range #t))
  (define did-rename-filters
    (for/list ([ext (in-list (get-module-suffixes))])
      (hasheq 'scheme "file"
              'pattern (hasheq 'glob (format "**/*.~a" ext)))))
  (hasheq 'textDocumentSync (hasheq 'openClose #t
                                    'change (->jsexpr TextDocumentSyncKind-Incremental)
                                    'willSave #f
                                    'willSaveWaitUntil #f)
          'hoverProvider #t
          'codeActionProvider #t
          'definitionProvider #t
          'referencesProvider #t
          'completionProvider (hasheq 'triggerCharacters (list "("))
          'signatureHelpProvider (hasheq 'triggerCharacters (list " " ")" "]"))
          'inlayHintProvider #t
          'renameProvider rename-provider
          'semanticTokensProvider semantic-provider
          'documentHighlightProvider #t
          'documentSymbolProvider #t
          'documentFormattingProvider #t
          'documentRangeFormattingProvider #t
          'documentOnTypeFormattingProvider
          (hasheq 'firstTriggerCharacter ")"
                  'moreTriggerCharacter (list "\n" "]"))
          'workspace
          (hasheq 'fileOperations
                  (hasheq 'didRename (hasheq 'filters did-rename-filters))
                  'workspaceFolders (hasheq 'changeNotifications #t))))

(define (initialize id params)
  (match params
    [(hash-table ['processId (? (or/c number? (json-null)))]
                 ['capabilities (? jsexpr? capabilities)]
                 ['rootUri (? (or/c string? (json-null)) root-uri)]
                 ['rootPath (? (or/c string? (json-null)) root-path)])
     (record-client-capabilities! capabilities)
     (initialize-workspace-root! root-uri root-path)
     (success-response id (hasheq 'capabilities
                                  (server-capabilities-for-client capabilities)))]
    [_
     (error-response id ErrorCode-InvalidParams "initialize failed")]))

(define (shutdown id)
  (set! already-shutdown? #t)
  (success-response id (json-null)))

(provide server%)
