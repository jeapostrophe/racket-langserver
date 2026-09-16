#lang racket/base
(provide didRenameFiles
         didChangeWorkspaceFolders
         didChangeWatchedFiles
         didChangeConfiguration
         update-configuration
         fetch-configuration
         client-capability-workspace/configuration?)
(require compiler/module-suffix
         json
         racket/match)
(require "../common/json-util.rkt"
         "../common/path-util.rkt"
         "../common/interfaces.rkt"
         "lsp.rkt"
         "safedoc.rkt"
         "../doclib/doc.rkt"
         "scheduler.rkt"
         "../common/settings.rkt"
         "../workspace/current.rkt"
         "../workspace/state.rkt")

(define (republish-open-doc-contributions!)
  (lsp-for-each-open-doc
    (lambda (safe-doc)
      (define contribution
        (with-read-doc safe-doc
          (lambda (doc)
            (Doc-contribution doc))))
      (when contribution
        (workspace-set-contribution! current-workspace contribution)))))

(define (didRenameFiles params)
  (match-define (^RenameFilesParams #:files files) params)
  (for ([f files])
    (match-define (FileRename #:oldUri old-uri #:newUri new-uri) f)
    (workspace-remove-path! current-workspace (uri->path old-uri))

    ; remove all awaiting internal queries about `old-uri`
    (define safe-doc (lsp-get-doc old-uri #f))


    ; `safe-doc = #f` should be rarely happened.
    ; we simply give up to handle it, let's trust LSP client will send
    ; other request about analysis this file.
    (when safe-doc
      (lsp-close-doc! old-uri))

    (when (and safe-doc (regexp-match (get-module-suffix-regexp) new-uri))
      (define-values (old-text old-version)
        (with-read-doc safe-doc
          (lambda (doc)
            (values (doc-get-text doc) (Doc-version doc)))))
      (lsp-open-doc! new-uri old-text old-version))))

(define (didChangeWorkspaceFolders params)
  (match-define (^DidChangeWorkspaceFoldersParams #:event event) params)
  (match-define (WorkspaceFoldersChangeEvent #:added added #:removed removed) event)
  (for ([f added])
    (match-define (WorkspaceFolder #:uri uri #:name _) f)
    (workspace-add-folder! current-workspace (uri->path uri)))
  (for ([f removed])
    (match-define (WorkspaceFolder #:uri uri #:name _) f)
    (workspace-remove-folder! current-workspace (uri->path uri)))
  (when (pair? added)
    (republish-open-doc-contributions!)))

(define (didChangeWatchedFiles params)
  (match-define (^DidChangeWatchedFilesParams #:changes changes) params)
  (for ([change changes])
    (match-define (FileEvent #:uri uri #:type type) change)
    (match (FileChangeType-v type)
      ['created (handle-file-created uri)]
      ['changed (handle-file-changed uri)]
      ['deleted (handle-file-deleted uri)]
      [_ (eprintf "Invalid file event type: ~a~n" type)])))

(define (handle-file-created uri)
  (when (regexp-match (get-module-suffix-regexp) uri)
    (lsp-open-doc! uri "" 0)))

(define (handle-file-changed uri)
  (when (regexp-match (get-module-suffix-regexp) uri)
    (let ([safe-doc (lsp-get-doc uri #f)])
      (when safe-doc
        (clear-old-queries/doc-close (SafeDoc-token safe-doc))))))

(define (handle-file-deleted uri)
  (workspace-remove-path! current-workspace (uri->path uri))
  (when (regexp-match (get-module-suffix-regexp) uri)
    (lsp-close-doc! uri)))

(define (apply-langserver-settings settings)
  (match-define (Langserver-Settings #:resyntax resyntax #:formatting formatting)
    settings)
  (match resyntax
    [(Resyntax-Settings #:enable (and enable (not (? Nothing?))))
     (set-resyntax-enabled! enable)]
    [_ (set-resyntax-enabled! default-resyntax-enabled)])
  (match formatting
    [(Formatting-Configuration
       #:document-formatter document-formatter
       #:indentation-formatter indentation-formatter
       #:fmt-settings fmt-settings)
     (set-formatting-settings!
       (Formatting-Settings
         (if (Nothing? document-formatter)
             (Formatting-Settings-document-formatter default-formatting-settings)
             (Document-Formatter-v document-formatter))
         (if (Nothing? indentation-formatter)
             (Formatting-Settings-indentation-formatter default-formatting-settings)
             (Indentation-Formatter-v indentation-formatter))
         (if (Nothing? fmt-settings)
             (Formatting-Settings-fmt-settings default-formatting-settings)
             fmt-settings)))]
    [_ (set-formatting-settings! default-formatting-settings)]))

;; A `racket-langserver` section is a snapshot. Omitted keys use shipped
;; defaults. `workspace/configuration` returns a list; `workspace/didChangeConfiguration`
;; may send the settings object, `null`, or an empty object.
(define (normalize-configuration-item item)
  (if (eq? item (json-null))
      (hasheq)
      item))

(define (update-configuration settings)
  (define normalized
    (if (list? settings)
        (map normalize-configuration-item settings)
        (normalize-configuration-item settings)))
  (match normalized
    [(as-Langserver-Settings-Update value)
     (for ([item (in-list (if (list? value) value (list value)))])
       (apply-langserver-settings item))]
    [_ (void)]))

(define client-capability-workspace/configuration? (make-parameter #f))

;; `scopeUri` is optional; the callback applies process-wide either way.
(define (fetch-configuration request-client [uri (Nothing)])
  (when (client-capability-workspace/configuration?)
    (request-client "workspace/configuration"
                    (->jsexpr
                      (ConfigurationParams
                        #:items (list (ConfigurationItem
                                        #:scopeUri uri
                                        #:section "racket-langserver"))))
                    update-configuration)))

;; A client that has no `racket-langserver` section to push is not saying the
;; configuration is empty, it is saying to use the pull model instead.
(define (didChangeConfiguration request-client params)
  (match-define (hash-table ['settings settings]) params)
  (match settings
    [(hash-table ['racket-langserver langserver-settings])
     (update-configuration langserver-settings)]
    [_ (fetch-configuration request-client)]))

