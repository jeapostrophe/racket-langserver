#lang racket/base

;; Workspace notifications and settings. Folder membership changes index and
;; search scope only, not source revisions or eligibility.
(provide didRenameFiles
         didChangeWorkspaceFolders
         didChangeWatchedFiles
         didChangeConfiguration
         update-configuration
         fetch-configuration
         client-capability-workspace/configuration?)

(require json
         racket/match
         "../common/json-util.rkt"
         "../common/path-util.rkt"
         "../common/source.rkt"
         "../common/interfaces.rkt"
         "lsp.rkt"
         "safedoc.rkt"
         "source-controller.rkt"
         (submod "source-controller.rkt" owner)
         "../doclib/doc.rkt"
         "../common/settings.rkt"
         "../workspace/current.rkt"
         "../workspace/state.rkt")

(define (republish-open-doc-contributions!)
  (lsp-for-each-open-doc
    (lambda (safe-doc)
      (define contribution
        (with-read-doc safe-doc Doc-contribution))
      (when contribution
        (workspace-set-contribution! current-workspace contribution)))))

(define (didRenameFiles params)
  (match-define (^RenameFilesParams #:files files) params)
  ;; textDocument open and close own the editor buffer. Rename invalidates the
  ;; old and new path identities and drops the old index entry.
  (for ([f (in-list files)])
    (match-define (FileRename #:oldUri old-uri #:newUri new-uri) f)
    (when (file-uri? old-uri)
      (define old-path (uri->path old-uri))
      (source-controller-invalidate! (current-source-controller) (uri->source old-uri))
      (workspace-remove-path! current-workspace old-path))
    (when (file-uri? new-uri)
      (source-controller-invalidate! (current-source-controller) (uri->source new-uri)))))

;; A folder change does not start a disk read.
(define (didChangeWorkspaceFolders params)
  (match-define (^DidChangeWorkspaceFoldersParams #:event event) params)
  (match-define (WorkspaceFoldersChangeEvent #:added added #:removed removed) event)
  (for ([f (in-list added)])
    (match-define (WorkspaceFolder #:uri uri #:name _) f)
    (when (file-uri? uri)
      (workspace-add-folder! current-workspace (uri->path uri))))
  (for ([f (in-list removed)])
    (match-define (WorkspaceFolder #:uri uri #:name _) f)
    (when (file-uri? uri)
      (workspace-remove-folder! current-workspace (uri->path uri))))
  (when (pair? added)
    (republish-open-doc-contributions!)))

;; Status: ignored. Watched-file events do not own the editor buffer or index.
(define (didChangeWatchedFiles _params)
  (void))

(define (apply-langserver-settings settings)
  (match-define (Langserver-Settings #:resyntax resyntax #:formatting formatting)
    settings)
  (set-resyntax-enabled!
    (match resyntax
      [(Resyntax-Settings #:enable (and enable (not (? Nothing?)))) enable]
      [_ default-resyntax-enabled]))
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

(define (normalize-configuration-item item)
  (if (eq? item (json-null))
      (hasheq)
      item))

;; A `racket-langserver` section is a snapshot. Omitted keys use shipped
;; defaults. `workspace/configuration` returns a list; `workspace/didChangeConfiguration`
;; may send the settings object, `null`, or an empty object.
(define (update-configuration settings)
  (define items
    (map normalize-configuration-item
         (if (list? settings) settings (list settings))))
  (match items
    [(as-Langserver-Settings-Update value)
     (for ([item (in-list value)])
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

;; A client with no `racket-langserver` section to push is asking for the pull
;; model, not sending an empty configuration.
(define (didChangeConfiguration request-client params)
  (match-define (hash-table ['settings settings]) params)
  (match settings
    [(hash-table ['racket-langserver langserver-settings])
     (update-configuration langserver-settings)]
    [_ (fetch-configuration request-client)]))
