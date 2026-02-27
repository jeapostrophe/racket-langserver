#lang racket
(provide didRenameFiles
         didChangeWorkspaceFolders
         didChangeWatchedFiles
         didChangeConfiguration
         update-configuration
         add-workspace-folder!)
(require compiler/module-suffix
         json)
(require "json-util.rkt"
         "interfaces.rkt"
         "lsp.rkt"
         "safedoc.rkt"
         "doc.rkt"
         "scheduler.rkt"
         "settings.rkt")

(define workspace-folders (mutable-set))
(define (add-workspace-folder! path)
  (set-add! workspace-folders path))

(define (didRenameFiles params)
  (match-define (^RenameFilesParams #:files files) params)
  (for ([f files])
    (match-define (FileRename #:oldUri old-uri #:newUri new-uri) f)

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
    (set-add! workspace-folders uri))
  (for ([f removed])
    (match-define (WorkspaceFolder #:uri uri #:name _) f)
    (set-remove! workspace-folders uri)))

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
  (when (regexp-match (get-module-suffix-regexp) uri)
    (lsp-close-doc! uri)))

(define (update-configuration settings)
  (for ([setting settings]
        #:unless (equal? setting (json-null)))
    (define key '(resyntax enable))
    (when (jsexpr-has-key? setting key)
      (set-resyntax-enabled! (jsexpr-ref setting key)))))

(define (didChangeConfiguration params)
  (match-define (hash-table ['settings settings]) params)
  (update-configuration settings))

