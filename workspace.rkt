#lang racket
(provide didRenameFiles
         didChangeWorkspaceFolders
         didChangeWatchedFiles
         didChangeConfiguration)
(require compiler/module-suffix)
(require "json-util.rkt"
         "doc.rkt"
         "scheduler.rkt"
         "settings.rkt")
(require "open-docs.rkt")

(define-json-expander FileRename
  [oldUri string?]
  [newUri string?])
(define-json-expander RenameFilesParams
  [files (listof hash?)])

(define-json-expander WorkspaceFolder
  [uri string?]
  [name string?])
(define-json-expander WorkspaceFoldersChangeEvent
  [added (listof hash?)]
  [removed (listof hash?)])

(define-json-expander FileEvent
  [uri string?]
  [type exact-positive-integer?])
(define-json-expander DidChangeWatchedFilesParams
  [changes (listof hash?)])

(define workspace-folders (mutable-set))

(define (didRenameFiles params)
  (match-define (RenameFilesParams #:files files) params)
  (for ([f files])
    (match-define (FileRename #:oldUri old-uri #:newUri new-uri) f)

    ; remove all awaiting internal queries about `old-uri`
    (clear-old-queries/doc-close old-uri)

    (if (regexp-match (get-module-suffix-regexp) new-uri)
        (let ([safe-doc (hash-ref open-docs (string->symbol old-uri) #f)])
          ; `safe-doc = #f` should be rarely happened.
          ; we simply give up to handle it, let's trust LSP client will send others request about analysis this file.
          (when safe-doc
            (with-write-doc safe-doc
                            (lambda (doc)
                              (doc-update-uri! doc new-uri)))
            (hash-set! open-docs (string->symbol new-uri) safe-doc)))
        (hash-remove! open-docs (string->symbol old-uri)))))

(define (didChangeWorkspaceFolders params)
  (match-define (hash-table ['event (WorkspaceFoldersChangeEvent #:added added #:removed removed)]) params)
  (for ([f added])
    (match-define (WorkspaceFolder #:uri uri #:name _) f)
    (set-add! workspace-folders uri))
  (for ([f removed])
    (match-define (WorkspaceFolder #:uri uri #:name _) f)
    (set-remove! workspace-folders uri)))

(define (didChangeWatchedFiles params)
  (match-define (DidChangeWatchedFilesParams #:changes changes) params)
  (for ([change changes])
    (match-define (FileEvent #:uri uri #:type type) change)
    (match type
      [1 (handle-file-created uri)]
      [2 (handle-file-changed uri)]
      [3 (handle-file-deleted uri)]
      [_ (eprintf "Invalid file event type: ~a~n" type)])))
(define (handle-file-created uri)
  (when (regexp-match (get-module-suffix-regexp) uri)
    (define safe-doc (new-doc uri "" 0))
    (hash-set! open-docs (string->symbol uri) safe-doc)))
(define (handle-file-changed uri)
  (when (regexp-match (get-module-suffix-regexp) uri)
    (let ([safe-doc (hash-ref open-docs (string->symbol uri) #f)])
      (when safe-doc
        (clear-old-queries/doc-close uri)))))
(define (handle-file-deleted uri)
  (when (regexp-match (get-module-suffix-regexp) uri)
    (clear-old-queries/doc-close uri)
    (hash-remove! open-docs (string->symbol uri))))

(define (didChangeConfiguration params)
  (match-define (hash-table ['settings settings]) params)

  (define key '(resyntax enable))
  (when (jsexpr-has-key? settings key)
    (set-resyntax-enabled! (jsexpr-ref settings key))))
