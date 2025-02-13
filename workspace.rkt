#lang racket
(provide didRenameFiles didChangeWorkspaceFolders)
(require "json-util.rkt"
         "doc.rkt"
         "scheduler.rkt")
(require "open-docs.rkt")

(define-json-expander FileRename
  [oldUri string?]
  [newUri string?])
(define-json-expander  RenameFilesParams
  [files (listof hash?)])

(define-json-expander WorkspaceFolder
  [uri string?]
  [name string?])
(define-json-expander WorkspaceFoldersChangeEvent
  [added (listof hash?)]
  [removed (listof hash?)])

(define workspace-folders (mutable-set))

(define (didRenameFiles params)
  (match-define (RenameFilesParams #:files files) params)
  (for ([f files])
    (match-define (FileRename #:oldUri old-uri #:newUri new-uri) f)

    ; remove all awaiting internal queries about `old-uri`
    (clear-old-queries/doc-close old-uri)

    (if (string-suffix? new-uri ".rkt")
      (let ([safe-doc (hash-ref open-docs (string->symbol old-uri) #f)])
        ; `safe-doc = #f` should be rarely happened.
        ; we simply give up to handle it, let's trust LSP client will send others request about analysis this file.
        (when safe-doc
          (doc-update-uri! safe-doc new-uri)
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
