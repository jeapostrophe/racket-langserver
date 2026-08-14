#lang racket/base

(require "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../common/path-util.rkt"
         "../../doclib/doc.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/workspace.rkt"
         "../../workspace/api.rkt"
         "../../workspace/current.rkt"
         racket/file
         racket/list
         rackunit)

(define source-text
  "#lang racket/base\n(require racket/list)\nfirst\n")

(define (folder-change added removed)
  (->jsexpr
    (DidChangeWorkspaceFoldersParams
      #:event
      (WorkspaceFoldersChangeEvent
        #:added
        (for/list ([path (in-list added)])
          (WorkspaceFolder #:uri (path->uri path) #:name "test"))
        #:removed
        (for/list ([path (in-list removed)])
          (WorkspaceFolder #:uri (path->uri path) #:name "test"))))))

(define (rename-files old-path new-path)
  (->jsexpr
    (RenameFilesParams
      #:files
      (list (FileRename #:oldUri (path->uri old-path) #:newUri (path->uri new-path))))))

(define (delete-file path)
  (->jsexpr
    (DidChangeWatchedFilesParams
      #:changes
      (list (FileEvent #:uri (path->uri path) #:type FileChangeType-deleted)))))

(define (open-expanded-doc path)
  (define uri (path->uri path))
  (define safe-doc (lsp-open-doc! uri source-text 0))
  (define contribution
    (with-write-doc safe-doc
      (lambda (doc)
        (check-true (doc-expand! doc))
        (Doc-contribution doc))))
  (values uri safe-doc contribution (first (hash-keys (Doc-Contribution-references contribution)))))

(define (check-contribution-paths module-binding expected)
  (check-equal?
    (map Reference-Source-path
         (workspace-reference-sources current-workspace module-binding))
    expected))

(module+ test
  (test-case
    "workspace lifecycle preserves only accepted covered contributions"
    (define root (make-temporary-file "workspace-lifecycle~a" 'directory))
    (define source-path (build-path root "source.rkt"))
    (define renamed-path (build-path root "renamed.rkt"))
    (define-values (uri safe-doc contribution module-binding)
      (open-expanded-doc source-path))

    (dynamic-wind
      void
      (lambda ()
        ;; Folder addition republishes the accepted contribution of an open doc.
        (didChangeWorkspaceFolders (folder-change (list root) '()))
        (check-contribution-paths module-binding (list source-path))

        ;; Closing a document does not remove its accepted contribution.
        (lsp-close-doc! uri)
        (check-contribution-paths module-binding (list source-path))

        ;; Removing the last covering folder purges it.
        (didChangeWorkspaceFolders (folder-change '() (list root)))
        (check-contribution-paths module-binding '())

        ;; A failed expansion retains the prior accepted contribution, which a
        ;; later folder addition republishes.
        (define-values (reopened-uri reopened-doc reopened-contribution reopened-binding)
          (open-expanded-doc source-path))
        (check-equal? reopened-binding module-binding)
        (with-write-doc reopened-doc
          (lambda (doc)
            (doc-reset! doc "#lang racket/base\n(")
            (check-false (doc-expand! doc))
            (check-eq? (Doc-contribution doc) reopened-contribution)))
        (didChangeWorkspaceFolders (folder-change (list root) '()))
        (check-contribution-paths module-binding (list source-path))

        ;; Delete removes the old path contribution.
        (didChangeWatchedFiles (delete-file source-path))
        (check-contribution-paths module-binding '())

        ;; Rename removes the old path contribution and opens the new URI.
        (define-values (_rename-uri _rename-doc _rename-contribution _rename-binding)
          (open-expanded-doc source-path))
        (didChangeWorkspaceFolders (folder-change (list root) '()))
        (check-contribution-paths module-binding (list source-path))
        (didRenameFiles (rename-files source-path renamed-path))
        (check-contribution-paths module-binding '())
        (check-true (SafeDoc? (lsp-get-doc (path->uri renamed-path) #f))))
      (lambda ()
        (lsp-close-doc! uri)
        (lsp-close-doc! (path->uri source-path))
        (lsp-close-doc! (path->uri renamed-path))
        (workspace-remove-folder! current-workspace root)
        (delete-directory/files root)))))
