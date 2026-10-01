#lang racket/base

(require "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../common/path-util.rkt"
         "../../doclib/doc.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/scheduler.rkt"
         "../../lsp/workspace.rkt"
         "../../workspace/api.rkt"
         "../../workspace/current.rkt"
         racket/file
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

(define (watched-file-change path type)
  (->jsexpr
    (DidChangeWatchedFilesParams
      #:changes
      (list (FileEvent #:uri (path->uri path) #:type type)))))

(define (open-expanded-doc path)
  (define uri (path->uri path))
  (define safe-doc (lsp-open-doc! uri source-text 0))
  (define contribution
    (with-write-doc safe-doc
      (lambda (doc)
        (check-true (doc-expand! doc))
        (Doc-contribution doc))))
  (values uri safe-doc contribution
          (with-read-doc safe-doc (lambda (doc) (doc-module-binding-at doc (Pos 2 0))))))

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
            (doc-update-version! doc 2)
            (check-false (doc-expand! doc))
            (check-eq? (Doc-contribution doc) reopened-contribution)))
        (didChangeWorkspaceFolders (folder-change (list root) '()))
        (check-contribution-paths module-binding (list source-path))

        ;; Watched-file events leave the editor buffer and contribution untouched.
        (for ([type (in-list (list FileChangeType-deleted
                                   FileChangeType-created
                                   FileChangeType-changed))])
          (didChangeWatchedFiles (watched-file-change source-path type))
          (check-eq? (lsp-get-doc reopened-uri #f) reopened-doc)
          (with-read-doc reopened-doc
            (lambda (doc)
              (check-equal? (doc-get-text doc) "#lang racket/base\n(")
              (check-equal? (Doc-version doc) 2)))
          (check-contribution-paths module-binding (list source-path))
          (didChangeWatchedFiles (watched-file-change renamed-path type))
          (check-false (lsp-get-doc (path->uri renamed-path) #f)))

        ;; The next incremental edit still applies to the editor's text.
        (with-write-doc (lsp-get-doc reopened-uri)
          (lambda (doc)
            (doc-apply-edit! doc (Range (Pos 1 1) (Pos 1 1)) "void)")
            (doc-update-version! doc 3)
            (check-equal? (doc-get-text doc) "#lang racket/base\n(void)")
            (check-equal? (Doc-version doc) 3)))

        ;; Rename preserves editor ownership until client close/open notifications.
        (define-values (_rename-uri _rename-doc _rename-contribution _rename-binding)
          (open-expanded-doc source-path))
        (define query-signal #f)
        (async-query-wait (SafeDoc-token _rename-doc)
                          (lambda (signal) (set! query-signal signal)))
        (didChangeWorkspaceFolders (folder-change (list root) '()))
        (check-contribution-paths module-binding (list source-path))
        (didRenameFiles (rename-files source-path renamed-path))
        (check-contribution-paths module-binding '())
        (check-eq? (lsp-get-doc uri #f) _rename-doc)
        (check-false query-signal "a rename does not finish or cancel the buffer's queries")
        (check-false (lsp-get-doc (path->uri renamed-path) #f))

        ;; A destination that is already open also keeps its text and version.
        (define destination (lsp-open-doc! (path->uri renamed-path) "unsaved destination" 7))
        (didRenameFiles (rename-files source-path renamed-path))
        (check-eq? (lsp-get-doc uri #f) _rename-doc)
        (check-eq? (lsp-get-doc (path->uri renamed-path) #f) destination)
        (check-equal? (with-read-doc destination doc-get-text) "unsaved destination")
        (check-equal? (with-read-doc destination Doc-version) 7))
      (lambda ()
        (lsp-close-doc! uri)
        (lsp-close-doc! (path->uri source-path))
        (lsp-close-doc! (path->uri renamed-path))
        (workspace-remove-folder! current-workspace root)
        (delete-directory/files root)))))
