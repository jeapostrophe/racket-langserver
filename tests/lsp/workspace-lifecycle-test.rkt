#lang racket/base

(require "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../common/path-util.rkt"
         "../../common/settings.rkt"
         "../../doclib/doc.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/workspace.rkt"
         "../../workspace/api.rkt"
         "../../workspace/current.rkt"
         "analysis-test-support.rkt"
         racket/async-channel
         racket/file
         racket/list
         rackunit)

(require/expose "../../lsp/workspace.rkt" (republish-open-doc-contributions!))
(require/expose "../../workspace/state.rkt" (Workspace-lock))

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
  (analyze! safe-doc)
  (wait-for-disk-verification! safe-doc)
  (define contribution
    (with-read-doc safe-doc Doc-contribution))
  (values uri safe-doc contribution
          (with-read-doc safe-doc
            (lambda (doc) (doc-module-binding-at doc (Pos 2 0))))))

(define (check-contribution-paths module-binding expected)
  (check-equal?
    (map Reference-Source-path
         (workspace-reference-sources current-workspace module-binding))
    expected))

(module+ test
  (test-case
    "folder republication cannot overwrite a newer accepted contribution"
    (define root (make-temporary-file "workspace-publication~a" 'directory))
    (define source-path (build-path root "source.rkt"))
    (define uri (path->uri source-path))
    (define resyntax-enabled (get-resyntax-enabled))
    (define workspace-lock (Workspace-lock current-workspace))
    (define lock-held? #f)
    (define republisher #f)
    (define updater #f)
    (dynamic-wind
      void
      (lambda ()
        (display-to-file source-text source-path)
        (set-resyntax-enabled! #f)
        (workspace-add-folder! current-workspace root)
        (define safe-doc (lsp-open-doc! uri (string-append source-text "first\n") 2))
        (analyze! safe-doc)
        (define binding
          (with-read-doc safe-doc
            (lambda (doc) (doc-module-binding-at doc (Pos 2 0)))))
        (define (reference-count)
          (length (append-map Reference-Source-locations
                              (workspace-reference-sources current-workspace binding))))
        (check-equal? (reference-count) 2)

        ;; Park the real republisher at the workspace lock after it reads the
        ;; old contribution, then let a newer analysis compete with it.
        (semaphore-wait workspace-lock)
        (set! lock-held? #t)
        (set! republisher (thread republish-open-doc-contributions!))
        (check-not-false (sync/timeout 20 (system-idle-evt)))
        (check-false (thread-dead? republisher))
        (thread-suspend republisher)
        (define edited (make-semaphore))
        (define updated (make-async-channel))
        (set! updater
              (thread
                (lambda ()
                  (async-channel-put
                    updated
                    (with-handlers ([exn? values])
                      (with-write-doc safe-doc
                        (lambda (doc)
                          (doc-reset! doc source-text)
                          (doc-update-version! doc 3)))
                      (semaphore-post edited)
                      (analyze! safe-doc)
                      (wait-for-disk-verification! safe-doc)
                      #t)))))
        (semaphore-post workspace-lock)
        (set! lock-held? #f)
        ;; Without the document lock, the newer analysis can finish first and
        ;; then be overwritten. With it, publication waits for the republisher.
        (check-not-false (sync/timeout 20 (system-idle-evt)))
        (define early-result
          (and (sync/timeout 0 edited)
               (let ([result (sync/timeout 20 updated)])
                 (check-not-false result "unblocked analysis finishes first")
                 result)))
        (thread-resume republisher)
        (check-eq? (sync/timeout 20 republisher) republisher)
        (define result (or early-result (sync/timeout 20 updated)))
        (when (exn? result) (raise result))
        (check-true result "newer analysis finishes")
        (check-eq? (sync/timeout 20 updater) updater)
        (check-equal? (reference-count) 1)
        ;; Provenance verified the newer contribution, so that is also the
        ;; contribution retained when the buffer closes.
        (lsp-close-doc! uri)
        (check-equal? (reference-count) 1))
      (lambda ()
        (when lock-held? (semaphore-post workspace-lock))
        (when republisher
          (thread-resume republisher)
          (sync/timeout 20 republisher))
        (when updater (sync/timeout 20 updater))
        (lsp-close-doc! uri)
        (workspace-remove-folder! current-workspace root)
        (set-resyntax-enabled! resyntax-enabled)
        (delete-directory/files root))))

  (test-case
    "workspace lifecycle preserves only accepted covered contributions"
    (define root (make-temporary-file "workspace-lifecycle~a" 'directory))
    (define source-path (build-path root "source.rkt"))
    (define renamed-path (build-path root "renamed.rkt"))
    (define resyntax-enabled (get-resyntax-enabled))
    (dynamic-wind
      void
      (lambda ()
        (display-to-file source-text source-path)
        (set-resyntax-enabled! #f)
        (define-values (uri safe-doc contribution module-binding)
          (open-expanded-doc source-path))

        ;; Folder addition republishes the accepted contribution of an open doc.
        (didChangeWorkspaceFolders (folder-change (list root) '()))
        (check-contribution-paths module-binding (list source-path))

        ;; Closing retains an accepted contribution verified against disk.
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

        ;; A disk deletion does not invalidate the open buffer's accepted
        ;; contribution. Watched events do not perform background analysis.
        (didChangeWatchedFiles (delete-file source-path))
        (check-eq? (lsp-get-doc reopened-uri) reopened-doc)
        (check-contribution-paths module-binding (list source-path))

        ;; The disk change is remembered until the client closes its buffer.
        (lsp-close-doc! reopened-uri)
        (check-contribution-paths module-binding '())
        (didChangeWatchedFiles (delete-file source-path))
        (check-contribution-paths module-binding '())

        ;; Rename invalidates disk paths; client notifications own the buffers.
        (define-values (_rename-uri rename-doc _rename-contribution _rename-binding)
          (open-expanded-doc source-path))
        (didChangeWorkspaceFolders (folder-change (list root) '()))
        (check-contribution-paths module-binding (list source-path))
        (rename-file-or-directory source-path renamed-path)
        (didRenameFiles (rename-files source-path renamed-path))
        (check-eq? (lsp-get-doc (path->uri source-path)) rename-doc)
        (check-false (lsp-get-doc (path->uri renamed-path) #f))
        (check-contribution-paths module-binding (list source-path))
        (lsp-close-doc! (path->uri source-path))
        (check-contribution-paths module-binding '())
        (define-values (_new-uri _new-doc _new-contribution _new-binding)
          (open-expanded-doc renamed-path))
        (check-contribution-paths module-binding (list renamed-path)))
      (lambda ()
        (lsp-close-doc! (path->uri source-path))
        (lsp-close-doc! (path->uri renamed-path))
        (workspace-remove-folder! current-workspace root)
        (set-resyntax-enabled! resyntax-enabled)
        (delete-directory/files root)))))
