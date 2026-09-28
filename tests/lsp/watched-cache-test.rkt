#lang racket/base

(require rackunit
         racket/async-channel
         racket/file
         racket/list
         racket/string
         "../../common/path-util.rkt"
         "../../common/settings.rkt"
         "../../doclib/doc.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         (submod "../../lsp/safedoc.rkt" test-support)
         "../../lsp/workspace.rkt"
         "../../workspace/api.rkt"
         "../../workspace/current.rkt")

(define source-text "#lang racket/base\n(require racket/list)\nfirst\n")

(define (notify-watched! uri type)
  (didChangeWatchedFiles
    (hasheq 'changes (list (hasheq 'uri uri 'type type)))))

(define (with-source proc)
  (define directory (make-temporary-file "watched-cache~a" 'directory))
  (define path (build-path directory "source.rkt"))
  (define uri (path->uri path))
  (define alias (string-replace uri "source.rkt" "%73ource.rkt"))
  (define resyntax-enabled (get-resyntax-enabled))
  (dynamic-wind
    (lambda ()
      (display-to-file source-text path)
      (workspace-add-folder! current-workspace directory)
      (set-resyntax-enabled! #f))
    (lambda () (proc path uri alias))
    (lambda ()
      (lsp-close-doc! uri)
      (lsp-close-doc! alias)
      (workspace-remove-folder! current-workspace directory)
      (set-resyntax-enabled! resyntax-enabled)
      (delete-directory/files directory))))

(define (open-accepted! uri)
  (define sd (lsp-open-doc! uri source-text 2))
  (define contribution
    (with-write-doc sd
      (lambda (doc)
        (check-true (doc-expand! doc))
        (Doc-contribution doc))))
  (workspace-set-contribution! current-workspace contribution)
  (values sd (first (hash-keys (Doc-Contribution-references contribution)))))

(define (check-paths binding expected)
  (check-equal?
    (map Reference-Source-path (workspace-reference-sources current-workspace binding))
    expected))

(module+ test
  (test-case
    "each disk event invalidates a closed cache without opening a document"
    (for ([type (in-list '(1 2 3))])
      (with-source
        (lambda (path uri alias)
          (define-values (_sd binding) (open-accepted! uri))
          (lsp-close-doc! uri)
          (check-paths binding (list path))
          (notify-watched! alias type)
          (check-paths binding '())
          (check-false (lsp-get-doc uri #f))
          (check-false (lsp-get-doc alias #f))))))

  (test-case
    "each disk event preserves an open alias and invalidates its cache on close"
    (for ([type (in-list '(1 2 3))])
      (with-source
        (lambda (path uri alias)
          (define-values (sd binding) (open-accepted! uri))
          (notify-watched! alias type)
          (check-eq? (lsp-get-doc uri) sd)
          (check-paths binding (list path))
          ;; An edit and successful expansion must not forget the disk event.
          (with-write-doc sd
            (lambda (doc)
              (doc-update-version! doc 3)
              (check-true (doc-expand! doc))))
          (lsp-close-doc! uri)
          (check-paths binding '())))))

  (test-case
    "invalidation waits until every URI for an open path closes"
    (with-source
      (lambda (path uri alias)
        (define-values (_sd binding) (open-accepted! uri))
        (define other (lsp-open-doc! alias source-text 2))
        (notify-watched! uri 3)
        (lsp-close-doc! uri)
        (check-eq? (lsp-get-doc alias) other)
        (check-paths binding (list path))
        (lsp-close-doc! alias)
        (check-paths binding '()))))

  (test-case
    "closing and reopening starts a new document lifetime"
    (with-source
      (lambda (path uri _alias)
        (define-values (old binding) (open-accepted! uri))
        (notify-watched! uri 2)
        (lsp-close-doc! uri)
        (check-paths binding '())
        (define-values (reopened _binding) (open-accepted! uri))
        ;; Simulate a publication already computed for the retired lifetime.
        ;; The same version in a new document must not make it admissible.
        (check-false
          (with-current-safedoc old 2
            (lambda (_sd) (fail "retired document accepted a late publication"))))
        (check-true
          (with-current-safedoc reopened 2 void))
        (check-false
          (with-current-safedoc reopened 1
            (lambda (_sd) (fail "stale version accepted a publication"))))
        (lsp-close-doc! uri)
        (check-paths binding (list path)))))

  (test-case
    "invalid batches cannot mark an open cache dirty or purge a closed cache"
    (with-source
      (lambda (path uri _alias)
        (define-values (_sd binding) (open-accepted! uri))
        (define (invalid-event!)
          (didChangeWatchedFiles
            (hasheq 'changes (list (hasheq 'uri uri 'type 3)
                                   (hasheq 'uri uri 'type 9)))))
        (check-exn exn:fail? invalid-event!)
        (lsp-close-doc! uri)
        (check-paths binding (list path))
        (check-exn exn:fail? invalid-event!)
        (check-paths binding (list path)))))

  (test-case
    "file events are handled even when virtual URIs occur in the same batch"
    (with-source
      (lambda (path uri _alias)
        (define-values (_sd binding) (open-accepted! uri))
        (lsp-close-doc! uri)
        (didChangeWatchedFiles
          (hasheq 'changes
                  (list (hasheq 'uri "untitled:source.rkt" 'type 1)
                        (hasheq 'uri "vscode-remote://example/source.rkt" 'type 2)
                        (hasheq 'uri uri 'type 3))))
        (check-paths binding '()))))

  (test-case
    "an encoded URI owned by the editor is protected from a canonical disk event"
    (with-source
      (lambda (path uri alias)
        (define-values (sd binding) (open-accepted! alias))
        (notify-watched! uri 3)
        (check-eq? (lsp-get-doc alias) sd)
        (check-paths binding (list path))
        (lsp-close-doc! alias)
        (check-paths binding '()))))

  (test-case
    "close serializes with an in-flight publication before purging deleted facts"
    (with-source
      (lambda (path uri alias)
        (define-values (sd binding) (open-accepted! uri))
        (define entered (make-async-channel))
        (define release (make-semaphore))
        (define closing (make-semaphore))
        (define worker #f)
        (define closer #f)
        (dynamic-wind
          void
          (lambda ()
            (safedoc-run-check-syntax!
              (lambda (_method _params)
                (async-channel-put entered (current-thread))
                (semaphore-wait release))
              sd)
            (set! worker (sync/timeout 20 entered))
            (check-true (thread? worker))
            (set! closer
                  (thread
                    (lambda ()
                      (semaphore-post closing)
                      (lsp-close-doc! uri)
                      (notify-watched! alias 3))))
            (check-not-false (sync/timeout 5 closing))
            ;; The callback is inside the existing write critical section.
            ;; Close must wait for it, then retire the document before purging.
            (check-false (sync/timeout 0.05 closer))
            (semaphore-post release)
            (check-eq? (sync/timeout 20 worker) worker)
            (check-eq? (sync/timeout 20 closer) closer)
            (check-false (lsp-get-doc uri #f))
            (check-paths binding '()))
          (lambda ()
            (semaphore-post release)
            (when worker (sync/timeout 20 worker))
            (when closer (sync/timeout 20 closer))))))))
