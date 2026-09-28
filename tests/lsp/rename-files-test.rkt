#lang racket/base

(require rackunit
         racket/async-channel
         racket/file
         racket/list
         racket/set
         racket/string
         "../../common/interfaces.rkt"
         "../../common/path-util.rkt"
         "../../common/settings.rkt"
         "../../doclib/doc.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/scheduler.rkt"
         "../../lsp/text-document.rkt"
         "../../lsp/workspace.rkt"
         "../../workspace/api.rkt"
         "../../workspace/current.rkt"
         "analysis-test-support.rkt")

(define source-text "#lang racket/base\n(require racket/list)\nfirst\n")
(define destination-text (string-append source-text "first\n"))

(define (encoded uri)
  (string-replace uri ".rkt" ".%72kt"))

(define (with-files proc)
  (define directory (make-temporary-file "rename-files~a" 'directory))
  (define old-path (build-path directory "old.rkt"))
  (define new-path (build-path directory "new.rkt"))
  (define old-uri (path->uri old-path))
  (define new-uri (path->uri new-path))
  (define resyntax-enabled (get-resyntax-enabled))
  (dynamic-wind
    void
    (lambda ()
      (display-to-file source-text old-path)
      (display-to-file destination-text new-path)
      (set-resyntax-enabled! #f)
      (workspace-add-folder! current-workspace directory)
      (proc old-uri new-uri))
    (lambda ()
      (for ([uri (in-list (list old-uri new-uri (encoded old-uri) (encoded new-uri)))])
        (lsp-close-doc! uri))
      (workspace-remove-folder! current-workspace directory)
      (set-resyntax-enabled! resyntax-enabled)
      (delete-directory/files directory))))

(define (accepted! uri text)
  (define sd (lsp-open-doc! uri text 2))
  (analyze! sd)
  (wait-for-disk-verification! sd)
  (values sd
          (with-read-doc sd
            (lambda (doc)
              (first (hash-keys (Doc-Contribution-references (Doc-contribution doc))))))))

(define (renamed! old-uri new-uri)
  (didRenameFiles (hasheq 'files (list (hasheq 'oldUri old-uri 'newUri new-uri)))))

(define (locations binding)
  (list->set
    (append-map Reference-Source-locations
                (workspace-reference-sources current-workspace binding))))

(module+ test
  (test-case
    "renaming closed files invalidates both source and destination caches"
    (with-files
      (lambda (old-uri new-uri)
        (define-values (_old binding) (accepted! old-uri source-text))
        (define-values (_new _binding) (accepted! new-uri destination-text))
        (lsp-close-doc! old-uri)
        (lsp-close-doc! new-uri)
        (check-equal? (set-count (locations binding)) 3)
        (rename-file-or-directory (uri->path old-uri) (uri->path new-uri) #t)
        (renamed! old-uri new-uri)
        (check-equal? (locations binding) (set))
        (check-false (lsp-get-doc old-uri #f))
        (check-false (lsp-get-doc new-uri #f)))))

  (test-case
    "rename preserves both client buffers, versions and pending requests"
    (with-files
      (lambda (old-uri new-uri)
        (define old (lsp-open-doc! old-uri source-text 2))
        (define new (lsp-open-doc! new-uri destination-text 7))
        (define signals '())
        (for ([sd (in-list (list old new))])
          (async-query-wait (SafeDoc-token sd)
                            (lambda (signal) (set! signals (cons signal signals)))))
        (renamed! old-uri new-uri)
        (check-eq? (lsp-get-doc old-uri #f) old)
        (check-eq? (lsp-get-doc new-uri #f) new)
        (check-equal? signals '())
        (for ([sd (in-list (list old new))]
              [text (in-list (list source-text destination-text))]
              [version '(2 7)])
          (with-read-doc sd
            (lambda (doc)
              (check-equal? (doc-get-text doc) text)
              (check-equal? (Doc-version doc) version)))))))

  (test-case
    "client reopen analyses the new URI for every rename notification ordering"
    (for ([order '(before between after)])
      (with-files
        (lambda (old-uri new-uri)
          (define-values (old binding) (accepted! old-uri source-text))
          (define-values (_cached _binding) (accepted! new-uri destination-text))
          (lsp-close-doc! new-uri)
          (rename-file-or-directory (uri->path old-uri) (uri->path new-uri) #t)
          (when (eq? order 'before)
            (renamed! old-uri new-uri)
            (check-eq? (lsp-get-doc old-uri #f) old)
            (check-false (lsp-get-doc new-uri #f)))
          (did-close! (hasheq 'textDocument (hasheq 'uri old-uri)))
          (when (eq? order 'between) (renamed! old-uri new-uri))
          (define published (make-async-channel))
          (did-open! void
                     (lambda (_method params)
                       (async-channel-put published (cons params (current-thread))))
                     (hasheq 'textDocument
                             (hasheq 'uri new-uri 'languageId "racket"
                                     'version 9 'text source-text)))
          (define reopened (lsp-get-doc new-uri))
          (when (eq? order 'after) (renamed! old-uri new-uri))
          (define publication (sync/timeout 20 published))
          (check-not-false publication "didOpen must schedule analysis itself")
          (check-equal? (hash-ref (car publication) 'uri) new-uri)
          (define worker (cdr publication))
          (check-eq? (sync/timeout 20 worker) worker)
          (check-eq? (lsp-get-doc new-uri) reopened)
          (with-read-doc reopened
            (lambda (doc)
              (check-equal? (Doc-version doc) 9)
              (check-true (doc-trace-latest? doc))))
          (check-equal? (locations binding)
                        (set (Location new-uri (Range (Pos 2 0) (Pos 2 5)))))
          (check-false (lsp-get-doc old-uri #f))))))

  (test-case
    "rename invalidation is deferred for encoded aliases at both paths"
    (with-files
      (lambda (old-uri new-uri)
        (define-values (canonical binding) (accepted! old-uri source-text))
        (define old (lsp-open-doc! (encoded old-uri) destination-text 2))
        (analyze! old)
        (define-values (new _binding) (accepted! (encoded new-uri) destination-text))
        (define destination-locations
          (set (Location new-uri (Range (Pos 2 0) (Pos 2 5)))
               (Location new-uri (Range (Pos 3 0) (Pos 3 5)))))
        (define before (locations binding))
        (renamed! old-uri new-uri)
        (check-eq? (lsp-get-doc old-uri) canonical)
        (check-eq? (lsp-get-doc (encoded old-uri)) old)
        (check-eq? (lsp-get-doc (encoded new-uri)) new)
        (check-false (lsp-get-doc new-uri #f))
        (check-equal? (locations binding) before)
        (lsp-close-doc! (encoded old-uri))
        (check-equal? (locations binding)
                      (set-add destination-locations
                               (Location old-uri (Range (Pos 2 0) (Pos 2 5)))))
        (lsp-close-doc! old-uri)
        (check-equal? (locations binding) destination-locations)
        (lsp-close-doc! (encoded new-uri))
        (check-equal? (locations binding) (set)))))

  (test-case
    "malformed rename batches cannot invalidate either cache"
    (with-files
      (lambda (old-uri new-uri)
        (define-values (_old binding) (accepted! old-uri source-text))
        (define-values (_new _binding) (accepted! new-uri destination-text))
        (lsp-close-doc! old-uri)
        (lsp-close-doc! new-uri)
        (define before (locations binding))
        (for ([invalid (in-list (list 42 "file:///bad%00.rkt"))])
          (check-exn exn:fail?
                     (lambda ()
                       (didRenameFiles
                         (hasheq 'files
                                 (list (hasheq 'oldUri old-uri 'newUri new-uri)
                                       (hasheq 'oldUri old-uri 'newUri invalid))))))
          (check-equal? (locations binding) before)))))

  (test-case
    "non-file rename URIs cannot invalidate matching filesystem paths"
    (with-files
      (lambda (old-uri new-uri)
        (define-values (_old binding) (accepted! old-uri source-text))
        (define-values (_new _binding) (accepted! new-uri destination-text))
        (lsp-close-doc! old-uri)
        (lsp-close-doc! new-uri)
        (define before (locations binding))
        (renamed! (string-replace old-uri "file:" "untitled:")
                  (string-replace new-uri "file:" "vscode-remote:"))
        (check-equal? (locations binding) before)
        ;; Both URI lists are decoded before any invalidation occurs.
        (didRenameFiles
          (hasheq 'files (list (hasheq 'oldUri "untitled:source.rkt"
                                       'newUri "untitled:renamed.rkt")
                               (hasheq 'oldUri old-uri 'newUri new-uri))))
        (check-equal? (locations binding) (set))))))
