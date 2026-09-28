#lang racket/base

(require rackunit
         racket/async-channel
         racket/file
         racket/list
         racket/path
         "../../common/path-util.rkt"
         "../../common/settings.rkt"
         "../../doclib/doc.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         (submod "../../lsp/safedoc.rkt" test-support)
         "../../lsp/scheduler.rkt"
         "../../workspace/api.rkt"
         "../../workspace/current.rkt"
         "../lsp/analysis-test-support.rkt")

(provide run-disk-provenance-tests)

;; Instantiated under a security guard by disk-provenance-test.rkt so the real
;; scheduler inherits that guard. The guard controls only this fixture's file.
(define (run-disk-provenance-tests path install-read-hook!)
  (define source "#lang racket/base\n(require racket/list)\nfirst\n")
  (define uri (path->uri path))
  (define directory (path-only path))
  (define resyntax-enabled (get-resyntax-enabled))
  (define (verified? sd)
    (with-read-safedoc sd SafeDoc-contribution-matches-disk?))
  (define (reference-paths sd)
    (define contribution (with-read-doc sd Doc-contribution))
    (define binding (first (hash-keys (Doc-Contribution-references contribution))))
    (map Reference-Source-path (workspace-reference-sources current-workspace binding)))
  (define (with-blocked-read proc)
    (define entered (make-async-channel))
    (define release (make-semaphore))
    (dynamic-wind
      (lambda ()
        (install-read-hook!
          (lambda ()
            (async-channel-put entered (current-thread))
            (parameterize-break #t (sync (semaphore-peek-evt release))))))
      (lambda () (proc entered))
      (lambda ()
        (install-read-hook! void)
        (semaphore-post release)
        (lsp-close-doc! uri))))
  (dynamic-wind
    (lambda ()
      (display-to-file source path)
      (workspace-add-folder! current-workspace directory)
      (set-resyntax-enabled! #f))
    (lambda ()
      (test-case
        "blocked disk verification withholds neither diagnostics nor waiting queries"
        (with-blocked-read
          (lambda (entered)
            (define sd (lsp-open-doc! uri source 2))
            (define queried (make-async-channel))
            (async-query-wait
              (SafeDoc-token sd)
              (lambda (signal)
                (async-channel-put queried (signal-check-syntax-finished? signal))))
            (analyze! sd)
            (define reader (sync/timeout 5 entered))
            (check-true (thread? reader) "verification reached the stalled file open")
            (check-true (sync/timeout 5 queried) "analysis released its waiting query")
            (check-false (verified? sd))
            (check-equal? (reference-paths sd) (list path))
            (define closer (thread (lambda () (lsp-close-doc! uri))))
            (check-eq? (sync/timeout 5 closer) closer "close does not wait for disk I/O")
            (check-eq? (sync/timeout 5 reader) reader "close cancels the actual disk reader")
            (check-equal? (reference-paths sd) '())
            (check-false (verified? sd))
            ;; The same URI and version in a new lifetime can verify normally.
            (install-read-hook! void)
            (define reopened (lsp-open-doc! uri source 2))
            (analyze! reopened)
            (wait-for-disk-verification! reopened)
            (lsp-close-doc! uri)
            (check-equal? (reference-paths reopened) (list path)))))

      (test-case
        "a new unsaved analysis cancels verification of the prior version"
        (with-blocked-read
          (lambda (entered)
            (define sd (lsp-open-doc! uri source 2))
            (analyze! sd)
            (define reader (sync/timeout 5 entered))
            (check-true (thread? reader))
            (install-read-hook! void)
            (with-write-doc sd
              (lambda (doc)
                (doc-reset! doc (string-append source "first\n"))
                (doc-update-version! doc 3)))
            (analyze! sd)
            (check-eq? (sync/timeout 5 reader) reader "edit cancels the superseded reader")
            (check-false (verified? sd))
            (lsp-close-doc! uri)
            (check-equal? (reference-paths sd) '())))))
    (lambda ()
      (install-read-hook! void)
      (lsp-close-doc! uri)
      (workspace-remove-folder! current-workspace directory)
      (set-resyntax-enabled! resyntax-enabled))))
