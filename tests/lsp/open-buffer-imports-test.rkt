#lang racket/base

;; Analysis reads an imported module from its open buffer, including unsaved
;; edits, and re-analyzes open documents that loaded it when that buffer changes.
(module+ test
  (require "controller-support.rkt"
           rackunit
           racket/async-channel
           racket/file
           compiler/cm
           "../../common/path-util.rkt"
           "../../common/source.rkt"
           "../../common/source-snapshot.rkt"
           "../../common/time.rkt"
           "../../lsp/analysis.rkt"
           (submod "../../lsp/analysis.rkt" test-support)
           "../../lsp/safedoc.rkt"
           "../../lsp/source-controller.rkt"
           (submod "../../lsp/source-controller.rkt" owner))

  (define (provider name)
    (format "#lang racket/base\n(provide ~a)\n(define (~a x) (+ x 1))\n" name name))

  (define (await-errors session path errors?)
    (define uri (path->uri path))
    (define deadline (+ (current-deadline-milliseconds) 60000))
    (let loop ()
      (when (>= (current-deadline-milliseconds) deadline)
        (error 'await-errors "no diagnostics for ~a with errors? = ~a" path errors?))
      (define message (await (Session-notifications session) 60))
      (define params (hash-ref message 'params (hasheq)))
      (define published-errors
        (for/list ([diagnostic (in-list (hash-ref params 'diagnostics '()))]
                   #:when (equal? (hash-ref diagnostic 'severity #f) 1))
          diagnostic))
      (unless (and (equal? (hash-ref message 'method #f) "textDocument/publishDiagnostics")
                   (equal? (hash-ref params 'uri #f) uri)
                   (eq? errors? (pair? published-errors)))
        (loop))))

  (define (with-project proc)
    (with-temp-dir
      "open-buffer-imports"
      (lambda (root)
        (with-session (make-source-controller #:resyntax #f)
                      (lambda (session) (proc session root))))))

  (test-case "imports read open buffers, and consumers follow their edits and close"
    (with-project
      (lambda (session root)
        (define dependency (build-path root "dependency.rkt"))
        (define consumer (build-path root "consumer.rkt"))
        (write-text dependency (provider "before"))
        (write-text consumer "#lang racket/base\n(require \"dependency.rkt\")\n(after 3)\n")
        (open! session consumer (file->string consumer))
        (await-errors session consumer #t)
        (open! session dependency (provider "after"))
        (await-errors session consumer #f)
        (change! session dependency (provider "before") 1)
        (await-errors session consumer #t)
        (change! session dependency (provider "after") 2)
        (await-errors session consumer #f)
        (close! session dependency)
        (await-errors session consumer #t)
        (check-equal? (file->string dependency) (provider "before")))))

  (test-case "project modules load from source, not from older compiled files"
    (with-project
      (lambda (session root)
        (define (leaf value)
          (format "#lang racket/base\n(provide stamp)\n(define stamp '~a)\n" value))
        (define leaf-path (build-path root "leaf.rkt"))
        (define bridge (build-path root "bridge.rkt"))
        (define consumer (build-path root "consumer.rkt"))
        (write-text leaf-path (leaf "old"))
        (write-text bridge
                    (string-append
                      "#lang racket/base\n"
                      "(require (for-syntax racket/base \"leaf.rkt\"))\n"
                      "(provide stamp)\n"
                      "(define-syntax (bake stx) (datum->syntax stx (list 'quote stamp)))\n"
                      "(define stamp (bake))\n"))
        (parameterize ([current-namespace (make-base-namespace)])
          (managed-compile-zo bridge))
        (define consumer-text
          (string-append
            "#lang racket/base\n"
            "(require (for-syntax racket/base \"bridge.rkt\"))\n"
            "(begin-for-syntax\n"
            "  (unless (eq? stamp 'new) (raise-syntax-error #f \"stale import\" #'stamp)))\n"))
        (write-text consumer consumer-text)
        (open! session consumer consumer-text)
        (await-errors session consumer #t)
        (open! session leaf-path (leaf "new"))
        (await-errors session consumer #f)
        (check-equal? (file->string leaf-path) (leaf "old")))))

  (test-case "a collection path added after startup keeps ordinary loading"
    (with-temp-dir
      "late-collection"
      (lambda (root)
        (define module-file (build-path root "late.rkt"))
        (write-text module-file "#lang racket/base\n")
        (define ordinary-loads '())
        (define ordinary-load (current-load/use-compiled))
        (parameterize ([current-library-collection-paths
                        (cons root (current-library-collection-paths))]
                       [current-load/use-compiled
                        (lambda (file expected)
                          (set! ordinary-loads (cons (normalize-source-path file) ordinary-loads))
                          (ordinary-load file expected))]
                       [current-namespace (make-base-namespace)])
          (call-with-source-snapshot (hash) #:source? (lambda (_) #t)
                                     (lambda () (dynamic-require module-file #f))))
        (check-not-false (member (normalize-source-path module-file) ordinary-loads)))))

  (test-case "a worker killed inside its load record leaves the controller responsive"
    (define entered (make-semaphore 0))
    (struct stuck ()
      #:property prop:equal+hash
      (list (lambda (a b _recur) (eq? a b))
            (lambda (_a _recur) (semaphore-post entered) (sync never-evt))
            (lambda (_a _recur) 0)))
    (define workers (make-async-channel))
    (with-temp-dir
      "open-buffer-kill"
      (lambda (root)
        (define custodian (make-custodian))
        (dynamic-wind
          void
          (lambda ()
            (parameterize ([current-custodian custodian])
              (define controller
                (make-source-controller
                  #:resyntax #f
                  #:analyze (lambda (_snapshot)
                              (async-channel-put workers (current-custodian))
                              (define loaded (Analysis-Context-loaded (current-analysis-context)))
                              (record-load! loaded (stuck) #f))))
              (define (open-buffer name)
                (define path (build-path root name))
                (source-controller-open! controller (path->source path)
                                         (new-safedoc (path->uri path) "#lang racket/base\n" 0)))
              (open-buffer "consumer.rkt")
              (define worker (await workers 30))
              (await entered 30)
              (custodian-shutdown-all worker)
              (define opened (make-async-channel))
              (thread (lambda ()
                        (open-buffer "dependency.rkt")
                        (async-channel-put opened 'returned)))
              (check-eq? (await opened 30) 'returned)))
          (lambda () (custodian-shutdown-all custodian)))))))
