#lang racket/base

;; Analysis reads an imported module from its open buffer, including unsaved
;; edits, and analyses again the open documents that loaded it when that
;; buffer opens, changes or closes.
(require rackunit
         racket/file
         compiler/cm
         "../../common/path-util.rkt"
         "../../common/time.rkt"
         "../../lsp/source-controller.rkt"
         "controller-support.rkt")

(define (write-text path text)
  (display-to-file text path #:exists 'truncate))

(define (provider name)
  (format "#lang racket/base\n(provide ~a)\n(define (~a x) (+ x 1))\n" name name))

;; Wait for diagnostics published for `path` that do, or do not, include an
;; error. Earlier publications for the same document are skipped.
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

;; The project lives outside every collection directory: modules below one
;; keep compiled loading.
(define (with-project proc)
  (define root (make-temporary-file "open-buffer-imports~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (with-session (make-source-controller #:resyntax #f)
                    (lambda (session) (proc session root))))
    (lambda () (delete-directory/files root))))

(module+ test
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
        ;; Closing the buffer returns the import to the file on disk.
        (close! session dependency)
        (await-errors session consumer #t)
        (check-equal? (file->string dependency) (provider "before")))))

  (test-case "project modules load from source, not from older compiled files"
    (with-project
      (lambda (session root)
        (define (leaf value) (format "#lang racket/base\n(provide stamp)\n(define stamp '~a)\n" value))
        (define leaf-path (build-path root "leaf.rkt"))
        (define bridge (build-path root "bridge.rkt"))
        (define consumer (build-path root "consumer.rkt"))
        (write-text leaf-path (leaf "old"))
        ;; The compiled bridge keeps the value of `stamp` it saw at compile time.
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
        (check-equal? (file->string leaf-path) (leaf "old"))))))
