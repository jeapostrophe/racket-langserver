#lang racket/base

(require rackunit
         racket/async-channel
         racket/file
         "../../common/path-util.rkt"
         "../../common/settings.rkt"
         "../../doclib/doc.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/scheduler.rkt"
         "../../lsp/text-document.rkt"
         "../../lsp/workspace.rkt")

(define buffer-text "#lang racket/base\n(define amount 12)\n(displayln amount)\n")
(define edited-text "#lang racket/base\n(define amount 34)\n(displayln amount)\n")

(define (notify-watched! uri . types)
  (didChangeWatchedFiles
    (hasheq 'changes
            (for/list ([type (in-list types)])
              (hasheq 'uri uri 'type type)))))

(define (check-buffer uri opened text version)
  (check-eq? (lsp-get-doc uri #f) opened)
  (with-read-doc opened
    (lambda (doc)
      (check-equal? (doc-get-text doc) text)
      (check-equal? (Doc-version doc) version))))

(define (check-analysis! uri version notify-document!)
  (define analyzed (make-async-channel))
  (notify-document!
    (lambda (method params)
      (when (and (equal? method "textDocument/publishDiagnostics")
                 (equal? (hash-ref params 'uri #f) uri))
        (async-channel-put analyzed (current-thread)))))
  (define worker (sync/timeout 15 analyzed))
  (check-not-false worker "the document notification schedules semantic analysis")
  ;; Diagnostics precede installation of the result. Join the worker so neither
  ;; analysis nor its completion callbacks can race assertions or file cleanup.
  (check-not-false (sync/timeout 15 worker) "semantic analysis finishes")
  (define status
    (with-read-safedoc (lsp-get-doc uri) SafeDoc-check-syntax-status))
  (check-equal? (Check-Syntax-Status-version status) version)
  (check-eq? (Check-Syntax-Status-state status) 'succeeded))

(define (open-buffer! uri text version)
  (check-analysis! uri version
                   (lambda (notify-client)
                     (did-open! void notify-client
                                (hasheq 'textDocument
                                        (hasheq 'uri uri 'languageId "racket"
                                                'version version 'text text))))))

(define (edit-buffer! uri)
  (check-analysis! uri 3
                   (lambda (notify-client)
                     (did-change! notify-client
                                  (hasheq 'textDocument (hasheq 'uri uri 'version 3)
                                          'contentChanges
                                          (list (hasheq 'range
                                                        (hasheq 'start (hasheq 'line 1 'character 15)
                                                                'end (hasheq 'line 1 'character 17))
                                                        'text "34")))))))

(define (with-source proc)
  (define directory (make-temporary-file "watched-files~a" 'directory))
  (define path (build-path directory "source.rkt"))
  (define uri (path->uri path))
  (define resyntax-enabled (get-resyntax-enabled))
  (dynamic-wind
    ;; These tests synchronize check-syntax, not optional Resyntax work.
    (lambda () (set-resyntax-enabled! #f))
    (lambda () (proc path uri))
    (lambda ()
      (lsp-close-doc! uri)
      (set-resyntax-enabled! resyntax-enabled)
      (delete-directory/files directory))))

(define (check-query-action! action close?)
  (with-source
    (lambda (_path uri)
      (define entered (make-async-channel))
      (define release (make-semaphore))
      (define worker #f)
      (define protocol-thread #f)
      (dynamic-wind
        void
        (lambda ()
          (define sd (lsp-open-doc! uri buffer-text 2))
          (async-query-wait
            (SafeDoc-token sd)
            (lambda (signal)
              (when (signal-check-syntax-finished? signal)
                (with-read-doc sd
                  (lambda (_doc)
                    (async-channel-put entered (current-thread))
                    (semaphore-wait release))))))
          (safedoc-run-check-syntax! void sd)
          (set! worker (sync/timeout 20 entered))
          (check-true (thread? worker) "a completion query holds the read lock")
          (set! protocol-thread (thread (lambda () (action uri))))
          (check-eq? (sync/timeout 2 protocol-thread) protocol-thread
                     "the action must finish before the query is released")
          (if close?
              (check-false (lsp-get-doc uri #f))
              (begin
                (check-eq? (lsp-get-doc uri) sd)
                (check-false (thread-dead? worker)
                             "an unrelated event must not cancel the query"))))
        (lambda ()
          (semaphore-post release)
          (when worker (sync/timeout 20 worker))
          (when protocol-thread (sync/timeout 20 protocol-thread)))))))

(module+ test
  (test-case
    "close cancels a completion query before waiting for its read lock"
    (check-query-action! lsp-close-doc! #t))

  (test-case
    "an unrelated watched event does not wait for a busy document lock"
    (check-query-action!
      (lambda (uri) (notify-watched! (string-append uri ".other.rkt") 3))
      #f))

  (test-case
    "delete and recreate on disk preserve the open buffer and its next incremental edit"
    (with-source
      (lambda (path uri)
        (display-to-file "#lang racket/base\n(define amount 0)\n" path)
        (open-buffer! uri buffer-text 2)
        (define opened (lsp-get-doc uri))
        (delete-file path)
        (notify-watched! uri 3)
        (check-buffer uri opened buffer-text 2)
        (display-to-file "#lang racket/base\n(define replacement 99)\n" path)
        (notify-watched! uri 1 2)
        (check-buffer uri opened buffer-text 2)
        (edit-buffer! uri)
        (check-buffer uri opened edited-text 3)
        (check-equal?
          (hash-ref (definition 41
                                (hasheq 'textDocument (hasheq 'uri uri)
                                        'position (hasheq 'line 2 'character 13)))
                    'result)
          (hasheq 'uri uri
                  'range (hasheq 'start (hasheq 'line 1 'character 8)
                                 'end (hasheq 'line 1 'character 14)))))))

  (test-case
    "an open buffer still analyzes after its file is deleted without replacement"
    (with-source
      (lambda (path uri)
        (display-to-file buffer-text path)
        (open-buffer! uri buffer-text 2)
        (define opened (lsp-get-doc uri))
        (delete-file path)
        (notify-watched! uri 3)
        (check-false (file-exists? path))
        (edit-buffer! uri)
        (check-buffer uri opened edited-text 3))))

  (test-case
    "a batched delete-create-change preserves the document and incremental edit coordinates"
    (with-source
      (lambda (_path uri)
        (define opened (lsp-open-doc! uri buffer-text 2))
        (notify-watched! uri 3 1 2)
        (check-buffer uri opened buffer-text 2)
        (edit-buffer! uri)
        (check-buffer uri opened edited-text 3))))

  (test-case
    "a watched change does not close an awaiting query on an open buffer"
    (with-source
      (lambda (_path uri)
        ;; Open without scheduling expansion so the query can only be resolved
        ;; by the notifications under test, not a concurrent syntax check.
        (define opened (lsp-open-doc! uri buffer-text 2))
        (define received-signal #f)
        (define wait-query
          (async-query-wait (SafeDoc-token opened)
                            (lambda (signal)
                              (set! received-signal signal)
                              signal)))
        (notify-watched! uri 2)
        (check-buffer uri opened buffer-text 2)
        (check-false received-signal)
        (edit-buffer! uri)
        (check-true (signal-doc-change? received-signal))
        (check-eq? (wait-query) received-signal)
        (check-buffer uri opened edited-text 3))))

  (test-case
    "watched events do not register unopened documents"
    (with-source
      (lambda (_path uri)
        (for ([type (in-list '(1 2 3 1))])
          (notify-watched! uri type)
          (check-false (lsp-get-doc uri #f))))))

  (test-case
    "malformed watched batches are rejected without changing an open buffer"
    (with-source
      (lambda (_path uri)
        (define opened (lsp-open-doc! uri buffer-text 2))
        (for ([params (in-list
                        (list (hasheq)
                              (hasheq 'changes #f)
                              (hasheq 'changes (list (hasheq 'uri uri)))
                              (hasheq 'changes (list (hasheq 'uri 42 'type 1)))
                              (hasheq 'changes (list (hasheq 'uri uri 'type 9)))
                              (hasheq 'changes (list (hasheq 'uri uri 'type 3)
                                                     (hasheq 'uri uri 'type 9)))))])
          (check-exn exn:fail? (lambda () (didChangeWatchedFiles params)))
          (check-buffer uri opened buffer-text 2)))))

  (test-case
    "mixed URIs and repeated watched events preserve each open buffer independently"
    (with-source
      (lambda (_path uri)
        (define other-uri (string-append uri ".other.rkt"))
        (define unknown-uri (string-append uri ".unknown.rkt"))
        (dynamic-wind
          void
          (lambda ()
            (define opened (lsp-open-doc! uri buffer-text 2))
            (define other (lsp-open-doc! other-uri buffer-text 2))
            (didChangeWatchedFiles
              (hasheq 'changes
                      (for/list ([event (in-list
                                          (list (cons unknown-uri 1)
                                                (cons uri 2)
                                                (cons other-uri 3)
                                                (cons unknown-uri 2)
                                                (cons uri 3)
                                                (cons uri 3)
                                                (cons other-uri 1)
                                                (cons other-uri 1)
                                                (cons uri 1)
                                                (cons unknown-uri 3)
                                                (cons uri 2)))])
                        (hasheq 'uri (car event) 'type (cdr event)))))
            (check-buffer uri opened buffer-text 2)
            (check-buffer other-uri other buffer-text 2)
            (check-false (lsp-get-doc unknown-uri #f))
            (edit-buffer! uri)
            (check-buffer uri opened edited-text 3)
            (check-buffer other-uri other buffer-text 2)
            (edit-buffer! other-uri)
            (check-buffer other-uri other edited-text 3))
          (lambda ()
            (lsp-close-doc! other-uri)
            (lsp-close-doc! unknown-uri))))))

  (test-case
    "only client close and reopen replace an open document"
    (with-source
      (lambda (_path uri)
        (define opened (lsp-open-doc! uri buffer-text 2))
        (notify-watched! uri 2 3 1)
        (check-buffer uri opened buffer-text 2)
        (define closed-signal #f)
        (define wait-query
          (async-query-wait (SafeDoc-token opened)
                            (lambda (signal)
                              (set! closed-signal signal)
                              signal)))
        (did-close! (hasheq 'textDocument (hasheq 'uri uri)))
        (check-false (lsp-get-doc uri #f))
        (check-true (signal-doc-close? closed-signal))
        (check-eq? (wait-query) closed-signal)
        (notify-watched! uri 1 2 3)
        (check-false (lsp-get-doc uri #f))
        (open-buffer! uri edited-text 1)
        (define reopened (lsp-get-doc uri))
        (check-not-eq? reopened opened)
        (check-buffer uri reopened edited-text 1)))))
