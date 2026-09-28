#lang racket/base

(require rackunit
         racket/async-channel
         racket/file
         "../../common/path-util.rkt"
         "../../common/settings.rkt"
         "../../doclib/doc.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/text-document.rkt"
         "analysis-test-support.rkt")

(define source "#lang racket/base\n(require racket/list)\nfirst\n")
(define analysis-started
  (make-log-receiver (current-logger) 'info 'analysis-failure-test))
(define (failure-source mode)
  (string-append
    source
    "(require (for-syntax racket/base))\n"
    "(begin-for-syntax\n"
    "  (define release (make-semaphore))\n"
    "  (log-message (current-logger) 'info 'analysis-failure-test \"started\" release)\n"
    "  (semaphore-wait release)\n"
    (if (eq? mode 'raised-value) "  (raise 'analysis-failure))\n" ")\n")))

(module+ test
  (for ([mode '(raised-value publication-error)])
    (test-case
      (format "analysis ~a releases requests and retains accepted facts" mode)
      (define directory (make-temporary-file "analysis-failure~a" 'directory))
      (define path (build-path directory "source.rkt"))
      (define uri (path->uri path))
      (define enabled (get-resyntax-enabled))
      (define waiters '())
      (define release #f)
      (dynamic-wind
        void
        (lambda ()
          (set-resyntax-enabled! #f)
          (display-to-file source path)
          (define sd (lsp-open-doc! uri source 2))
          (analyze! sd)
          (wait-for-disk-verification! sd)
          (define accepted (with-read-doc sd Doc-contribution))
          (with-write-doc sd
            (lambda (doc)
              (doc-reset! doc (failure-source mode))
              (doc-update-version! doc 3)))
          ;; Hold expansion at the fixture's semaphore until both requests are
          ;; registered, without suspending the scheduler's async-channel wait.
          (safedoc-run-check-syntax!
            (if (eq? mode 'publication-error)
                (lambda (_method _params) (error 'test "diagnostic delivery failed"))
                void)
            sd)
          (define started (sync/timeout 20 analysis-started))
          (check-not-false started "analysis reached the expansion gate")
          (set! release (vector-ref started 2))
          (define responses (make-async-channel))
          (define replies
            (list
              (full-semantic-tokens 1 (hasheq 'textDocument (hasheq 'uri uri)))
              (inlay-hint 2
                          (hasheq 'textDocument (hasheq 'uri uri)
                                  'range (hasheq 'start (hasheq 'line 0 'character 0)
                                                 'end (hasheq 'line 1 'character 0))))))
          (for ([reply (in-list replies)])
            (check-true (procedure? reply))
            (set! waiters
                  (cons (thread (lambda () (async-channel-put responses (reply)))) waiters)))
          (semaphore-post release)
          (for ([_ (in-list replies)])
            (define reply (sync/timeout 20 responses))
            (check-true (hash? reply) "analysis failure releases the pending request")
            (check-true (hash-has-key? reply 'result)))
          (check-eq? (Check-Syntax-Status-state
                       (with-read-safedoc sd SafeDoc-check-syntax-status))
                     'failed)
          (check-eq? (with-read-doc sd Doc-contribution) accepted)
          (check-false (procedure?
                         (full-semantic-tokens 3 (hasheq 'textDocument (hasheq 'uri uri))))))
        (lambda ()
          (when release (semaphore-post release))
          (lsp-close-doc! uri)
          (for ([waiter (in-list waiters)]) (sync/timeout 5 waiter))
          (set-resyntax-enabled! enabled)
          (delete-directory/files directory))))))
