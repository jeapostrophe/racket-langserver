#lang racket/base

(require rackunit
         racket/async-channel
         racket/file
         "../../common/path-util.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/scheduler.rkt"
         (submod "../../lsp/scheduler.rkt" test-support)
         "../../lsp/text-document.rkt")

(require/expose "../../lsp/safedoc.rkt" (publish-analysis!))
(require/expose "../../lsp/scheduler.rkt" (_scheduler *await-queries-semaphore*))

(define scheduler-custodian (current-custodian))
(define (suspend-scheduler!)
  (parameterize ([current-custodian scheduler-custodian])
    (thread-suspend _scheduler)))

(define (with-suspended-document proc)
  (define path (make-temporary-file "analysis-publication~a.rkt"))
  (define uri (path->uri path))
  (dynamic-wind
    (lambda () (suspend-scheduler!))
    (lambda () (proc (lsp-open-doc! uri "#lang racket/base\n(define x 1)\n" 1) uri))
    (lambda ()
      (lsp-close-doc! uri)
      (thread-resume _scheduler)
      (delete-file path))))

(define (start! sd)
  (safedoc-run-check-syntax! void sd)
  (with-read-safedoc sd SafeDoc-check-syntax-status))

(module+ test
  (test-case
    "superseded same-version success and failure cannot release newer waiters"
    (with-suspended-document
      (lambda (sd _uri)
        (define old (start! sd))
        (define current (start! sd))
        (check-false (eq? old current))
        (define observed #f)
        (define response
          (async-query-wait (SafeDoc-token sd)
                            (lambda (signal) (set! observed signal) signal)))
        (for ([state '(succeeded failed)])
          (publish-analysis! sd old state
                             (lambda (_sd) (fail "superseded analysis updated the document")))
          (check-eq? (with-read-safedoc sd SafeDoc-check-syntax-status) current)
          (check-false observed))
        (publish-analysis! sd current 'failed void)
        (check-true (signal-check-syntax-finished? observed))
        (check-eq? (response) observed))))

  (test-case
    "an accepted completion cannot drain a replacement run's newer query"
    (with-suspended-document
      (lambda (sd _uri)
        (define old (start! sd))
        (define gate-held? #f)
        (define completer #f)
        (dynamic-wind
          void
          (lambda ()
            (semaphore-wait *await-queries-semaphore*)
            (set! gate-held? #t)
            (set! completer (thread (lambda () (publish-analysis! sd old 'succeeded void))))
            (check-not-false (sync/timeout 5 (system-idle-evt)))
            (check-eq? (Check-Syntax-Status-state
                         (with-read-safedoc sd SafeDoc-check-syntax-status))
                       'succeeded)
            (check-false (thread-dead? completer))
            (thread-suspend completer)
            (define current (start! sd))
            (semaphore-post *await-queries-semaphore*)
            (set! gate-held? #f)
            (define observed #f)
            (define response
              (async-query-wait (SafeDoc-token sd)
                                (lambda (signal) (set! observed signal) signal)))
            (thread-resume completer)
            (check-eq? (sync/timeout 5 completer) completer)
            (check-false observed "old completion must leave the new waiter queued")
            (check-eq? (with-read-safedoc sd SafeDoc-check-syntax-status) current)
            (publish-analysis! sd current 'failed void)
            (check-true (signal-check-syntax-finished? (response))))
          (lambda ()
            (when gate-held? (semaphore-post *await-queries-semaphore*))
            (when completer
              (thread-resume completer)
              (sync/timeout 5 completer)))))))

  (test-case
    "a retired lifetime rejects late failure publication"
    (with-suspended-document
      (lambda (sd uri)
        (define running (start! sd))
        (lsp-close-doc! uri)
        (define reopened (lsp-open-doc! uri "#lang racket/base\n" 1))
        (define current (start! reopened))
        (define observed #f)
        (async-query-wait (SafeDoc-token reopened)
                          (lambda (signal) (set! observed signal)))
        (publish-analysis! sd running 'failed
                           (lambda (_sd) (fail "retired analysis updated the document")))
        (check-eq? (with-read-safedoc reopened SafeDoc-check-syntax-status) current)
        (check-false observed))))

  (test-case
    "a real computation timeout completes document status and waiting queries"
    (with-suspended-document
      (lambda (sd uri)
        (define running (start! sd))
        (define response (full-semantic-tokens 1 (hasheq 'textDocument (hasheq 'uri uri))))
        (check-true (procedure? response))
        (define worker
          (thread
            (handle-timeout-or-break
              0.1 (lambda () (sync never-evt))
              (lambda _ (fail "timed-out computation published success"))
              (lambda (_e) (publish-analysis! sd running 'failed void)))))
        (check-eq? (sync/timeout 5 worker) worker)
        (check-eq? (Check-Syntax-Status-state
                     (with-read-safedoc sd SafeDoc-check-syntax-status))
                   'failed)
        (check-true (hash-has-key? (response) 'result)))))

  (for ([method '(tokens hints)])
    (test-case
      (format "~a registration after the completion drain cannot lose its response" method)
      (with-suspended-document
        (lambda (sd uri)
          (define running (start! sd))
          (define responses (make-async-channel))
          (define gate-held? #f)
          (define requester #f)
          (define completer #f)
          (dynamic-wind
            void
            (lambda ()
              (semaphore-wait *await-queries-semaphore*)
              (set! gate-held? #t)
              (set! requester
                    (thread
                      (lambda ()
                        (define reply
                          (if (eq? method 'tokens)
                              (full-semantic-tokens 1 (hasheq 'textDocument (hasheq 'uri uri)))
                              (inlay-hint 1
                                          (hasheq 'textDocument (hasheq 'uri uri)
                                                  'range
                                                  (hasheq 'start (hasheq 'line 0 'character 0)
                                                          'end (hasheq 'line 1 'character 0))))))
                        (async-channel-put responses (if (procedure? reply) (reply) reply)))))
              ;; The requester has read 'running and is parked at registration.
              (check-not-false (sync/timeout 5 (system-idle-evt)))
              (check-false (thread-dead? requester))
              (thread-suspend requester)
              (set! completer (thread (lambda () (publish-analysis! sd running 'failed void))))
              (check-not-false (sync/timeout 5 (system-idle-evt)))
              (semaphore-post *await-queries-semaphore*)
              (set! gate-held? #f)
              (check-eq? (sync/timeout 5 completer) completer)
              (thread-resume requester)
              (define reply (sync/timeout 5 responses))
              (check-true (hash? reply) "registration must notice completed analysis")
              (check-true (hash-has-key? reply 'result)))
            (lambda ()
              (when gate-held? (semaphore-post *await-queries-semaphore*))
              (when requester (thread-resume requester))
              (lsp-close-doc! uri)
              (when requester (sync/timeout 5 requester))
              (when completer (sync/timeout 5 completer)))))))))
