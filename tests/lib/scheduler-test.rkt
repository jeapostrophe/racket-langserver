#lang racket

(module+ test
  (require rackunit
           "../../lsp/scheduler.rkt"
           (submod "../../lsp/scheduler.rkt" test-support)
           "../../common/rwlock.rkt")

  (test-case
    "a computation timeout cannot publish partial results"
    (define entered (make-semaphore))
    (define published? #f)
    (define worker
      (thread
        (handle-timeout-or-break
          0.1
          (lambda ()
            (semaphore-post entered)
            (sync never-evt))
          (lambda _results (set! published? #t)))))
    (check-not-false (sync/timeout 5 entered))
    (check-eq? (sync/timeout 5 worker) worker)
    (check-false published?))

  (test-case
    "publication is outside the hard timeout and releases its lock after cancellation"
    (define custodian (make-custodian))
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([current-custodian custodian])
          (define lock (make-rwlock))
          (define entered (make-semaphore))
          (define release (make-semaphore))
          (define worker
            (thread
              (handle-timeout-or-break
                0.1
                (lambda () (values 'result 'contribution))
                (lambda (result contribution)
                  (call-with-write-lock lock
                    (lambda ()
                      (check-eq? result 'result)
                      (check-eq? contribution 'contribution)
                      (semaphore-post entered)
                      (semaphore-wait release)))))))
          (check-not-false (sync/timeout 5 entered))
          ;; Longer than the private computation limit: the publisher survives.
          (check-false (sync/timeout 0.3 worker))
          (break-thread worker)
          (semaphore-post release)
          (check-eq? (sync/timeout 5 worker) worker)
          (define reader (thread (lambda () (call-with-read-lock lock void))))
          (check-eq? (sync/timeout 5 reader) reader)))
      (lambda () (custodian-shutdown-all custodian))))

  (test-case
    "clear-old-queries/check-syntax-finished releases waiting queries"
    (define token (gensym 'doc-token))
    (define waiter
      (async-query-wait token (lambda (signal) signal)))
    (define result-box (box #f))
    (define waiter-thread
      (thread
        (lambda ()
          (set-box! result-box (waiter)))))

    (clear-old-queries/check-syntax-finished token)

    (check-not-false (sync/timeout 1.0 waiter-thread))
    (check-true (signal-check-syntax-finished? (unbox result-box))))

  (test-case
    "clear-old-queries/doc-close releases waiting queries"
    (define token (gensym 'doc-token))
    (define waiter
      (async-query-wait token (lambda (signal) signal)))
    (define result-box (box #f))
    (define waiter-thread
      (thread
        (lambda ()
          (set-box! result-box (waiter)))))

    (clear-old-queries/doc-close token)

    (check-not-false (sync/timeout 1.0 waiter-thread))
    (check-true (signal-doc-close? (unbox result-box)))))
