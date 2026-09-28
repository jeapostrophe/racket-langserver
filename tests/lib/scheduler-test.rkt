#lang racket

(module+ test
  (require rackunit
           racket/sandbox
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
    "failed computations report exceptions and arbitrary raised values"
    (for ([failure (list (exn:fail "analysis error" (current-continuation-marks)) 'analysis-error)])
      (define received #f)
      (define published? #f)
      ((handle-timeout-or-break
         1 (lambda () (raise failure))
         (lambda _ (set! published? #t))
         (lambda (e) (set! received e))))
      (check-eq? received failure)
      (check-false published?)))

  (test-case
    "timeout failure publication survives the computation time limit"
    (define entered (make-semaphore))
    (define release (make-semaphore))
    (define received #f)
    (define worker
      (thread
        (handle-timeout-or-break
          0.1 (lambda () (sync never-evt))
          (lambda _ (fail "timed-out computation was published"))
          (lambda (e)
            (set! received e)
            (semaphore-post entered)
            (semaphore-wait release)))))
    (dynamic-wind
      void
      (lambda ()
        (check-not-false (sync/timeout 5 entered))
        (check-true (exn:fail:resource? received))
        (check-false (sync/timeout 0.2 worker))
        (semaphore-post release)
        (check-eq? (sync/timeout 5 worker) worker))
      (lambda () (semaphore-post release) (sync/timeout 5 worker))))

  (test-case
    "cancelling a computation does not report failure or completion"
    (define entered (make-semaphore))
    (define called? #f)
    (define worker
      (thread
        (handle-timeout-or-break
          5 (lambda () (semaphore-post entered) (sync never-evt))
          (lambda _ (set! called? #t))
          (lambda (_e) (set! called? #t)))))
    (check-not-false (sync/timeout 5 entered))
    (break-thread worker)
    (check-eq? (sync/timeout 5 worker) worker)
    (check-false called?))

  (test-case
    "cancelling failure completion releases a document read lock"
    (define lock (make-rwlock))
    (define entered (make-semaphore))
    (define release (make-semaphore))
    (define worker
      (thread
        (handle-timeout-or-break
          1 (lambda () (error 'test "analysis failed")) void
          (lambda (_e)
            (call-with-read-lock lock
              (lambda ()
                (semaphore-post entered)
                (semaphore-wait release)))))))
    (dynamic-wind
      void
      (lambda ()
        (check-not-false (sync/timeout 5 entered))
        (break-thread worker)
        (check-eq? (sync/timeout 2 worker) worker
                   "cancellation must finish before the read gate is released")
        (define writer (thread (lambda () (call-with-write-lock lock void))))
        (check-eq? (sync/timeout 2 writer) writer))
      (lambda () (semaphore-post release) (sync/timeout 5 worker))))

  (test-case
    "a query registered after completion is answered immediately"
    (define token (gensym 'completed-doc))
    (clear-old-queries/check-syntax-finished token)
    (define response
      (async-query-wait token signal-check-syntax-finished? #:ready? (lambda () #t)))
    (define result (make-channel))
    (define waiter (thread (lambda () (channel-put result (response)))))
    (dynamic-wind
      void
      (lambda () (check-true (sync/timeout 2 result)))
      (lambda () (clear-old-queries/doc-close token) (kill-thread waiter))))

  (test-case
    "a superseded completion leaves a newer run's query queued"
    (define token (gensym 'superseded-doc))
    (define observed #f)
    (define response
      (async-query-wait token (lambda (signal) (set! observed signal) signal)))
    (clear-old-queries/check-syntax-finished token #:ready? (lambda () #f))
    (check-false observed)
    (clear-old-queries/check-syntax-finished token #:ready? (lambda () #t))
    (check-true (signal-check-syntax-finished? observed))
    (check-eq? (response) observed))

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
