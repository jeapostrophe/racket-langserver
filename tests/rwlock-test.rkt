#lang racket

(require rackunit
         "../common/rwlock.rkt")

(module+ test
  (define (acquire-write-with-timeout rwlock [timeout 1.0])
    (define result-ch (make-channel))
    (thread
      (λ ()
        (call-with-write-lock rwlock
          (λ ()
            (channel-put result-ch #t)))))
    (sync/timeout timeout result-ch))

  ;; A break must not abort the writer while it's inside call-with-write-lock.
  (test-case "write lock critical section is non-breakable"
    (define rwlock (make-rwlock))
    (define entered (make-semaphore 0))
    (define done (make-semaphore 0))

    (define t
      (thread
        (λ ()
          (call-with-write-lock rwlock
            (λ ()
              (semaphore-post entered)
              (semaphore-wait done))))))

    (check-not-false (sync/timeout 1.0 entered))
    (break-thread t)
    (check-false (sync/timeout 0.2 (thread-dead-evt t)))

    (semaphore-post done)
    (check-not-false (sync/timeout 1.0 (thread-dead-evt t)))
    (check-true (acquire-write-with-timeout rwlock)))

  ;; Read thunks are breakable, but bookkeeping must still clean up correctly.
  (test-case "read lock bookkeeping is released when break aborts protected thunk"
    (define rwlock (make-rwlock))
    (define entered (make-semaphore 0))

    (define t
      (thread
        (λ ()
          (call-with-read-lock rwlock
            (λ ()
              (semaphore-post entered)
              (sync never-evt))))))

    (check-not-false (sync/timeout 1.0 entered))
    (break-thread t)
    (check-not-false (sync/timeout 1.0 (thread-dead-evt t)))
    (check-true (acquire-write-with-timeout rwlock))))
