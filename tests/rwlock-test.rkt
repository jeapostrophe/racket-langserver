#lang racket/base

(module+ test
  (require rackunit
           "../common/rwlock.rkt")

  (define (acquire-write-with-timeout rwlock [timeout 1.0])
    (define result-ch (make-channel))
    (thread
      (lambda ()
        (call-with-write-lock rwlock
          (lambda ()
            (channel-put result-ch #t)))))
    (sync/timeout timeout result-ch))

  ;; A break must not abort the writer while it is inside call-with-write-lock.
  (test-case "write lock critical section is non-breakable"
    (define rwlock (make-rwlock))
    (define entered (make-semaphore 0))
    (define done (make-semaphore 0))

    (define worker
      (thread
        (lambda ()
          (call-with-write-lock rwlock
            (lambda ()
              (semaphore-post entered)
              (semaphore-wait done))))))

    (check-not-false (sync/timeout 1.0 entered))
    (break-thread worker)
    (sync (system-idle-evt))
    (check-false (thread-dead? worker))

    (semaphore-post done)
    (check-not-false (sync/timeout 1.0 (thread-dead-evt worker)))
    (check-true (acquire-write-with-timeout rwlock)))

  ;; Read thunks are breakable, but bookkeeping must still clean up without printing `user break`.
  (test-case "read lock bookkeeping is released when break aborts protected thunk"
    (define rwlock (make-rwlock))
    (define entered (make-semaphore 0))

    (define worker
      (thread
        (lambda ()
          (with-handlers ([exn:break? void])
            (call-with-read-lock rwlock
              (lambda ()
                (semaphore-post entered)
                (sync never-evt)))))))

    (check-not-false (sync/timeout 1.0 entered))
    (break-thread worker)
    (check-not-false (sync/timeout 1.0 (thread-dead-evt worker)))
    (check-true (acquire-write-with-timeout rwlock))))
