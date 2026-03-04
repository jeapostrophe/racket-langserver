#lang racket/base

;; A Readers Writer Lock
(struct RWLock
  (access-sema write-sema readers)
  #:mutable)

(define (make-rwlock)
  (RWLock (make-semaphore 1) (make-semaphore 1) 0))

;; Read critical section is breakable, but the bookkeeping of lock state is non-breakable.
(define (call-with-read-lock rwlock proc)
  (define access (RWLock-access-sema rwlock))
  (define write (RWLock-write-sema rwlock))
  (define lock-held? #f)

  (define (acquire!)
    (when (= 0 (RWLock-readers rwlock))
      (semaphore-wait write))
    (set-RWLock-readers! rwlock (add1 (RWLock-readers rwlock)))
    (set! lock-held? #t))

  (define (release!)
    (when lock-held?
      (set-RWLock-readers! rwlock (sub1 (RWLock-readers rwlock)))
      (when (= 0 (RWLock-readers rwlock))
        (semaphore-post write))))

  (dynamic-wind
    void
    (λ ()
      (parameterize-break #f
        (call-with-semaphore access acquire!))
      (proc))
    (λ ()
      (parameterize-break #f
        (call-with-semaphore access release!)))))

;; Write critical section is non-breakable.
(define (call-with-write-lock rwlock proc)
  (parameterize-break #f
    (call-with-semaphore
      (RWLock-write-sema rwlock)
      proc)))

(provide make-rwlock
         call-with-read-lock
         call-with-write-lock)

