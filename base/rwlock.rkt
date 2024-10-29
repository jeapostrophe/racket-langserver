#lang racket/base

;; A Readers Writer Lock
(struct RWLock
  (access-sema write-sema readers)
  #:mutable)

(define (make-rwlock)
  (RWLock (make-semaphore 1) (make-semaphore 1) 0))

(define (call-with-read-lock rwlock proc)
  (define access (RWLock-access-sema rwlock))
  (define write (RWLock-write-sema rwlock))

  (semaphore-wait access)
  (when (= 0 (RWLock-readers rwlock))
    (semaphore-wait write))
  (set-RWLock-readers! rwlock (add1 (RWLock-readers rwlock)))
  (semaphore-post access)

  (define results (call-with-values proc list))

  (semaphore-wait access)
  (set-RWLock-readers! rwlock (sub1 (RWLock-readers rwlock)))
  (when (= 0 (RWLock-readers rwlock))
    (semaphore-post write))
  (semaphore-post access)
  (apply values results))

(define (call-with-write-lock rwlock proc)
  (define write (RWLock-write-sema rwlock))
  (semaphore-wait write)
  (define results (call-with-values proc list))
  (semaphore-post write)
  (apply values results))

(provide make-rwlock
         call-with-read-lock
         call-with-write-lock)

