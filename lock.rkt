#lang racket/base

;; A Readers Writer Lock
(struct RWLock
  (sema wsema readers)
  #:mutable)

(define (make-rwlock)
  (RWLock (make-semaphore 1) (make-semaphore 1) 0))

(define (call-with-read-lock rwlock proc)
  (define sema (RWLock-sema rwlock))
  (define write (RWLock-wsema rwlock))

  (semaphore-wait sema)
  (when (= 0 (RWLock-readers rwlock))
    (semaphore-wait write))
  (set-RWLock-readers! rwlock (add1 (RWLock-readers rwlock)))
  (semaphore-post sema)

  (define results (call-with-values proc list))

  (semaphore-wait sema)
  (set-RWLock-readers! rwlock (sub1 (RWLock-readers rwlock)))
  (when (= 0 (RWLock-readers rwlock))
    (semaphore-post write))
  (semaphore-post sema)
  (apply values results))

(define (call-with-write-lock rwlock proc)
  (define write (RWLock-wsema rwlock))
  (semaphore-wait write)
  (define results (call-with-values proc list))
  (semaphore-post write)
  (apply values results))

(provide make-rwlock
         call-with-read-lock
         call-with-write-lock)

