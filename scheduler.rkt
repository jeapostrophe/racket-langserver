#lang racket/base

(require racket/async-channel
         racket/match
         racket/sandbox
         "version.rkt")

(define (timeout-task time-sec task)
  (λ ()
    (with-limits time-sec #f (task))))

;; Scheduler

;; Scheduler manages a list of asynchronous and cancellable tasks for each document.
;; For each document, a task is uniquely identified by its key.
;; A new task always replaces the old one that has the same key.

(define incoming-jobs-ch (make-async-channel))

;; new incoming task will replace the old task immediately
;; no matter if the old one is running or completed
(define (schedule)
  ;; TODO: allow cancelling running tasks
  (define documents (make-hash))
  (let loop ()
    (sync (handle-evt incoming-jobs-ch
            (λ (data)
              (match-define (list uri type task) data)
              (unless (hash-has-key? documents uri)
                (hash-set! documents uri (make-hash)))
              (define doc (hash-ref documents uri))

              (when (hash-has-key? doc type)
                (define th (hash-ref doc type))
                (unless (thread-dead? th)
                  (kill-thread th)))
              (define timeout-task90 (timeout-task 90 task))
              (hash-set! doc type
                         (if (version>=9.0?)
                             (thread #:pool 'own timeout-task90)
                             (thread timeout-task90))))))
    (loop)))

(define _scheduler (thread schedule))

(define (scheduler-push-task! uri type task)
  (async-channel-put incoming-jobs-ch (list uri type task)))

(provide scheduler-push-task!)

;; schedule queries

;; All awaiting queries are run before next document change
;; or after a new check syntax is completed.
;; A signal is sent to indicate an event, then queries are
;; either run or cleared depending on the signal type.

(define *await-queries* (make-hash))
(define *await-queries-semaphore* (make-semaphore 1))

(struct QuerySignal
  ())

(define *doc-change-signal* (QuerySignal))
(define *new-trace-signal* (QuerySignal))

(define (signal-doc-change? s)
  (eq? s *doc-change-signal*))

(define (signal-new-trace? s)
  (eq? s *new-trace-signal*))

(define (run-and-remove-queries uri signal)
  (for ([data (hash-ref *await-queries* uri '())])
    (match-define (list task ch) data)
    (async-channel-put ch (task signal)))
  (hash-remove! *await-queries* uri))

(define (async-query-wait uri task)
  (define query-ch (make-async-channel))
  (call-with-semaphore
    *await-queries-semaphore*
    (λ ()
      (hash-update! *await-queries*
                    uri
                    (λ (old) (cons (list task query-ch) old))
                    '())))

  (λ () (sync query-ch)))

;; send doc change event signal and waiting for all waiting queries
;; to be processed.
(define (clear-old-queries/doc-change uri)
  (call-with-semaphore
    *await-queries-semaphore*
    (λ ()
      (run-and-remove-queries uri *doc-change-signal*))))

;; send new trace signal (when check syntax completed) and waiting for all waiting queries
;; to be processed.
(define (clear-old-queries/new-trace uri)
  (call-with-semaphore
    *await-queries-semaphore*
    (λ ()
      (run-and-remove-queries uri *new-trace-signal*))))

;; remove all await queries
(define (clear-old-queries/doc-close uri)
  (call-with-semaphore
    *await-queries-semaphore*
    (λ ()
      (hash-remove! *await-queries* uri))))

(provide async-query-wait
         signal-doc-change?
         signal-new-trace?
         clear-old-queries/doc-change
         clear-old-queries/new-trace
         clear-old-queries/doc-close)

