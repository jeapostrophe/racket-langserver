#lang racket/base

(require racket/async-channel
         racket/match)

;; schedule check syntax

(define incoming-jobs-ch (make-async-channel))

;; new incoming task will replace the old task immediately
;; no matter if the old one is running or completed
(define (schedule)
  (define open-doc (make-hash))
  (let loop ()
    (sync (handle-evt incoming-jobs-ch
                      (λ (data)
                        (match-define (cons path task) data)
                        (when (hash-has-key? open-doc path)
                          (define th (hash-ref open-doc path))
                          (unless (thread-dead? th)
                            (kill-thread th)))
                        (hash-set! open-doc path
                                   (thread task)))))
    (loop)))

(define _scheduler (thread schedule))

(define (scheduler-push-task! path task)
  (async-channel-put incoming-jobs-ch (cons path task)))

(provide scheduler-push-task!)

;; schedule queries

;; All awaiting queries are run before next document change
;; or after a new check syntax is completed.

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

