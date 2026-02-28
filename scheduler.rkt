#lang racket/base

(require racket/async-channel
         racket/match
         racket/set
         racket/sandbox)

(struct PushTask
  (token type task)
  #:transparent)

(struct RegisterToken
  (token)
  #:transparent)

(struct CloseToken
  (token)
  #:transparent)

(struct StopTokenTasks
  (token)
  #:transparent)

(define (timeout-task time-sec task)
  (λ ()
    (with-handlers ([exn:fail:resource? (λ (_e) (void))])
      (with-limits time-sec #f (task)))))

;; Scheduler

;; Scheduler manages asynchronous cancellable tasks per document token.
;; A token represents one opened document instance.
;; For each token, a task is uniquely identified by its token and type.
;; A new task always replaces the old one that has the same token and type.

(define incoming-jobs-ch (make-async-channel))

(define (kill-running-thread! th)
  (unless (thread-dead? th)
    (kill-thread th)))

(define (cancel-tasks! token->tasks token)
  (define doc-tasks (hash-ref token->tasks token #f))
  (when doc-tasks
    (for ([th (in-hash-values doc-tasks)])
      (kill-running-thread! th))
    (hash-remove! token->tasks token)))

(define (handle-push-task! token->tasks active-tokens token type task)
  (when (set-member? active-tokens token)
    (define doc (hash-ref! token->tasks token make-hash))
    (when (hash-has-key? doc type)
      (kill-running-thread! (hash-ref doc type)))
    ;; Each scheduled task is bounded to avoid zombie long-running jobs.
    (define task/timeout (timeout-task 90 task))
    (hash-set! doc type (thread task/timeout))))

;; new incoming task will replace the old task immediately
;; no matter if the old one is running or completed
(define (schedule)
  (define token->tasks (make-hash))
  (define active-tokens (mutable-set))
  (let loop ()
    (define job (async-channel-get incoming-jobs-ch))
    (match job
      [(PushTask token type task)
       (handle-push-task! token->tasks active-tokens token type task)]
      [(RegisterToken token)
       (cancel-tasks! token->tasks token)
       (set-add! active-tokens token)]
      [(CloseToken token)
       (cancel-tasks! token->tasks token)
       (set-remove! active-tokens token)]
      [(StopTokenTasks token)
       (cancel-tasks! token->tasks token)])
    (loop)))

(define _scheduler (thread schedule))

(define (scheduler-register-doc! token)
  (async-channel-put incoming-jobs-ch (RegisterToken token)))

(define (scheduler-close-doc! token)
  (async-channel-put incoming-jobs-ch (CloseToken token)))

(define (scheduler-stop-all-tasks! token)
  ;; Stop all running tasks for this token, but keep token active for future tasks.
  (async-channel-put incoming-jobs-ch (StopTokenTasks token)))

(define (scheduler-push-task! token type task)
  (async-channel-put incoming-jobs-ch (PushTask token type task)))

(provide scheduler-register-doc!
         scheduler-close-doc!
         scheduler-stop-all-tasks!
         scheduler-push-task!)

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

(define (run-and-remove-queries token signal)
  (for ([data (hash-ref *await-queries* token '())])
    (match-define (list task ch) data)
    (async-channel-put ch (task signal)))
  (hash-remove! *await-queries* token))

(define (async-query-wait token task)
  (define query-ch (make-async-channel))
  (call-with-semaphore
    *await-queries-semaphore*
    (λ ()
      (hash-update! *await-queries*
                    token
                    (λ (old) (cons (list task query-ch) old))
                    '())))

  (λ () (sync query-ch)))

;; send doc change event signal and waiting for all waiting queries
;; to be processed.
(define (clear-old-queries/doc-change token)
  (call-with-semaphore
    *await-queries-semaphore*
    (λ ()
      (run-and-remove-queries token *doc-change-signal*))))

;; send new trace signal (when check syntax completed) and waiting for all waiting queries
;; to be processed.
(define (clear-old-queries/new-trace token)
  (call-with-semaphore
    *await-queries-semaphore*
    (λ ()
      (run-and-remove-queries token *new-trace-signal*))))

;; remove all await queries
(define (clear-old-queries/doc-close token)
  (call-with-semaphore
    *await-queries-semaphore*
    (λ ()
      (hash-remove! *await-queries* token))))

(provide async-query-wait
         signal-doc-change?
         signal-new-trace?
         clear-old-queries/doc-change
         clear-old-queries/new-trace
         clear-old-queries/doc-close)

