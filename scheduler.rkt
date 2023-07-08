#lang racket/base

(require racket/async-channel
         racket/match)

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
                          (when (not (thread-dead? th))
                            (kill-thread th)))
                        (hash-set! open-doc path
                                   (thread task)))))
    (loop)))

(define _scheduler (thread schedule))

(define (scheduler-push-task! path task)
  (async-channel-put incoming-jobs-ch (cons path task)))

(provide scheduler-push-task!)
