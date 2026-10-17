#lang racket/base

;; Bounded private workers for disk reads and analysis jobs.
(require racket/async-channel
         racket/port
         "../../common/time.rkt")

(provide start-worker!)

;; Runs `work` on a thread made by `launch` under `custodian`, with output sent
;; nowhere: workers must not write the LSP stdout, even when the consumer's own
;; redirection changes. A watcher posts `(done outcome)` to `events`, where
;; outcome is the result or `fail` applied to the raised value, `exited` or
;; `timed-out`. The watcher then shuts the custodian down without waiting for
;; the consumer, so a timed-out worker cannot keep running.
(define (start-worker! events custodian seconds work
                       #:launch [launch thread]
                       #:fail fail
                       #:exited exited
                       #:timed-out timed-out
                       #:done done)
  (define deadline (+ (current-deadline-milliseconds) (* 1000 seconds)))
  (define result-ch (make-async-channel))
  (parameterize ([current-custodian custodian]
                 [current-output-port (open-output-nowhere)])
    (define worker
      (launch (lambda ()
                (async-channel-put result-ch (with-handlers ([(lambda (_) #t) fail])
                                               (work))))))
    (thread
      (lambda ()
        (define outcome
          (sync result-ch
                (wrap-evt (thread-dead-evt worker)
                          (lambda (_) (or (sync/timeout 0 result-ch) (fail exited))))
                (wrap-evt (deadline-evt deadline) (lambda (_) (fail timed-out)))))
        (async-channel-put events (done outcome))
        (custodian-shutdown-all custodian)))))
