#lang racket/base

(require racket/async-channel
         racket/port
         "../../common/time.rkt")

(provide Supervised-Worker? start-supervised-worker! stop-supervised-worker!)

;; Only this module can access the worker's resource boundary. The owner serializes stops.
(struct Supervised-Worker (custodian))

(define (stop-supervised-worker! worker)
  (custodian-shutdown-all (Supervised-Worker-custodian worker)))

;; launch runs inside the resource boundary, including any parallel-thread pool it creates.
;; The supervisor stays outside: work may shut down its own custodian before reporting.
;; Its deadline uses timeout-result; raised values always go through fail, even
;; when they resemble the timeout result.
(define (start-supervised-worker! events seconds work
                                  #:launch [launch thread]
                                  #:fail fail
                                  #:exited exited
                                  #:timeout-result timeout-result
                                  #:done done)
  (define handle (Supervised-Worker (make-custodian)))
  (define deadline (+ (current-deadline-milliseconds) (* 1000 seconds)))
  (define result-ch (make-async-channel))
  (define worker
    (parameterize ([current-custodian (Supervised-Worker-custodian handle)]
                   [current-output-port (open-output-nowhere)])
      (launch (lambda ()
                (async-channel-put result-ch
                                   (with-handlers ([(lambda (_) #t) fail])
                                     (work)))))))
  (thread
    (lambda ()
      (define wake
        (sync (wrap-evt result-ch box) (thread-dead-evt worker) (deadline-evt deadline)))
      ;; Observe death before polling: a worker that has exited cannot enqueue
      ;; another result. Queued results win over death or expiry, including #f.
      (define exited? (thread-dead? worker))
      (define result
        (if (box? wake) wake (sync/timeout 0 (wrap-evt result-ch box))))
      (define outcome
        (cond
          [result (unbox result)]
          [exited? (fail exited)]
          [else (timeout-result)]))
      ;; Release resources before notifying the owner, even if done itself raises.
      (stop-supervised-worker! handle)
      (async-channel-put events (done outcome))))
  handle)
