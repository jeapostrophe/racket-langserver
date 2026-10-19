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
(define (start-supervised-worker! events seconds work
                                  #:launch [launch thread]
                                  #:fail fail
                                  #:exited exited
                                  #:timed-out timed-out
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
      (define outcome
        (sync result-ch
              (wrap-evt (thread-dead-evt worker)
                        (lambda (_)
                          ;; Death and a queued result can both be ready. Boxing the poll
                          ;; distinguishes a returned #f from an absent result.
                          (define result (sync/timeout 0 (wrap-evt result-ch box)))
                          (if result (unbox result) (fail exited))))
              (wrap-evt (deadline-evt deadline) (lambda (_) (fail timed-out)))))
      ;; Release resources before notifying the owner, even if done itself raises.
      (stop-supervised-worker! handle)
      (async-channel-put events (done outcome))))
  handle)
