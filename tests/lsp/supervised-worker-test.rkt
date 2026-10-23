#lang racket/base

(require rackunit
         racket/async-channel
         racket/port
         "../../lsp/source-controller/completion-event.rkt"
         "../../lsp/source-controller/supervised-worker.rkt")

;; All assertions run on the test thread; background threads only report observations.
(define (await event)
  (define result (sync/timeout 3 (wrap-evt event box)))
  (unless result (error 'await "event did not become ready"))
  (unbox result))

(define (start events work #:seconds [seconds 2] #:launch [launch thread])
  (start-supervised-worker! events seconds work
                            #:launch launch
                            #:fail (lambda (reason) (list 'failed reason))
                            #:exited 'exited
                            #:timed-out 'timeout
                            #:done values))

(module+ test
  (test-case "completion preserves its first false result for multiple and later readers"
    (define completion (make-completion-event))
    (define replies (make-async-channel))
    (check-true (Completion-Event? completion))
    (check-false (evt-ready? completion))
    (for ([_ (in-range 4)])
      (thread (lambda () (async-channel-put replies (sync completion)))))
    (resolve-completion! completion #f)
    (resolve-completion! completion 'replacement)
    (for ([_ (in-range 4)])
      (check-false (await replies)))
    (check-true (evt-ready? completion))
    (check-false (await completion))
    (check-true (evt-ready? (ready-evt #f)))
    (check-false (await (ready-evt #f))))

  (test-case "a dead worker's queued false result wins over premature exit"
    (define events (make-async-channel))
    (define handle
      (start events (lambda () #f)
             #:launch (lambda (thunk)
                        (define worker (thread thunk))
                        (await (thread-dead-evt worker))
                        worker)))
    (check-true (Supervised-Worker? handle))
    (check-false (await events))
    (stop-supervised-worker! handle))

  (test-case "raised values become failures"
    (define events (make-async-channel))
    (start events (lambda () (raise 'raised-value)))
    (check-equal? (await events) '(failed raised-value)))

  (test-case "timeout releases child resources before reporting"
    (define events (make-async-channel))
    (define started (make-async-channel))
    (define handle
      (start events
             (lambda ()
               (define child (thread (lambda () (sync never-evt))))
               (async-channel-put started child)
               (sync never-evt))
             #:seconds 0.1))
    (define child (await started))
    (check-equal? (await events) '(failed timeout))
    (check-true (thread-dead? child))
    (stop-supervised-worker! handle))

  (test-case "cancellation is idempotent and releases child resources"
    (define events (make-async-channel))
    (define started (make-async-channel))
    (define handle
      (start events
             (lambda ()
               (define child (thread (lambda () (sync never-evt))))
               (async-channel-put started child)
               (sync never-evt))))
    (define child (await started))
    (stop-supervised-worker! handle)
    (stop-supervised-worker! handle)
    (check-true (thread-dead? child))
    (check-equal? (await events) '(failed exited)))

  (test-case "the supervisor survives work shutting down its own custodian"
    (define events (make-async-channel))
    (start events (lambda () (custodian-shutdown-all (current-custodian))))
    (check-equal? (await events) '(failed exited)))

  (test-case "worker and launch stdout cannot reach the caller's output"
    (define events (make-async-channel))
    (define output (open-output-string))
    (parameterize ([current-output-port output])
      (start events (lambda () (display "worker output") 'finished)
             #:launch (lambda (thunk) (display "launch output") (thread thunk))))
    (check-equal? (await events) 'finished)
    (check-equal? (get-output-string output) "")))
