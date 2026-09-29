#lang racket/base

(require rackunit
         racket/logging
         "../../common/debug.rkt"
         "../../common/dynamic-import.rkt")

(module+ test
  (test-case
    "existing helpers and optional imports share the server logger"
    (define logs '())
    (with-intercepted-logging
      (lambda (event) (set! logs (cons event logs)))
      (lambda ()
        (maybe-debug-log '(incoming message))
        (let ()
          (dynamic-imports ('racket/base missing-binding-for-logging-test) void)
          (check-true (void? missing-binding-for-logging-test)))
        (check-equal? (call-with-values (lambda () (D (values 1 2))) list) '(1 2))
        (check-equal? (call-with-values (lambda () (T (values 3 4))) list) '(3 4)))
      #:logger racket-langserver-logger 'debug)
    (define events (reverse logs))
    (check-equal? (map (lambda (event) (vector-ref event 0)) events)
                  '(debug info debug debug))
    (check-equal? (map (lambda (event) (vector-ref event 3)) events)
                  '(racket-langserver racket-langserver racket-langserver racket-langserver))
    (check-equal? (vector-ref (car events) 1) "racket-langserver: (incoming message)")
    (check-regexp-match #rx"missing-binding-for-logging-test" (vector-ref (cadr events) 1)))

  (test-case
    "server logging coexists with expansion log interception"
    (define server-logs (make-log-receiver racket-langserver-logger 'debug))
    (define expansion-logs '())
    (define payload (list #'value))
    (with-intercepted-logging
      (lambda (event) (set! expansion-logs (cons event expansion-logs)))
      (lambda ()
        (maybe-debug-log 'server-message)
        (log-message (current-logger) 'info 'online-check-syntax
                     "tooltip information" payload))
      'info)
    (define server-event (sync/timeout 5 server-logs))
    (check-not-false server-event)
    (check-equal? (vector-ref server-event 3) 'racket-langserver)
    (check-false (sync/timeout 0 server-logs))
    (check-equal? (length expansion-logs) 1)
    (check-eq? (vector-ref (car expansion-logs) 2) payload)
    (check-equal? (vector-ref (car expansion-logs) 3) 'online-check-syntax)))
