#lang racket/base

(require rackunit
         racket/async-channel
         "../../lsp/safedoc.rkt"
         (submod "../../lsp/safedoc.rkt" test-support))

(provide analyze! wait-for-disk-verification!)

(define (analyze! sd [state 'succeeded])
  (define analyzed (make-async-channel))
  (safedoc-run-check-syntax!
    (lambda (_method _params)
      (async-channel-put analyzed (current-thread)))
    sd)
  (define worker (sync/timeout 20 analyzed))
  (check-true (thread? worker) "analysis publishes diagnostics")
  (check-eq? (sync/timeout 20 worker) worker "analysis finishes publication")
  (check-eq?
    (Check-Syntax-Status-state
      (with-read-safedoc sd SafeDoc-check-syntax-status))
    state))

(define (wait-for-disk-verification! sd)
  (define deadline (+ (current-inexact-milliseconds) 10000))
  (let loop ()
    (cond
      [(with-read-safedoc sd SafeDoc-contribution-matches-disk?) (void)]
      [(< (current-inexact-milliseconds) deadline)
       (sleep 0.005)
       (loop)]
      [else (fail "disk verification did not finish")])))
