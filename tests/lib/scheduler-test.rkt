#lang racket

(module+ test
  (require rackunit
           "../../lsp/scheduler.rkt")

  (test-case
    "clear-old-queries/doc-close releases waiting queries"
    (define token (gensym 'doc-token))
    (define waiter
      (async-query-wait token (lambda (signal) signal)))
    (define result-box (box #f))
    (define waiter-thread
      (thread
        (lambda ()
          (set-box! result-box (waiter)))))

    (clear-old-queries/doc-close token)

    (check-not-false (sync/timeout 1.0 waiter-thread))
    (check-true (signal-doc-close? (unbox result-box)))))