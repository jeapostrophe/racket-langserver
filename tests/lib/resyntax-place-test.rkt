#lang racket

(require "../../common/dynamic-import.rkt"
         "../../common/interfaces.rkt"
         "../../lsp/resyntax-place.rkt"
         "resyntax-place-test-support.rkt")

(define has-resyntax? #t)
(dynamic-imports ('resyntax
                   resyntax-analyze)
                 (λ () (set! has-resyntax? #f)))

(module+ test
  (require rackunit)

  (define (wait-for-thread th)
    (sync th)
    (void))

  (test-case
    "run-resyntax/in-place: returns Resyntax-Result list"
    (define code
      "#lang racket\n(or 1 (or 2 3))")
    (define results (run-resyntax/in-place code "file:///test.rkt"))
    (check-true (list? results))
    (check-true (andmap Resyntax-Result? results))

    (when has-resyntax?
      (check-equal? (length results) 1)
      (define result (first results))
      (check-equal? (Resyntax-Result-start result) 13)
      (check-equal? (Resyntax-Result-end result) 28)
      (check-equal? (Resyntax-Result-rule-name result) 'nested-or-to-flat-or)
      (check-equal? (Resyntax-Result-new-text result) "(or 1 2 3)")))

  (test-case
    "run-resyntax/in-place: unavailable resyntax does not spawn a worker"
    (reset-resyntax-worker!)
    (dynamic-wind
      void
      (lambda ()
        (call-with-test-resyntax-available?
          #f
          (lambda ()
            (check-equal? (run-resyntax/in-place "#lang racket\n(+ 1 2)" "file:///unavailable.rkt")
                          (list))
            (check-false (get-resyntax-worker))
            (check-false (resyntax-worker-live?)))))
      reset-resyntax-worker!))

  (test-case
    "run-resyntax/in-place: reuses one worker across calls"
    (reset-resyntax-worker!)
    (dynamic-wind
      void
      (lambda ()
        (when has-resyntax?
          (run-resyntax/in-place "#lang racket\n(+ 1 2)" "file:///reuse-1.rkt")
          (define worker-1 (get-resyntax-worker))
          (check-not-false worker-1)
          (check-true (resyntax-worker-live?))
          (run-resyntax/in-place "#lang racket\n(+ 3 4)" "file:///reuse-2.rkt")
          (check-eq? (get-resyntax-worker) worker-1)))
      reset-resyntax-worker!))

  (test-case
    "run-resyntax/in-place: cancellation kills the worker and recreates it later"
    (reset-resyntax-worker!)
    (dynamic-wind
      void
      (lambda ()
        (define delayed-worker #f)
        (call-with-test-resyntax-available?
          #t
          (lambda ()
            (call-with-test-delayed-worker
              250
              (lambda ()
                (set! delayed-worker (get-resyntax-worker))
                (define worker-thread
                  (thread
                    (lambda ()
                      (with-handlers ([exn:break? (lambda (_exn) (void))])
                        (run-resyntax/in-place "#lang racket\n(+ 1 2)" "file:///cancel.rkt")))))
                (sleep 0.05)
                (break-thread worker-thread)
                (wait-for-thread worker-thread)))))
        (check-false (get-resyntax-worker))
        (check-false (resyntax-worker-live?))
        (when has-resyntax?
          (run-resyntax/in-place "#lang racket\n(+ 5 6)" "file:///after-cancel.rkt")
          (define worker-after-cancel (get-resyntax-worker))
          (check-not-false worker-after-cancel)
          (check-true (resyntax-worker-live?))
          (check-false (eq? worker-after-cancel delayed-worker))))
      reset-resyntax-worker!)))
