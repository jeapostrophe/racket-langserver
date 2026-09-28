#lang racket

(require "../../common/dynamic-import.rkt"
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         racket/async-channel
         racket/place
         racket/sandbox
         "../../lsp/resyntax-place.rkt"
         "resyntax-place-test-support.rkt")

(define has-resyntax? #t)
(dynamic-imports ('resyntax
                   resyntax-analyze)
                 (λ () (set! has-resyntax? #f)))

(module+ test
  (require rackunit)

  (define (await event)
    (or (sync/timeout 20 event) (fail "timed out waiting for worker lifecycle event")))

  (define (start text [timeout #f])
    (define results (make-async-channel))
    (define caller
      (thread
        (lambda ()
          (async-channel-put results
                             (with-handlers ([exn:break? (lambda (_e) 'cancelled)]
                                             [exn:fail:resource? (lambda (_e) 'timed-out)]
                                             [exn? values])
                               (if timeout
                                   (with-limits timeout #f (run-resyntax/in-place text "file:///controlled.rkt"))
                                   (run-resyntax/in-place text "file:///controlled.rkt")))))))
    (values caller results))

  (define (request-received! worker text)
    (check-equal? (await (Controlled-Worker-notice worker))
                  (list text "file:///controlled.rkt")))

  (define (reply! worker text)
    (define result (Resyntax-Result 0 (string-length text) "file:///controlled.rkt" 'test text))
    (place-channel-put (Controlled-Worker-control worker) (list (->jsexpr result)))
    (list result))

  (define (complete! worker text caller results)
    (request-received! worker text)
    (define expected (reply! worker text))
    (check-equal? (await results) expected)
    (await caller)
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
    "successful requests reuse the same worker and return their own results"
    (call-with-controlled-workers
      (lambda (created)
        (define-values (first results-1) (start "first"))
        (define worker (await created))
        (complete! worker "first" first results-1)
        (define-values (second results-2) (start "second"))
        (complete! worker "second" second results-2)
        (check-eq? (get-resyntax-worker) (Controlled-Worker-place worker))
        (check-true (resyntax-worker-live?))
        (check-false (sync/timeout 0 created)))))

  (for ([mode '(break timeout custodian kill death)])
    (test-case
      (format "~a disposes the worker and permits a distinct replacement result" mode)
      (call-with-controlled-workers
        (lambda (created)
          ;; Warm startup is outside the short computation timeout.
          (define-values (warm warm-results) (start "warm"))
          (define old (await created))
          (complete! old "warm" warm warm-results)
          (define caller-custodian (make-custodian))
          (define-values (caller results)
            (parameterize ([current-custodian caller-custodian])
              (start "abandoned" (and (eq? mode 'timeout) 0.5))))
          (request-received! old "abandoned")
          (case mode
            [(break) (break-thread caller)]
            [(custodian) (custodian-shutdown-all caller-custodian)]
            [(kill) (kill-thread caller)]
            [(death) (place-channel-put (Controlled-Worker-control old) 'die)])
          (await caller)
          (await (place-dead-evt (Controlled-Worker-place old)))
          (await (resyntax-worker-idle-evt))
          ;; Check before fixture cleanup can clear any global state.
          (check-false (get-resyntax-worker))
          (case mode
            [(break) (check-eq? (await results) 'cancelled)]
            [(timeout) (check-eq? (await results) 'timed-out)]
            [(death) (check-equal? (await results) '())])
          (define-values (next next-results) (start "replacement"))
          (define fresh (await created))
          (check-not-eq? (Controlled-Worker-place fresh) (Controlled-Worker-place old))
          (complete! fresh "replacement" next next-results)
          (check-eq? (get-resyntax-worker) (Controlled-Worker-place fresh))
          (check-true (resyntax-worker-live?))
          (custodian-shutdown-all caller-custodian)))))

  (test-case
    "cancelling a caller waiting for ownership leaves the active request intact"
    (call-with-controlled-workers
      (lambda (created)
        (define-values (owner owner-results) (start "owner"))
        (define worker (await created))
        (request-received! worker "owner")
        (define-values (waiting waiting-results) (start "waiting"))
        (await (system-idle-evt))
        (break-thread waiting)
        (check-eq? (await waiting-results) 'cancelled)
        (await waiting)
        (check-eq? (get-resyntax-worker) (Controlled-Worker-place worker))
        (check-true (resyntax-worker-live?))
        (define expected (reply! worker "owner"))
        (check-equal? (await owner-results) expected)
        (await owner)
        (void)))))
