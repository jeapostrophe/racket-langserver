#lang racket/base

(provide call-with-controlled-workers
         call-with-test-resyntax-available?
         get-resyntax-worker
         reset-resyntax-worker!
         resyntax-worker-live?
         resyntax-worker-idle-evt
         (struct-out Controlled-Worker))

(require racket/async-channel
         racket/match
         racket/place
         (submod "../../lsp/resyntax-place.rkt" test-support))

(struct Controlled-Worker (place notice control) #:transparent)

(define (make-controlled-worker)
  (define-values (notice-parent notice-child) (place-channel))
  (define-values (control-parent control-child) (place-channel))
  (define worker
    (place ch
      (match-define (list notice control) (place-channel-get ch))
      (let loop ()
        (match-define (list (? string? text) (? string? uri)) (place-channel-get ch))
        (place-channel-put notice (list text uri))
        (match (place-channel-get control)
          ['die (void)]
          [reply (place-channel-put ch reply) (loop)]))))
  (place-channel-put worker (list notice-child control-child))
  (Controlled-Worker worker notice-parent control-parent))

(define (call-with-controlled-workers proc)
  (define created (make-async-channel))
  (define workers '())
  (define callers (make-custodian))
  (dynamic-wind
    reset-resyntax-worker!
    (lambda ()
      (parameterize ([current-custodian callers]
                     [current-resyntax-worker-factory
                      (lambda ()
                        (define worker (make-controlled-worker))
                        (set! workers (cons worker workers))
                        (async-channel-put created worker)
                        (Controlled-Worker-place worker))])
        (call-with-test-resyntax-available? #t (lambda () (proc created)))))
    (lambda ()
      (custodian-shutdown-all callers)
      (for ([worker (in-list workers)]) (place-kill (Controlled-Worker-place worker)))
      (reset-resyntax-worker!))))

(define (call-with-test-resyntax-available? available? thunk)
  (parameterize ([current-resyntax-presence-check (lambda () available?)])
    (thunk)))

(define (reset-resyntax-worker!)
  ;; A broken implementation can strand this lock. Cleanup must report that
  ;; failure instead of hanging the whole suite while attempting to reset it.
  (unless (sync/timeout 5 resyntax-worker-lock)
    (error 'reset-resyntax-worker! "worker lock remained held"))
  (dynamic-wind void
                kill-worker!
                (lambda () (semaphore-post resyntax-worker-lock))))

(define (resyntax-worker-idle-evt)
  (semaphore-peek-evt resyntax-worker-lock))
