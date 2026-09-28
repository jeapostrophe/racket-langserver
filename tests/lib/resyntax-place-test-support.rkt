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

(define resyntax-place-ns
  (begin
    (dynamic-require '(file "../../lsp/resyntax-place.rkt") #f)
    (module->namespace '(file "../../lsp/resyntax-place.rkt"))))

(define external-resyntax-ns
  (begin
    (dynamic-require '(file "../../doclib/external/resyntax.rkt") #f)
    (module->namespace '(file "../../doclib/external/resyntax.rkt"))))

(define (eval-in namespace expr)
  (eval expr namespace))

(define (module-value namespace symbol)
  (eval-in namespace symbol))

(define (set-module-variable! namespace symbol value)
  (define temp-name (gensym 'temp))
  (namespace-set-variable-value! temp-name value #t namespace)
  (eval-in namespace `(set! ,symbol ,temp-name)))

(define (call-with-module-variable namespace symbol value thunk)
  (define previous-value (module-value namespace symbol))
  (dynamic-wind
    (lambda ()
      (set-module-variable! namespace symbol value))
    thunk
    (lambda ()
      (set-module-variable! namespace symbol previous-value))))

(define (worker-live? worker)
  (and worker
       (not (sync/timeout 0 (place-dead-evt worker)))))

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
  (call-with-module-variable external-resyntax-ns 'has-resyntax? available? thunk))

(define (reset-resyntax-worker!)
  ;; A broken implementation can strand this lock. Cleanup must report that
  ;; failure instead of hanging the whole suite while attempting to reset it.
  (define lock (module-value resyntax-place-ns 'resyntax-worker-lock))
  (unless (sync/timeout 5 lock) (error 'reset-resyntax-worker! "worker lock remained held"))
  (dynamic-wind void
                (lambda () (eval-in resyntax-place-ns '(kill-worker!)))
                (lambda () (semaphore-post lock))))

(define (resyntax-worker-idle-evt)
  (semaphore-peek-evt (module-value resyntax-place-ns 'resyntax-worker-lock)))

(define (get-resyntax-worker)
  (module-value resyntax-place-ns 'resyntax-worker))

(define (resyntax-worker-live?)
  (eval-in resyntax-place-ns '(worker-live? resyntax-worker)))
