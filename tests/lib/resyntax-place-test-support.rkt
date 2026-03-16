#lang racket/base

(provide call-with-test-delayed-worker
         call-with-test-resyntax-available?
         get-resyntax-worker
         reset-resyntax-worker!
         resyntax-worker-live?)

(require racket/match
         racket/place)

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

(define (make-delayed-worker delay-ms)
  (define worker
    (place ch
      (define delay-ms (place-channel-get ch))
      (let loop ()
        (match (place-channel-get ch)
          [(list 'run (? string?) (? string?))
           (sleep (/ delay-ms 1000.0))
           (place-channel-put ch (list))
           (loop)]
          ['stop
           (void)]
          [_
           (place-channel-put ch (list))
           (loop)]))))
  (place-channel-put worker delay-ms)
  worker)

(define (call-with-test-delayed-worker delay-ms thunk)
  (define previous-worker (module-value resyntax-place-ns 'resyntax-worker))
  (define delayed-worker (make-delayed-worker delay-ms))
  (dynamic-wind
    (lambda ()
      (set-module-variable! resyntax-place-ns 'resyntax-worker delayed-worker))
    thunk
    (lambda ()
      (when (worker-live? delayed-worker)
        (place-kill delayed-worker))
      (set-module-variable! resyntax-place-ns 'resyntax-worker previous-worker))))

(define (call-with-test-resyntax-available? available? thunk)
  (call-with-module-variable external-resyntax-ns 'has-resyntax? available? thunk))

(define (reset-resyntax-worker!)
  (eval-in resyntax-place-ns '(call-with-semaphore resyntax-worker-lock kill-worker!)))

(define (get-resyntax-worker)
  (module-value resyntax-place-ns 'resyntax-worker))

(define (resyntax-worker-live?)
  (eval-in resyntax-place-ns '(worker-live? resyntax-worker)))
