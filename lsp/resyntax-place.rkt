#lang racket/base

(provide run-resyntax/in-place)

(require racket/place
         racket/match
         "../common/json-util.rkt"
         "../common/interfaces.rkt"
         "../doclib/external/resyntax.rkt")

;; Runs resyntax in one reusable place so callers avoid extra startup cost and
;; return empty results if Resyntax is unavailable. Executor failures become a
;; failed analysis outcome in the scheduler; acceptance discards them without
;; publishing a candidate.

;; Prefab so the place can send the failure value.
(struct Resyntax-Failure (message) #:prefab)

(define resyntax-worker-lock (make-semaphore 1))
(define resyntax-worker #f)
(define resyntax-worker-custodian (current-custodian))

(define (kill-worker-and-return-empty-results!)
  (kill-worker!)
  '())

(define (worker-live? worker)
  (and worker
       (not (sync/timeout 0 (place-dead-evt worker)))))

(define (kill-worker!)
  (when (worker-live? resyntax-worker)
    (place-kill resyntax-worker))
  (set! resyntax-worker #f))

(define (worker-loop ch)
  (let loop ()
    (match-define (list text uri) (place-channel-get ch))
    (define result
      (with-handlers ([(lambda (_) #t)
                       (lambda (reason)
                         (Resyntax-Failure
                           (if (exn? reason) (exn-message reason) (format "~v" reason))))])
        (map ->jsexpr (run-resyntax text uri))))
    (place-channel-put ch result)
    (loop)))

(define current-resyntax-worker-factory
  (make-parameter (lambda () (place ch (worker-loop ch)))))

(define (spawn-worker!)
  (parameterize-break #f
    (set! resyntax-worker ((current-resyntax-worker-factory))))
  resyntax-worker)

(define (ensure-worker!)
  (if (worker-live? resyntax-worker)
      resyntax-worker
      (spawn-worker!)))

(define (run-resyntax/safely text uri)
  (with-handlers ([(lambda (_) #t) (lambda (reason) (kill-worker!) (raise reason))])
    (define worker (ensure-worker!))
    (place-channel-put worker (list text uri))
    (define result
      (sync worker
            (handle-evt (place-dead-evt worker)
                        (lambda (_event) (error 'run-resyntax/in-place "worker exited")))))
    (when (Resyntax-Failure? result)
      (error 'run-resyntax/in-place "~a" (Resyntax-Failure-message result)))
    (map jsexpr->Resyntax-Result result)))

(define (run-resyntax/in-place text uri)
  ;; A timeout can kill the caller without unwinding its locks. A nested helper
  ;; under the module custodian receives a break and can dispose of the worker
  ;; before releasing ownership. It must remain breakable under with-limits.
  (call-in-nested-thread
    (lambda ()
      (parameterize ([current-custodian resyntax-worker-custodian])
        (parameterize-break #t
          (call-with-semaphore
            resyntax-worker-lock
            (lambda ()
              (if (resyntax-available?)
                  (run-resyntax/safely text uri)
                  (kill-worker-and-return-empty-results!)))))))
    resyntax-worker-custodian))

(module+ test-support
  (provide current-resyntax-worker-factory))
