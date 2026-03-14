#lang racket/base

(provide run-resyntax/in-place)

(require racket/place
         racket/match
         racket/sandbox
         "../common/json-util.rkt"
         "../common/interfaces.rkt"
         "../doclib/external/resyntax.rkt")

;; Runs resyntax in one reusable place so callers avoid extra startup cost and
;; degrade to empty results if the worker is unavailable or interrupted.

(define resyntax-worker-lock (make-semaphore 1))
(define resyntax-worker #f)

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

(define (run-resyntax-task text uri ch)
  (lambda ()
    (define result
      (with-handlers ([exn:fail? (lambda (_exn) '())])
        (map ->jsexpr (run-resyntax text uri))))
    (place-channel-put ch result)))

(define (worker-loop ch)
  (define th #f)

  (define (stop-task!)
    (when th
      (break-thread th)
      (set! th #f)))

  (let loop ()
    (match (place-channel-get ch)
      [(list text uri)
       (stop-task!)
       (set! th (thread (run-resyntax-task text uri ch)))
       (loop)]
      ['stop
       (stop-task!)])))

(define (spawn-worker!)
  (set! resyntax-worker (place ch (worker-loop ch)))
  resyntax-worker)

(define (ensure-worker!)
  (if (worker-live? resyntax-worker)
      resyntax-worker
      (spawn-worker!)))

(define (cancel-task! worker)
  (place-channel-put worker 'stop)
  '())

(define (run-resyntax/safely text uri)
  (define worker (ensure-worker!))
  (with-handlers ([exn:break? (lambda (_exn) (cancel-task! worker))]
                  [exn:fail:resource? (lambda (_exn) (cancel-task! worker))]
                  [exn:fail? (lambda (_exn) (cancel-task! worker))])
    (place-channel-put worker (list text uri))
    (define result (place-channel-get worker))
    (map jsexpr->Resyntax-Result result)))

(define (run-resyntax/in-place text uri)
  (call-with-semaphore
    resyntax-worker-lock
    (lambda ()
      (if (resyntax-available?)
          (run-resyntax/safely text uri)
          (kill-worker-and-return-empty-results!)))))
