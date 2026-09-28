#lang racket/base

(provide run-resyntax/in-place)

(require racket/place
         racket/match
         "../common/json-util.rkt"
         "../common/interfaces.rkt"
         "../doclib/external/resyntax.rkt")

;; Runs resyntax in one reusable place so callers avoid extra startup cost and
;; degrade to empty results if the worker is unavailable or interrupted.

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
      (with-handlers ([exn:fail? (lambda (_exn) '())])
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
  (with-handlers ([exn:break? (lambda (exn) (kill-worker!) (raise exn))]
                  [exn:fail? (lambda (_exn) (kill-worker-and-return-empty-results!))])
    (define worker (ensure-worker!))
    (place-channel-put worker (list text uri))
    (define result
      (sync worker
            (handle-evt (place-dead-evt worker)
                        (lambda (_event) (kill-worker-and-return-empty-results!)))))
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
