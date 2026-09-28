#lang racket/base

(require racket/file
         racket/runtime-path)

(define-runtime-path driver "../fixtures/disk-provenance-driver.rkt")

;; Requires fresh module instances, as in raco test's normal process mode,
;; so the scheduler inherits the guard installed before loading the driver.
(module+ test
  (define directory (make-temporary-file "disk-provenance~a" 'directory))
  (define path (build-path directory "source.rkt"))
  (define before-read void)
  (define guard
    (make-security-guard
      (current-security-guard)
      (lambda (who file modes)
        (when (and (eq? who 'open-input-file) (equal? file path) (memq 'read modes))
          (before-read)))
      void))
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([current-security-guard guard])
        ((dynamic-require driver 'run-disk-provenance-tests)
         path (lambda (hook) (set! before-read hook)))))
    (lambda ()
      (delete-directory/files directory))))
