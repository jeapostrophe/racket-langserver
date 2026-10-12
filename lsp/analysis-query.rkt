#lang racket/base

;; Revision-bound waits for features that need the installed Check Syntax
;; projection. The deadline is captured before returning the deferred thunk.
(require "../common/interfaces.rkt"
         "../common/json-util.rkt"
         "../doclib/doc.rkt"
         "../workspace/source-store.rkt"
         "analysis.rkt"
         "responses.rkt"
         "safedoc.rkt"
         "source-controller.rkt")

(provide analysis-query current-analysis-request-timeout)

(define current-analysis-request-timeout (make-parameter 90))

(define (analysis-query safe-doc id read-result)
  (define deadline (+ (current-inexact-monotonic-milliseconds)
                      (* 1000 (current-analysis-request-timeout))))
  (define controller (current-source-controller))
  (define shutdown (source-controller-shutdown-evt controller))
  (define-values (view job immediate)
    (with-read-safedoc safe-doc
      (lambda (sd)
        (define outcome (and (SafeDoc-analysis sd) (sync/timeout 0 (SafeDoc-analysis sd))))
        (values (SafeDoc-source-view sd) (SafeDoc-analysis sd)
                ;; Client versions need not change on source invalidation.
                (and (Analysis-Outcome? outcome)
                     (eq? 'succeeded (Analysis-Outcome-state outcome))
                     (success-response id (->jsexpr (read-result (SafeDoc-doc sd)))))))))
  (define (failed reason)
    (error-response id ErrorCode-RequestFailed reason))
  (define (cancelled)
    (error-response id ErrorCode-RequestCancelled "Document changed or closed"))
  (define (expired?)
    (>= (current-inexact-monotonic-milliseconds) deadline))
  (define (changed?)
    (or (not view) (sync/timeout 0 (Source-View-changed view))))
  (cond
    [immediate immediate]
    [else
     (lambda ()
       (with-handlers ([(lambda (_) #t) (lambda (_) (failed "Analysis request failed"))])
         (define outcome
           (and job view
                (sync job (Source-View-changed view) shutdown (alarm-evt deadline #t))))
         (cond
           [(changed?) (cancelled)]
           [(expired?) (failed "Analysis request deadline expired")]
           [(sync/timeout 0 shutdown) (failed "Server shut down")]
           [(and (Analysis-Outcome? outcome)
                 (eq? 'succeeded (Analysis-Outcome-state outcome)))
            (with-read-safedoc safe-doc
              (lambda (sd)
                (cond
                  [(not (eq? view (SafeDoc-source-view sd))) (cancelled)]
                  [(expired?) (failed "Analysis request deadline expired")]
                  [(sync/timeout 0 shutdown) (failed "Server shut down")]
                  [else (success-response id (->jsexpr (read-result (SafeDoc-doc sd))))])))]
           [else (failed "Check Syntax did not succeed")])))]))
