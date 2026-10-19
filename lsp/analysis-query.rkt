#lang racket/base

;; Revision-bound waits for features that need the installed Check Syntax
;; projection. The deadline is captured before returning the deferred thunk.
;; Unsuccessful expansion returns the feature's empty result, never an old trace.
(require "../common/interfaces.rkt"
         "../common/json-util.rkt"
         "../common/time.rkt"
         "../doclib/doc.rkt"
         "../workspace/source-store.rkt"
         "analysis.rkt"
         "responses.rkt"
         "safedoc.rkt"
         "source-controller.rkt")

(provide analysis-query current-analysis-request-timeout)

(define current-analysis-request-timeout (make-parameter 90))

(define (analysis-query safe-doc id read-result empty-result)
  (define deadline (+ (current-deadline-milliseconds)
                      (* 1000 (current-analysis-request-timeout))))
  (define controller (current-source-controller))
  (define shutdown (source-controller-shutdown-evt controller))
  ;; Called only under the buffer lock, including after a completion wakes us.
  (define (available-response sd)
    (define available (SafeDoc-available sd))
    (define current-view (SafeDoc-source-view sd))
    (and available current-view
         (eq? (Source-Snapshot-revision (Available-Analysis-snapshot available))
              (Source-View-revision current-view))
         (success-response id (->jsexpr (read-result (SafeDoc-doc sd))))))
  (define-values (view completion immediate)
    (with-read-safedoc safe-doc
      (lambda (sd)
        (values (SafeDoc-source-view sd) (SafeDoc-analysis-completion sd)
                (available-response sd)))))
  (define (failed reason)
    (error-response id ErrorCode-RequestFailed reason))
  (define (cancelled)
    (error-response id ErrorCode-RequestCancelled "Document changed or closed"))
  (define (expired?)
    (>= (current-deadline-milliseconds) deadline))
  (cond
    [immediate immediate]
    [else
     (lambda ()
       (with-handlers ([(lambda (_) #t) (lambda (_) (failed "Analysis request failed"))])
         (let loop ([completion completion])
           (define outcome
             (and completion view
                  (sync completion (Source-View-changed view) shutdown (deadline-evt deadline))))
           (define result
             (with-read-safedoc safe-doc
               (lambda (sd)
                 (define current-completion (SafeDoc-analysis-completion sd))
                 (cond
                   [(or (not view) (not (eq? view (SafeDoc-source-view sd)))) (cancelled)]
                   [(expired?) (failed "Analysis request deadline expired")]
                   [(sync/timeout 0 shutdown) (failed "Server shut down")]
                   [(available-response sd) => values]
                   ;; A same-revision refresh may finish before this waiter runs.
                   ;; Follow its completion when the old result is no longer available.
                   [(and current-completion (not (eq? completion current-completion)))
                    current-completion]
                   [(and (Analysis-Outcome? outcome)
                         (eq? 'errors (Analysis-Outcome-state outcome)))
                    (success-response id (->jsexpr empty-result))]
                   [else (failed "Check Syntax did not succeed")]))))
           (if (evt? result) (loop result) result))))]))
