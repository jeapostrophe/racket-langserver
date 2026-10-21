#lang racket/base

;; `analysis-query` reads the buffer when an installed `Available-Analysis`
;; matches the current source-view revision, and returns that response
;; immediately. Otherwise it returns a thunk and leaves the wait to the
;; caller. The deadline is fixed at this call, so time before the thunk runs
;; counts against `current-analysis-request-timeout`.
;;
;; After the wait, an `Analysis-Outcome` of `errors` becomes a success
;; response carrying `empty-result`. A changed or closed document becomes
;; `ContentModified`. Deadline expiry, shutdown, and any other unsuccessful
;; wake become `RequestFailed`.
(require "../common/debug.rkt"
         "../common/interfaces.rkt"
         "../common/json-util.rkt"
         "../common/time.rkt"
         "../workspace/source-store.rkt"
         "analysis.rkt"
         "responses.rkt"
         "safedoc.rkt"
         "source-controller.rkt")

(provide analysis-query current-analysis-request-timeout)

;; Seconds allowed for one query, measured from the call to `analysis-query`.
(define current-analysis-request-timeout (make-parameter 30))

(define (analysis-failed-response request-id message reason)
  (log-racket-langserver-info "Analysis request ~a failed: ~a: ~v" request-id message reason)
  (error-response request-id ErrorCode-RequestFailed message))

;; Call under the buffer read lock. The lock covers the revision check and
;; `read-result`.
(define (available-analysis-response safe-doc request-id read-result)
  (define installed-analysis (SafeDoc-available safe-doc))
  (define current-view (SafeDoc-source-view safe-doc))
  (and installed-analysis current-view
       (eq? (Source-Snapshot-revision (Available-Analysis-snapshot installed-analysis))
            (Source-View-revision current-view))
       (success-response request-id (->jsexpr (read-result (SafeDoc-doc safe-doc))))))

;; `next-response` receives the awaited completion and the wake value. A
;; response ends the wait. An event is the next completion to wait for, under
;; the deadline given to this function. A raised value becomes a RequestFailed
;; response.
(define (wait-for-analysis completion view shutdown-evt deadline-ms next-response request-id)
  (with-handlers ([(lambda (_) #t)
                   (lambda (reason)
                     (analysis-failed-response request-id "Analysis request failed" reason))])
    (let loop ([awaited-completion completion])
      (define wake-result
        (and awaited-completion view
             (sync awaited-completion (Source-View-changed view)
                   shutdown-evt (deadline-evt deadline-ms))))
      (define response-or-completion (next-response awaited-completion wake-result))
      (if (evt? response-or-completion)
          (loop response-or-completion)
          response-or-completion))))

(define (analysis-query safe-doc request-id read-result empty-result)
  (define deadline-ms (+ (current-deadline-milliseconds)
                         (* 1000 (current-analysis-request-timeout))))
  (define shutdown-evt (source-controller-shutdown-evt (current-source-controller)))
  (define-values (request-view initial-completion immediate-response)
    (with-read-safedoc safe-doc
      (lambda (locked-safe-doc)
        (values (SafeDoc-source-view locked-safe-doc) (SafeDoc-analysis-completion locked-safe-doc)
                (available-analysis-response locked-safe-doc request-id read-result)))))
  (define (response-after-wake awaited-completion wake-result)
    (with-read-safedoc safe-doc
      (lambda (locked-safe-doc)
        (define current-completion (SafeDoc-analysis-completion locked-safe-doc))
        (cond
          [(or (not request-view) (not (eq? request-view (SafeDoc-source-view locked-safe-doc))))
           (error-response request-id ErrorCode-ContentModified "Document changed or closed")]
          [(>= (current-deadline-milliseconds) deadline-ms)
           (error-response request-id ErrorCode-RequestFailed "Analysis request deadline expired")]
          [(sync/timeout 0 shutdown-evt)
           (error-response request-id ErrorCode-RequestFailed "Server shut down")]
          [(available-analysis-response locked-safe-doc request-id read-result) => values]
          ;; The buffer's completion event changed during the wait. Wait on the
          ;; new event. The wake value is from the completion this waiter held.
          [(and current-completion (not (eq? awaited-completion current-completion)))
           current-completion]
          [(and (Analysis-Outcome? wake-result) (eq? 'errors (Analysis-Outcome-state wake-result)))
           (success-response request-id (->jsexpr empty-result))]
          [else (analysis-failed-response request-id "Check Syntax did not succeed" wake-result)]))))
  (or immediate-response
      (lambda ()
        (wait-for-analysis initial-completion request-view shutdown-evt deadline-ms
                           response-after-wake request-id))))
