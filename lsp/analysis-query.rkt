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
;;
;; `visual-query` shares that wait loop but reads the current adjusted trace on
;; source errors, supervisor timeouts, query deadlines, or a stopped attempt
;; for a still-current view.
;; Successful visual responses return in a `Visual-Delivery` so the session
;; consumer can recheck freshness before sending. Failures return plain errors.
(require json
         racket/contract
         "../common/debug.rkt"
         "../common/interfaces.rkt"
         "../common/json-util.rkt"
         "../common/time.rkt"
         "../workspace/source-store.rkt"
         "analysis.rkt"
         "responses.rkt"
         "safedoc.rkt"
         "source-controller.rkt")

(provide analysis-query visual-query current-analysis-request-timeout
         (struct-out Visual-Delivery))

;; A session-consumer command: queue the response only after rechecking the
;; requester view. The degraded view is carried for subsequent refresh recovery.
(struct/contract Visual-Delivery
  ([response (and/c hash? jsexpr?)]
   [feature (or/c 'inlay-hints 'semantic-tokens)]
   [request-view Source-View?]
   [degraded-view (or/c #f Source-View?)])
  #:transparent)

;; Seconds allowed for one analysis or visual query, measured from construction.
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


;; Visual queries share the analysis wait loop, but read the current adjusted
;; trace on source errors or their own deadline. Worker failures remain errors.
;; Selection and rendering share one read lock, keeping the exact/degraded
;; classification consistent with the rendered trace during invalidation.
(define (visual-query safe-doc request-id feature read-result)
  (define deadline-ms (+ (current-deadline-milliseconds)
                         (* 1000 (current-analysis-request-timeout))))
  (define shutdown (source-controller-shutdown-evt (current-source-controller)))
  (define view (with-read-safedoc safe-doc SafeDoc-source-view))

  (define (next-response awaited wake)
    (with-read-safedoc safe-doc
      (lambda (sd)
        (define current (SafeDoc-analysis-completion sd))
        ;; Poll the current broadcast outcome even when sync selected the deadline.
        (define outcome
          (or (and current (sync/timeout 0 current))
              (and (eq? current awaited) wake)))
        (define outcome-state
          (and (Analysis-Outcome? outcome) (Analysis-Outcome-state outcome)))
        (define available (SafeDoc-available sd))
        (define exact?
          (and view available
               (eq? (Source-Snapshot-revision (Available-Analysis-snapshot available))
                    (Source-View-revision view))))
        (define (read-visual degraded?)
          (Visual-Delivery (success/enc request-id (read-result (SafeDoc-doc sd)))
                           feature view (and degraded? view)))

        (cond
          [(or (not view) (not (eq? view (SafeDoc-source-view sd))))
           (error-response request-id ErrorCode-ContentModified "Document changed or closed")]
          [(sync/timeout 0 shutdown)
           (error-response request-id ErrorCode-RequestFailed "Server shut down")]
          [exact? (read-visual #f)]
          ;; Trap: worker failure must win over deadline expiry before line 150 degrades.
          [(eq? outcome-state 'failed)
           (analysis-failed-response request-id "Check Syntax did not succeed" outcome)]
          [(>= (current-deadline-milliseconds) deadline-ms) (read-visual #t)]
          [(and current (not (eq? current awaited))) current]
          [(memq outcome-state '(errors timed-out stopped)) (read-visual #t)]
          [else (analysis-failed-response request-id "Check Syntax did not succeed" outcome)]))))

  (define initial (next-response #f #f))
  (if (evt? initial)
      (lambda ()
        (wait-for-analysis initial view shutdown deadline-ms next-response request-id))
      initial))
