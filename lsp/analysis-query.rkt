#lang racket/base

;; Visual queries read exact analysis when available, or the adjusted trace on
;; source errors, supervisor timeouts, query deadlines, or a stopped attempt.
;; The deadline starts at construction, including time before deferred dispatch.
;; Successful responses return in a Visual-Delivery so the session consumer can
;; recheck freshness before sending. Failures return plain errors.
(require json
         racket/contract
         "../common/debug.rkt"
         "../common/interfaces.rkt"
         "../common/time.rkt"
         "../workspace/source-store.rkt"
         "analysis.rkt"
         "responses.rkt"
         "safedoc.rkt"
         "source-controller.rkt")

(provide visual-query current-analysis-request-timeout
         (struct-out Visual-Delivery))

;; A session-consumer command: queue the response only after rechecking the
;; requester view. A degraded response permits refresh after a delayed fallback.
(struct/contract Visual-Delivery
  ([response (and/c hash? jsexpr?)]
   [feature (or/c 'inlay-hints 'semantic-tokens)]
   [request-view Source-View?]
   [degraded? boolean?])
  #:transparent)

;; Seconds allowed for one visual query, measured from construction.
(define current-analysis-request-timeout (make-parameter 30))

(define (analysis-failed-response request-id message reason)
  (log-racket-langserver-info "Analysis request ~a failed: ~a: ~v" request-id message reason)
  (error-response request-id ErrorCode-RequestFailed message))

;; Selection and rendering share one read lock, keeping the exact/degraded
;; classification consistent with the rendered trace during invalidation.
(define (visual-query safe-doc request-id feature read-result)
  (define deadline-ms (+ (current-deadline-milliseconds)
                         (* 1000 (current-analysis-request-timeout))))
  (define shutdown (source-controller-shutdown-evt (current-source-controller)))
  (define view (with-read-safedoc safe-doc SafeDoc-source-view))

  (define (next-response awaited)
    (with-read-safedoc safe-doc
      (lambda (sd)
        (define current (SafeDoc-analysis-completion sd))
        ;; Poll the current broadcast outcome even when sync selected the deadline.
        (define outcome (and current (sync/timeout 0 current)))
        (define outcome-state
          (and (Analysis-Outcome? outcome) (Analysis-Outcome-state outcome)))
        (define exact?
          (available-analysis-exact-for-view? (SafeDoc-available sd) view))
        (define (read-visual degraded?)
          (Visual-Delivery (success/enc request-id (read-result (SafeDoc-doc sd)))
                           feature view degraded?))

        (cond
          [(or (not view) (not (eq? view (SafeDoc-source-view sd))))
           (error-response request-id ErrorCode-ContentModified "Document changed or closed")]
          [(sync/timeout 0 shutdown)
           (error-response request-id ErrorCode-RequestFailed "Server shut down")]
          [exact? (read-visual #f)]
          ;; A ready worker failure takes priority over deadline fallback.
          [(eq? outcome-state 'failed)
           (analysis-failed-response request-id "Check Syntax did not succeed" outcome)]
          [(>= (current-deadline-milliseconds) deadline-ms) (read-visual #t)]
          [(and current (not (eq? current awaited))) current]
          [(memq outcome-state '(errors timed-out stopped)) (read-visual #t)]
          [else (analysis-failed-response request-id "Check Syntax did not succeed" outcome)]))))

  (define initial (next-response #f))
  (if (evt? initial)
      (lambda ()
        (with-handlers ([(lambda (_) #t)
                         (lambda (reason)
                           (analysis-failed-response request-id "Analysis request failed" reason))])
          (let loop ([completion initial])
            (sync completion (Source-View-changed view) shutdown (deadline-evt deadline-ms))
            (define response (next-response completion))
            (if (evt? response) (loop response) response))))
      initial))
