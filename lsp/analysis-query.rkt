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
         "../doclib/doc.rkt"
         "../workspace/source-store.rkt"
         "analysis.rkt"
         "responses.rkt"
         "safedoc.rkt"
         "source-controller.rkt")

(define visual-feature/c (or/c 'inlay-hints 'semantic-tokens))

;; A session-consumer command: queue the response only after rechecking the
;; requester view. A degraded response permits refresh after a delayed fallback.
(struct/contract Visual-Delivery
  ([response (and/c hash? jsexpr?)]
   [feature visual-feature/c]
   [request-view Source-View?]
   [degraded? boolean?])
  #:transparent)

;; Visual query results may be immediate protocol JSON, an immediate delivery command,
;; or a deferred thunk computing either variant.
(define immediate-visual-result/c (or/c Visual-Delivery? (and/c hash? jsexpr?)))
(define visual-result/c (or/c immediate-visual-result/c (-> immediate-visual-result/c)))

;; Seconds allowed for one visual query, measured from construction.
(define current-analysis-request-timeout (make-parameter 30))

(provide visual-feature/c
         visual-result/c
         (struct-out Visual-Delivery)
         (contract-out
           [current-analysis-request-timeout (parameter/c (and/c real? (>=/c 0)))]
           [visual-query
            (-> SafeDoc? (or/c number? string?) visual-feature/c (-> Doc? any/c)
                visual-result/c)]))

(define (analysis-failed-response request-id message reason)
  (log-racket-langserver-info "Analysis request ~a failed: ~a: ~v" request-id message reason)
  (error-response request-id ErrorCode-RequestFailed message))

;; Selection and rendering share one read lock, keeping the exact/degraded
;; classification consistent with the rendered trace during invalidation.
(define (visual-query safe-doc request-id feature read-result)
  (define deadline-ms (deadline-after (current-analysis-request-timeout)))
  (define shutdown (source-controller-shutdown-evt (current-source-controller)))

  (define (with-failure-handler thunk)
    (with-handlers ([(lambda (_) #t)
                     (lambda (reason)
                       (analysis-failed-response request-id "Analysis request failed" reason))])
      (thunk)))

  (define (evaluate sd view [expired? #f])
    (define (read-visual degraded?)
      (Visual-Delivery (success/enc request-id (read-result (SafeDoc-doc sd)))
                       feature view degraded?))

    (cond
      [(not (eq? view (SafeDoc-source-view sd)))
       (content-modified-response request-id)]
      [(sync/timeout 0 shutdown)
       (shutdown-response request-id)]
      [(available-analysis-exact-for-view? (SafeDoc-available sd) view)
       (read-visual #f)]
      [else
       (define current (SafeDoc-analysis-completion sd))
       ;; Poll the current broadcast outcome even when sync selected the deadline.
       (define ready (and current (sync/timeout 0 (wrap-evt current box))))
       (define outcome (and ready (unbox ready)))
       (define outcome-state
         (and (Analysis-Outcome? outcome) (Analysis-Outcome-state outcome)))
       (cond
         ;; A ready worker failure takes priority over deadline fallback.
         [(eq? outcome-state 'failed)
          (analysis-failed-response request-id "Check Syntax did not succeed" outcome)]
         [(or (memq outcome-state '(errors timed-out stopped))
              expired?
              (>= (current-deadline-milliseconds) deadline-ms))
          (read-visual #t)]
         [(and current (not ready)) current]
         [else
          (analysis-failed-response request-id "Check Syntax did not succeed" outcome)])]))

  (define-values (view initial)
    (with-read-safedoc safe-doc
      (lambda (sd)
        (define view (SafeDoc-source-view sd))
        (values view
                (if (not view)
                    (content-modified-response request-id)
                    (with-failure-handler (lambda () (evaluate sd view))))))))

  (if (evt? initial)
      (lambda ()
        (with-failure-handler
          (lambda ()
            (define deadline-wake
              (wrap-evt (deadline-evt deadline-ms) (lambda (_) 'deadline)))
            (let loop ([completion initial])
              (define wake (sync completion (Source-View-changed view) shutdown deadline-wake))
              (define response
                (with-read-safedoc safe-doc
                  (lambda (sd) (evaluate sd view (eq? wake 'deadline)))))
              (if (evt? response) (loop response) response)))))
      initial))
