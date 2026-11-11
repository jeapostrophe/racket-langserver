#lang racket/base

;; Cross-file binding lookup for definition and hover. Each request observes
;; the target once and uses analysis for that snapshot. An observed target
;; change ends target lookup; hover can compose a local fallback. An unobserved
;; disk change affects a later query.
(require racket/contract
         racket/match
         "../../common/debug.rkt"
         "../../common/interfaces.rkt"
         "../../common/source.rkt"
         "../../common/time.rkt"
         "../../doclib/internal-types.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt"
         "../responses.rkt"
         "../source-controller.rkt")

(provide target-query)

;; Immutable inputs for one request. The requesting view governs cancellation;
;; the target view is acquired separately and governs obsolescence.
(struct/contract Target-Request
  ([id (or/c number? string?)]
   [controller Source-Controller?]
   [deadline real?]
   [view (or/c #f Source-View?)]
   [target Module-Binding?]
   [feature string?]
   [shutdown evt?])
  #:transparent)

;; Expected stops have explicit provenance; arbitrary raised values never
;; become fallback permissions. The handler rechecks requester cancellation.
(struct/contract Request-Stop
  ([reason (or/c 'requester-changed 'shutdown 'deadline 'target-changed 'timed-out 'stopped)])
  #:transparent)

(struct/contract Target-Failure
  ([stage string?]
   [detail any/c])
  #:transparent)

;; Expected target unavailability mapped to its failure stage description.
(define (stop-stage reason)
  (case reason
    [(deadline) "request deadline expired"]
    [(target-changed) "target changed"]
    [(timed-out) "target timed out"]
    [(stopped) "analysis stopped"]
    [else #f]))

(define (target-fallback-reason? reason)
  (and (stop-stage reason) #t))

(define (request-stop request [check-deadline? #t])
  (define view (Target-Request-view request))
  (cond
    [(or (not view) (sync/timeout 0 (Source-View-changed view))) 'requester-changed]
    [(sync/timeout 0 (Target-Request-shutdown request)) 'shutdown]
    [(and check-deadline?
          (>= (current-deadline-milliseconds) (Target-Request-deadline request))) 'deadline]
    [else #f]))

(define (check-request! request [check-deadline? #t])
  (define reason (request-stop request check-deadline?))
  (when reason
    (raise (Request-Stop reason))))

(define (check-live! request [target-view #f] [check-deadline? #t])
  (check-request! request check-deadline?)
  (when (and target-view (sync/timeout 0 (Source-View-changed target-view)))
    (raise (Request-Stop 'target-changed))))

;; A ready failure must remain an error even if sync selected the deadline.
(define (check-result! result)
  (when (Analysis-Handle? result)
    (define outcome (sync/timeout 0 (Analysis-Handle-completion-evt result)))
    (when outcome
      (check-result! outcome)))
  (define failure (if (Source-Result? result) (Source-Result-error result) result))
  (cond
    [(Source-Failure? failure)
     (raise (Target-Failure (if (Source-Result? result)
                                "source read failed"
                                "source or analysis failed")
                            (Source-Failure-reason failure)))]
    [(Source-Read-Timeout? failure) (raise (Request-Stop 'timed-out))]
    [(and (Analysis-Outcome? result) (eq? 'failed (Analysis-Outcome-state result)))
     (raise (Target-Failure "analysis did not succeed" result))]))

;; Constructing a controller event submits work. Delay it until after the
;; liveness check, including when the deferred query was already expired.
;; Existing events and submitted replies must be repeatable broadcasts: polling
;; after sync is safe and recovers ready failures when sync chose the deadline.
(define (wait-live request event-or-submit [target-view #f])
  (define event
    (if (evt? event-or-submit)
        (begin
          (check-live! request target-view #f)
          event-or-submit)
        (begin
          (check-live! request target-view #t)
          (event-or-submit))))
  (define requester-view (Target-Request-view request))
  (define wake
    (sync event (Target-Request-shutdown request) (deadline-evt (Target-Request-deadline request))
          (if requester-view (Source-View-changed requester-view) never-evt)
          (if target-view (Source-View-changed target-view) never-evt)))
  (check-request! request #f)
  (define ready (sync/timeout 0 (wrap-evt event box)))
  (define result (if ready (unbox ready) wake))
  (check-result! result)
  (check-live! request target-view)
  result)

;; Missing files and syntax errors have no detail. Analysis is always for
;; the acquired snapshot; target changes end the wait instead of retrying.
(define (resolve-detail request acquired lookup)
  (match-define (struct* Target-Request ([controller controller] [target target] [feature feature]))
    request)
  (define view (Source-Result-view acquired))
  (case (Source-View-content-state view)
    [(unknown) (raise (Request-Stop 'target-changed))]
    [(file-not-found) #f]
    [else
     (define handle
       (wait-live request
                  (lambda () (source-controller-analysis-evt controller (Source-Result-snapshot acquired)))
                  view))
     (define available (Analysis-Handle-reusable-summary handle))
     (define outcome
       (or (sync/timeout 0 (Analysis-Handle-completion-evt handle))
           (and available (Analysis-Outcome 'succeeded available))
           (wait-live request (Analysis-Handle-completion-evt handle) view)))
     (match outcome
       [(struct* Analysis-Outcome ([state 'succeeded] [value facts]))
        (define detail (lookup facts target))
        (unless detail (log-racket-langserver-info "~a detail unavailable: ~v" feature target))
        detail]
       [(struct* Analysis-Outcome ([state 'errors]))
        (log-racket-langserver-info "~a analysis reported errors for ~a: ~v"
                                    feature (Module-Binding-filepath target) target)
        #f]
       [(struct* Analysis-Outcome ([state (and reason (or 'timed-out 'stopped))]))
        (raise (Request-Stop reason))]
       [_ (raise (Target-Failure "analysis did not succeed" outcome))])]))

(define (stop-response request reason)
  (define id (Target-Request-id request))
  (define stage (stop-stage reason))
  (cond
    [(eq? reason 'requester-changed)
     (error-response id ErrorCode-ContentModified "Document changed or closed")]
    [(eq? reason 'shutdown)
     (error-response id ErrorCode-RequestFailed "Server shut down")]
    [stage
     (error-response id ErrorCode-RequestFailed
                     (string-append (Target-Request-feature request) " " stage))]
    [else
     (error 'stop-response "unexpected stop reason: ~v" reason)]))

(define (query-error-response request reason)
  (define cancelled (request-stop request #f))
  (cond
    [cancelled (stop-response request cancelled)]
    [(Request-Stop? reason) (stop-response request (Request-Stop-reason reason))]
    [else
     (define-values (stage detail)
       (match reason
         [(Target-Failure stage detail) (values stage detail)]
         [_ (values "request failed" reason)]))
     (define message (string-append (Target-Request-feature request) " " stage))
     (log-racket-langserver-error "~a request failed for ~a: ~a: ~v"
                                  (Target-Request-feature request)
                                  (Module-Binding-filepath (Target-Request-target request)) message detail)
     (error-response (Target-Request-id request) ErrorCode-RequestFailed message)]))

(define (resolve-target-value request lookup compose)
  (define controller (Target-Request-controller request))
  (define target (Target-Request-target request))
  (define acquired
    (wait-live request
               (lambda ()
                 (source-controller-observe-evt controller (path->source (Module-Binding-filepath target))))))
  (define view (Source-Result-view acquired))
  (check-live! request view)
  (define detail (resolve-detail request acquired lookup))
  ;; This final owner check does not read disk a second time.
  (unless (wait-live request (lambda () (source-controller-current-view-evt controller view)) view)
    (raise (Request-Stop 'target-changed)))
  (define value (compose (Source-Result-snapshot acquired) detail))
  (check-live! request view)
  (success/enc (Target-Request-id request) value))

(define (execute-target-query request lookup compose fallback)
  (define (run-fallback)
    (check-request! request #f)
    (define value (fallback))
    (check-request! request #f)
    (success/enc (Target-Request-id request) value))

  ;; The outer boundary also catches failures while composing a fallback.
  (with-handlers ([(lambda (_) #t) (lambda (reason) (query-error-response request reason))])
    (with-handlers ([Request-Stop?
                     (lambda (stop)
                       (if (and fallback (target-fallback-reason? (Request-Stop-reason stop)))
                           (run-fallback)
                           (raise stop)))])
      (resolve-target-value request lookup compose))))

;; Return a deferred response for one observation. `view` belongs to the
;; requester. `deadline` is captured by the caller; waits do not cancel shared
;; analysis. Missing files, syntax errors and absent bindings give no detail.
;;
;; lookup: (summary binding) -> detail or #f.
;; compose: (snapshot detail) -> success payload. Missing files pass #f for
;; both; a present detail always has its snapshot. Unexpected failures skip compose.
;; fallback: optional local-payload thunk for expected target unavailability;
;; it runs with requester/shutdown checks but without the expired target budget.
(define (target-query id controller deadline view target feature lookup compose
                      #:fallback [fallback #f])
  (define request
    (Target-Request id controller deadline view target feature (source-controller-shutdown-evt controller)))
  (lambda () (execute-target-query request lookup compose fallback)))
