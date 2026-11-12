#lang racket/base

;; Cross-file binding lookup for definition and hover. Each request observes
;; the target once and uses analysis for that snapshot. An observed target
;; change ends target lookup; hover can compose a local fallback. An unobserved
;; disk change affects a later query.
(require racket/contract
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

;; Requester edits/closes and shutdown never permit fallback. Expected target
;; unavailability permits fallback when the caller supplies one.
(struct Request-Cancelled () #:transparent)

(struct/contract Target-Unavailable
  ([reason (or/c 'deadline 'target-changed 'timed-out 'stopped)])
  #:transparent)

(struct/contract Target-Failure
  ([stage string?]
   [detail any/c])
  #:transparent)

(define (unavailable-stage reason)
  (case reason
    [(deadline) "request deadline expired"]
    [(target-changed) "target changed"]
    [(timed-out) "target timed out"]
    [(stopped) "analysis stopped"]))

(define (cancellation-reason request)
  (define view (Target-Request-view request))
  (cond
    [(or (not view) (sync/timeout 0 (Source-View-changed view))) 'requester-changed]
    [(sync/timeout 0 (Target-Request-shutdown request)) 'shutdown]
    [else #f]))

(define (check-request! request [check-deadline? #t])
  (when (cancellation-reason request)
    (raise (Request-Cancelled)))
  (when (and check-deadline?
             (>= (current-deadline-milliseconds) (Target-Request-deadline request)))
    (raise (Target-Unavailable 'deadline))))

(define (check-live! request [target-view #f] [check-deadline? #t])
  (check-request! request check-deadline?)
  (when (and target-view (sync/timeout 0 (Source-View-changed target-view)))
    (raise (Target-Unavailable 'target-changed))))

;; Unexpected outcome failures take priority over liveness checks; expected
;; unavailability waits until `check-live!` establishes whether target changed.
(define (check-analysis-outcome! outcome [raise-unavailable? #t])
  (case (and (Analysis-Outcome? outcome) (Analysis-Outcome-state outcome))
    [(succeeded errors) outcome]
    [(timed-out stopped)
     (when raise-unavailable?
       (raise (Target-Unavailable (Analysis-Outcome-state outcome))))]
    [else (raise (Target-Failure "analysis did not succeed" outcome))]))

;; A ready failure must remain an error even if sync selected the deadline.
(define (check-result! result)
  (cond
    [(Analysis-Handle? result)
     (define ready (sync/timeout 0 (wrap-evt (Analysis-Handle-completion-evt result) box)))
     (when ready
       (check-analysis-outcome! (unbox ready) #f))]
    [(Source-Result? result)
     (define err (Source-Result-error result))
     (cond
       [(Source-Failure? err)
        (raise (Target-Failure "source read failed" (Source-Failure-reason err)))]
       [(Source-Read-Timeout? err)
        (raise (Target-Unavailable 'timed-out))])]
    [(Source-Failure? result)
     (raise (Target-Failure "source or analysis failed" (Source-Failure-reason result)))]
    [(Analysis-Outcome? result)
     (check-analysis-outcome! result #f)]))

;; Constructing a controller event submits work. Delay it until after the
;; liveness check, including when the deferred query was already expired.
;; Existing events and submitted replies must be repeatable broadcasts: polling
;; after sync is safe and recovers ready failures when sync chose the deadline.
(define (wait-live request event-or-submit [target-view #f])
  (define submit? (not (evt? event-or-submit)))
  (check-live! request target-view submit?)
  (define event (if submit? (event-or-submit) event-or-submit))
  (define requester-view (Target-Request-view request))
  (sync event (Target-Request-shutdown request) (deadline-evt (Target-Request-deadline request))
        (Source-View-changed requester-view)
        (if target-view (Source-View-changed target-view) never-evt))
  (check-request! request #f)
  (define ready (sync/timeout 0 (wrap-evt event box)))
  (unless ready
    (check-live! request target-view)
    ;; A non-monotonic deadline-evt may fire slightly before the clock check catches up.
    (raise (Target-Unavailable 'deadline)))
  (define result (unbox ready))
  (check-result! result)
  (check-live! request target-view)
  result)

;; Missing files and syntax errors have no detail. Analysis is always for
;; the acquired snapshot; target changes end the wait instead of retrying.
(define (resolve-detail request acquired lookup)
  (define controller (Target-Request-controller request))
  (define target (Target-Request-target request))
  (define feature (Target-Request-feature request))
  (define view (Source-Result-view acquired))
  (case (Source-View-content-state view)
    [(unknown) (raise (Target-Unavailable 'target-changed))]
    [(file-not-found) #f]
    [else
     (define handle
       (wait-live request
                  (lambda ()
                    (source-controller-analysis-evt controller
                                                    (Source-Result-snapshot acquired)))
                  view))
     (define available (Analysis-Handle-reusable-summary handle))
     (define outcome
       (or (sync/timeout 0 (Analysis-Handle-completion-evt handle))
           (and available (Analysis-Outcome 'succeeded available))
           (wait-live request (Analysis-Handle-completion-evt handle) view)))
     (check-analysis-outcome! outcome)
     (case (Analysis-Outcome-state outcome)
       [(succeeded)
        (define detail (lookup (Analysis-Outcome-value outcome) target))
        (unless detail (log-racket-langserver-info "~a detail unavailable: ~v" feature target))
        detail]
       [(errors)
        (log-racket-langserver-info "~a analysis reported errors for ~a: ~v"
                                    feature (Module-Binding-filepath target) target)
        #f])]))

(define (query-error-response request reason)
  (define id (Target-Request-id request))
  (define feature (Target-Request-feature request))
  (define cancelled (cancellation-reason request))
  (cond
    [(eq? cancelled 'requester-changed) (content-modified-response id)]
    [(eq? cancelled 'shutdown) (shutdown-response id)]
    [(Target-Unavailable? reason)
     (error-response id ErrorCode-RequestFailed
                     (string-append feature " "
                                    (unavailable-stage (Target-Unavailable-reason reason))))]
    [else
     (define stage
       (if (Target-Failure? reason) (Target-Failure-stage reason) "request failed"))
     (define detail
       (if (Target-Failure? reason) (Target-Failure-detail reason) reason))
     (define message (string-append feature " " stage))
     (log-racket-langserver-error "~a request failed for ~a: ~a: ~v"
                                  feature
                                  (Module-Binding-filepath (Target-Request-target request))
                                  message detail)
     (error-response id ErrorCode-RequestFailed message)]))

(define (resolve-target-value request lookup compose)
  (define controller (Target-Request-controller request))
  (define target (Target-Request-target request))
  (define acquired
    (wait-live request
               (lambda ()
                 (source-controller-observe-evt
                   controller (path->source (Module-Binding-filepath target))))))
  (define view (Source-Result-view acquired))
  (check-live! request view)
  (define detail (resolve-detail request acquired lookup))
  ;; This final owner check does not read disk a second time.
  (unless (wait-live request (lambda () (source-controller-current-view-evt controller view)) view)
    (raise (Target-Unavailable 'target-changed)))
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
    (if fallback
        (with-handlers ([Target-Unavailable? (lambda (_) (run-fallback))])
          (resolve-target-value request lookup compose))
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
    (Target-Request id controller deadline view target feature
                    (source-controller-shutdown-evt controller)))
  (lambda () (execute-target-query request lookup compose fallback)))
