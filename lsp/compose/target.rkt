#lang racket/base

;; Cross-file binding lookup for definition and hover. Each request observes
;; the target once and uses analysis for that snapshot. An observed target
;; change ends target lookup without detail; an unobserved disk change affects a later query.
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

;; Both signals end the request; neither starts another attempt. Request-Stop
;; carries no response because the handler recomputes cancellation priority.
(struct/contract Request-Stop () #:transparent)
(struct/contract Target-Failure
  ([stage string?]
   [detail any/c])
  #:transparent)

(define (request-cancelled? request)
  (define view (Target-Request-view request))
  (or (not view) (sync/timeout 0 (Source-View-changed view))))

(define (request-cancelled-response request)
  (and (request-cancelled? request)
       (error-response (Target-Request-id request) ErrorCode-ContentModified
                       "Document changed or closed")))

(define (check-live! request [target-view #f])
  (when (or (request-cancelled? request)
            (>= (current-deadline-milliseconds) (Target-Request-deadline request))
            (sync/timeout 0 (Target-Request-shutdown request)))
    (raise (Request-Stop)))
  (when (and target-view (sync/timeout 0 (Source-View-changed target-view)))
    (raise (Target-Failure "target changed" (Target-Request-target request)))))

;; Constructing a controller event submits work. Delay it until after the
;; liveness check, including when the deferred query was already expired.
(define (wait-live request make-event [target-view #f])
  (check-live! request target-view)
  (define event (make-event))
  (define result
    (sync event (Target-Request-shutdown request) (deadline-evt (Target-Request-deadline request))
          (Source-View-changed (Target-Request-view request))
          (if target-view (Source-View-changed target-view) never-evt)))
  (check-live! request target-view)
  (when (Source-Failure? result)
    ;; Disk/worker outcomes have their own typed paths below. A bare command
    ;; failure carries an unexpected owner exception; keep it an LSP error.
    (raise (Source-Failure-reason result)))
  result)

;; Missing files and syntax errors have no detail. Analysis is always for
;; the acquired snapshot; target changes end the wait instead of retrying.
(define (resolve-detail request acquired lookup)
  (match-define (struct* Target-Request ([controller controller] [target target] [feature feature]))
    request)
  (define view (Source-Result-view acquired))
  (match (Source-Result-error acquired)
    [(Source-Read-Timeout reason)
     (raise (Target-Failure "source read timed out" reason))]
    [(Source-Failure reason)
     (raise reason)]
    [#f (void)])
  (case (Source-View-content-state view)
    [(unknown) (raise (Target-Failure "target changed" 'invalidated-observation))]
    [(file-not-found) #f]
    [else
     (define handle
       (wait-live request
                  (lambda () (source-controller-analysis-evt controller (Source-Result-snapshot acquired)))
                  view))
     (define available (Analysis-Handle-reusable-summary handle))
     (define outcome
       (if available
           (Analysis-Outcome 'succeeded available)
           (wait-live request (lambda () (Analysis-Handle-completion-evt handle)) view)))
     (match outcome
       [(struct* Analysis-Outcome ([state 'succeeded] [value facts]))
        (define detail (lookup facts target))
        (unless detail (log-racket-langserver-info "~a detail unavailable: ~v" feature target))
        detail]
       [(struct* Analysis-Outcome ([state 'errors]))
        (log-racket-langserver-info "~a analysis reported errors for ~a: ~v"
                                    feature (Module-Binding-filepath target) target)
        #f]
       [(struct* Analysis-Outcome ([state (or 'timed-out 'stopped)]))
        (raise (Target-Failure "analysis did not succeed" outcome))]
       [(struct* Analysis-Outcome ([state 'failed] [value reason])) (raise reason)]
       [_ (error 'target-query "Unexpected analysis outcome: ~v" outcome)])]))

(define (query-error-response request reason compose)
  (define id (Target-Request-id request))
  (define feature (Target-Request-feature request))
  (define target (Target-Request-target request))
  (or (request-cancelled-response request)
      (match reason
        [(or (? Request-Stop?) (? Target-Failure?))
         (when (Target-Failure? reason)
           (log-racket-langserver-info "~a unavailable for ~a: ~a: ~v"
                                       feature (Module-Binding-filepath target)
                                       (Target-Failure-stage reason)
                                       (Target-Failure-detail reason)))
         ;; Target enrichment is optional. Never reuse its snapshot or detail
         ;; on failure; hover may still have a card from the requesting view.
         (success/enc id (compose #f #f))]
        [_
         (define message (string-append feature " request failed"))
         (log-racket-langserver-error "~a for ~a: ~v"
                                      message (Module-Binding-filepath target) reason)
         (error-response id ErrorCode-RequestFailed message)])))

(define (execute-target-query request lookup compose)
  (match-define (struct* Target-Request ([id id] [controller controller] [target target])) request)
  (with-handlers ([(lambda (_) #t) (lambda (reason) (query-error-response request reason compose))])
    (define acquired
      (wait-live request
                 (lambda ()
                   (source-controller-observe-evt controller (path->source (Module-Binding-filepath target))))))
    (define view (Source-Result-view acquired))
    (define detail (resolve-detail request acquired lookup))
    ;; Validate the accepted observation, including missing files and reusable
    ;; facts. This owner check does not read disk a second time.
    (unless (wait-live request (lambda () (source-controller-current-view-evt controller view)) view)
      (raise (Target-Failure "target changed" target)))
    (success/enc id (compose (Source-Result-snapshot acquired) detail))))

;; Return a deferred response for one observation. `view` belongs to the
;; requester. `deadline` is captured by the caller; waits do not cancel shared
;; analysis. Missing files, syntax errors and absent bindings give no detail.
;;
;; lookup: (summary binding) -> detail or #f.
;; compose: (snapshot detail) -> success payload. Missing files pass #f for
;; both; a present detail always has its snapshot. Target failure, deadline and
;; shutdown also compose with #f for both, while requester changes cancel.
(define (target-query id controller deadline view target feature lookup compose)
  (define request
    (Target-Request id controller deadline view target feature (source-controller-shutdown-evt controller)))
  (lambda () (execute-target-query request lookup compose)))
