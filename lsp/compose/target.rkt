#lang racket/base

;; Cross-file binding lookup for definition and hover. Each request observes
;; the target once and uses analysis for that snapshot. An observed target
;; change ends the request; an unobserved disk change affects a later query.
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

(define (request-error request)
  (match-define (struct* Target-Request ([id id] [view view] [deadline deadline]
                                         [feature feature] [shutdown shutdown])) request)
  (cond
    [(or (not view) (sync/timeout 0 (Source-View-changed view)))
     (error-response id ErrorCode-ContentModified "Document changed or closed")]
    [(>= (current-deadline-milliseconds) deadline)
     (error-response id ErrorCode-RequestFailed (string-append feature " request deadline expired"))]
    [(sync/timeout 0 shutdown)
     (error-response id ErrorCode-RequestFailed "Server shut down")]
    [else #f]))

(define (check-live! request [target-view #f])
  (when (request-error request) (raise (Request-Stop)))
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
    (raise (Target-Failure "source or analysis failed" (Source-Failure-reason result))))
  result)

;; Missing files and syntax errors have no detail. Analysis is always for
;; the acquired snapshot; target changes end the wait instead of retrying.
(define (resolve-detail request acquired lookup)
  (match-define (struct* Target-Request ([controller controller] [target target] [feature feature]))
    request)
  (define view (Source-Result-view acquired))
  (when (Source-Result-error acquired)
    (raise (Target-Failure "source read failed" (Source-Failure-reason (Source-Result-error acquired)))))
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
       [_ (raise (Target-Failure "analysis did not succeed" outcome))])]))

(define (query-error-response request reason)
  (or (request-error request)
      (let ()
        (define-values (stage detail)
          (match reason
            [(Target-Failure stage detail) (values stage detail)]
            [_ (values "request failed" reason)]))
        (define message (string-append (Target-Request-feature request) " " stage))
        (log-racket-langserver-error "~a request failed for ~a: ~a: ~v"
                                     (Target-Request-feature request)
                                     (Module-Binding-filepath (Target-Request-target request)) message detail)
        (error-response (Target-Request-id request) ErrorCode-RequestFailed message))))

(define (execute-target-query request lookup compose)
  (match-define (struct* Target-Request ([id id] [controller controller] [target target])) request)
  (with-handlers ([(lambda (_) #t) (lambda (reason) (query-error-response request reason))])
    (define acquired
      (wait-live request
                 (lambda ()
                   (source-controller-observe-evt controller (path->source (Module-Binding-filepath target))))))
    (define view (Source-Result-view acquired))
    (check-live! request view)
    (define detail (resolve-detail request acquired lookup))
    ;; Validate the accepted observation, including missing files and reusable
    ;; facts. This owner check does not read disk a second time.
    (unless (wait-live request (lambda () (source-controller-current-view-evt controller view)) view)
      (raise (Target-Failure "target changed" target)))
    (define value (compose (Source-Result-snapshot acquired) detail))
    (check-live! request view)
    (success/enc id value)))

;; Return a deferred response for one observation. `view` belongs to the
;; requester. `deadline` is captured by the caller; waits do not cancel shared
;; analysis. Missing files, syntax errors and absent bindings give no detail.
;;
;; lookup: (summary binding) -> detail or #f.
;; compose: (snapshot detail) -> success payload. Missing files pass #f for
;; both; a present detail always has its snapshot. Failures skip compose.
(define (target-query id controller deadline view target feature lookup compose)
  (define request
    (Target-Request id controller deadline view target feature (source-controller-shutdown-evt controller)))
  (lambda () (execute-target-query request lookup compose)))
