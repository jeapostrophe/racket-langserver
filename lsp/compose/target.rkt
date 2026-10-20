#lang racket/base

;; Cross-file binding lookup for definition and hover. A `shared-binding?`
;; target is answered from that file's analysis summary. Any other binding
;; uses the caller's legacy lookup. A changed target is acquired again until
;; the request stops. `target-query` states the caller contract.
(require json
         racket/contract
         racket/match
         "../../common/debug.rkt"
         "../../common/interfaces.rkt"
         "../../common/path-util.rkt"
         "../../common/source.rkt"
         "../../common/time.rkt"
         "../../doclib/internal-types.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt"
         "../analysis-query.rkt"
         "../responses.rkt"
         "../source-controller.rkt")

(provide shared-binding? target-query)

;; Whether shared analysis answers this binding. The route accepts no
;; submodules and phase+space 0. The filepath must also be unchanged by
;; `path->uri` then `uri->path`: the source controller identifies the target
;; by that exact path, so a different path stays on the legacy lookup.
(define (shared-binding? binding)
  (match-define (struct* Module-Binding ([filepath path] [submods submods]
                                         [phase+space phase]))
    binding)
  (and (null? submods) (equal? phase 0) (equal? path (uri->path (path->uri path)))))

;; Fixed fields for one lookup. Retries keep this record and its deadline.
;;
;; controller: Source controller used for observe, analysis, and view checks.
;; deadline: Absolute milliseconds from `current-deadline-milliseconds`.
;;   Queueing, waiting, and retries all consume it.
;; view: Source view of the requesting document, not of the target. `#f`, or
;;   a change to this view, cancels the request.
;; target: Module binding to resolve. Its file is the one this lookup observes.
;; feature: Word used in logs and error text, such as "Definition" or "Hover".
;; shutdown: Becomes ready when the source controller stops.
(struct/contract Target-Request
  ([id (or/c number? string?)]
   [controller Source-Controller?]
   [deadline real?]
   [view (or/c #f Source-View?)]
   [target Module-Binding?]
   [feature string?]
   [shutdown evt?])
  #:transparent)

;; Raised with the protocol response that ends the request.
(struct/contract Request-Stop ([response jsexpr?]) #:transparent)

;; Raised when the acquired target view is no longer current. Resolution
;; starts again with the same `Target-Request`.
(struct/contract Target-Retry () #:transparent)

;; Raised when the target cannot be resolved and the request is still live.
;;
;; stage: Text appended after `feature` in the client error message.
;; detail: Logged with the failure. The client does not receive it.
(struct/contract Target-Failure
  ([stage string?]
   [detail any/c])
  #:transparent)

;; The response that ends this request, or #f while it may continue.
;; A changed or closed requesting document takes priority over an expired
;; deadline, which takes priority over controller shutdown.
(define (request-error request)
  (match-define (struct* Target-Request ([id id] [view view] [deadline deadline]
                                         [feature feature] [shutdown shutdown])) request)
  (cond
    [(or (not view) (sync/timeout 0 (Source-View-changed view)))
     (error-response id ErrorCode-RequestCancelled "Document changed or closed")]
    [(>= (current-deadline-milliseconds) deadline)
     (error-response id ErrorCode-RequestFailed (string-append feature " request deadline expired"))]
    [(sync/timeout 0 shutdown)
     (error-response id ErrorCode-RequestFailed "Server shut down")]
    [else #f]))

;; Raise `Request-Stop` when the request must end. Raise `Target-Retry` when
;; `target-view` is no longer current.
(define (check-live! request [target-view #f])
  (define error (request-error request))
  (when error (raise (Request-Stop error)))
  (when (and target-view (sync/timeout 0 (Source-View-changed target-view)))
    (raise (Target-Retry))))

;; Report failure only while the request and `view` are still live, so a
;; cancellation or target change takes precedence over a stale
;; source-controller result.
(define (target-failed! request view stage detail)
  (check-live! request view)
  (raise (Target-Failure stage detail)))

;; Wait until `event` is ready, or until `check-live!` would stop or retry.
;; A `Source-Failure` becomes `Target-Failure` only when that second check
;; still finds the request and target view live.
(define (wait-live request event [target-view #f])
  (check-live! request target-view)
  (define result
    (sync (if target-view (choice-evt event (Source-View-changed target-view)) event)
          (Target-Request-shutdown request)
          (deadline-evt (Target-Request-deadline request))
          (Source-View-changed (Target-Request-view request))))
  (check-live! request target-view)
  (when (Source-Failure? result)
    (raise (Target-Failure "source or lookup failed" (Source-Failure-reason result))))
  result)

;; Ask the owner whether `view` is still current, and retry when it is not.
;; A snapshot or installed summary can already be stale. Check liveness
;; before building the event: `source-controller-current-view-evt` submits
;; its command immediately.
(define (validate-target! request view)
  (check-live! request view)
  (unless (wait-live request
                     (source-controller-current-view-evt
                       (Target-Request-controller request) view)
                     view)
    (raise (Target-Retry))))

;; Take the analysis outcome, then refresh the source before returning it.
;; When that refresh finds a changed view, the outcome is discarded and
;; resolution retries.
(define (current-target-outcome request view snapshot)
  (check-live! request view)
  (define acquisition
    (wait-live request
               (source-controller-analysis-evt (Target-Request-controller request) snapshot) view))
  (define available (Analysis-Handle-reusable-summary acquisition))
  (begin0
    (if available
        (Analysis-Outcome 'succeeded available)
        (wait-live request (Analysis-Handle-completion-evt acquisition) view))
    (refresh-target! request view)))

;; Refresh a returned outcome, including reusable success, errors, and
;; stopped work. A save during analysis can invalidate any of those before
;; the caller interprets it. A raised failure never produces an outcome, so
;; it is not refreshed.
(define (refresh-target! request view)
  (check-live! request view)
  (define refreshed
    (wait-live request
               (source-controller-observe-evt (Target-Request-controller request)
                                              (Source-View-source view)) view))
  (unless (eq? view (Source-Result-view refreshed)) (raise (Target-Retry)))
  (when (Source-Result-error refreshed)
    (target-failed! request view "validation read failed"
                    (Source-Failure-reason (Source-Result-error refreshed))))
  (validate-target! request view))

(define (checked-detail request view detail)
  (check-live! request view)
  (unless detail
    (log-racket-langserver-info "~a detail unavailable: ~v"
                                (Target-Request-feature request) (Target-Request-target request)))
  detail)

;; On `succeeded`, look the binding up in the summary; a missing entry is no
;; detail. `errors` means Check Syntax finished without facts: that also
;; returns no detail and does not fail the request. `failed`, `stopped`, and
;; any other outcome fail the request.
(define (resolve-shared request view snapshot lookup)
  (match (current-target-outcome request view snapshot)
    [(struct* Analysis-Outcome ([state 'succeeded] [value facts]))
     (checked-detail request view (lookup facts (Target-Request-target request)))]
    [(struct* Analysis-Outcome ([state 'errors]))
     (check-live! request view)
     (log-racket-langserver-info "~a analysis reported errors for ~a: ~v"
                                 (Target-Request-feature request)
                                 (Module-Binding-filepath (Target-Request-target request))
                                 (Target-Request-target request))
     #f]
    [outcome
     (target-failed! request view "analysis did not succeed" outcome)]))

;; `legacy` calls `wait` in the scope that afterwards shuts its worker down.
;; Pass `wait-live`, so cancellation and target changes leave that scope and
;; the shutdown still runs. The lookup does not use the acquired snapshot,
;; so validate the view before accepting its detail.
(define (resolve-legacy request view legacy)
  (define detail
    (legacy (Target-Request-target request) (lambda (event) (wait-live request event view))))
  (validate-target! request view)
  (checked-detail request view detail))

;; Fail a read error before the caller looks at content state. Unknown
;; content with no error is an invalidated observation; `resolve-target`
;; retries it. A missing file is not a read error.
(define (observe-target request)
  (check-live! request)
  (define acquired
    (wait-live request
               (source-controller-observe-evt
                 (Target-Request-controller request)
                 (path->source (Module-Binding-filepath (Target-Request-target request))))))
  (validate-target! request (Source-Result-view acquired))
  (when (Source-Result-error acquired)
    (target-failed! request (Source-Result-view acquired) "source read failed"
                    (Source-Failure-reason (Source-Result-error acquired))))
  acquired)

;; Acquire the target's current view and resolve this request against it.
;; `Target-Retry` repeats the acquisition. Unknown content has no trusted
;; text, so it retries. A missing file returns no snapshot and no detail.
(define (resolve-target request lookup legacy)
  (let loop ()
    (with-handlers ([Target-Retry? (lambda (_) (loop))])
      (define acquired (observe-target request))
      (define view (Source-Result-view acquired))
      (case (Source-View-content-state view)
        [(unknown) (raise (Target-Retry))]
        [(file-not-found) (check-live! request view) (values #f #f)]
        [else
         (define snapshot (Source-Result-snapshot acquired))
         (values snapshot
                 (if (shared-binding? (Target-Request-target request))
                     (resolve-shared request view snapshot lookup)
                     (resolve-legacy request view legacy)))]))))

(define (failure-response request stage detail)
  (define message (string-append (Target-Request-feature request) " " stage))
  (log-racket-langserver-error "~a request failed for ~a: ~a: ~v"
                               (Target-Request-feature request)
                               (Module-Binding-filepath (Target-Request-target request)) message detail)
  (error-response (Target-Request-id request) ErrorCode-RequestFailed message))

;; Prefer a live request error over the failure being reported. Cancellation,
;; deadline expiry, or shutdown replaces that failure.
(define (query-error-response request reason)
  (or (request-error request)
      (match reason
        [(Request-Stop response) response]
        [(Target-Failure stage detail) (failure-response request stage detail)]
        [reason (failure-response request "request failed" reason)])))

;; Turn every raised failure, including an unexpected exception, into one
;; protocol response. `compose` runs only when resolution returns.
(define (execute-target-query request lookup legacy compose)
  (with-handlers ([(lambda (_) #t) (lambda (reason) (query-error-response request reason))])
    (define-values (snapshot detail) (resolve-target request lookup legacy))
    (define value (compose snapshot detail))
    (check-live! request)
    (success/enc (Target-Request-id request) value)))

;; Return a thunk that resolves `target` and produces an encoded response.
;; The caller has already captured `view` and `deadline`; running the thunk
;; does not take the requesting document's lock.
;;
;; lookup: Called as `(lookup summary binding)` when `target` satisfies
;;   `shared-binding?`. Returns the feature detail, or #f when the summary
;;   has none.
;; legacy: Called as `(legacy binding wait)` for every other target, or #f.
;;   `wait` takes an event and returns its value, or escapes when the request
;;   stops or the target view changes. Pass #f only when `target` satisfies
;;   `shared-binding?`.
;; compose: Called as `(compose snapshot detail)` after resolution finishes,
;;   including when detail is absent. A missing file passes #f for both. An
;;   absent detail can still have a snapshot. A present detail always has a
;;   snapshot. The result is the success payload. Failures skip `compose`.
(define (target-query id controller deadline view target feature lookup legacy compose)
  (lambda ()
    (execute-target-query
      (Target-Request id controller deadline view target feature
                      (source-controller-shutdown-evt controller))
      lookup legacy compose)))
