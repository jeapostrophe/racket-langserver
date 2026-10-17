#lang racket/base

(require racket/match
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

(define (shared-binding? binding)
  (match-define (struct* Module-Binding ([filepath path] [submods submods]
                                         [phase+space phase])) binding)
  ;; Shared analysis needs the exact binding path as its source identity.
  ;; A successful conversion with a different path keeps the legacy lookup.
  (and (null? submods) (equal? phase 0) (equal? path (uri->path (path->uri path)))))

(define (resolve-target controller target feature lookup legacy finish-target check-request! wait failed)
  (define path (Module-Binding-filepath target))
  (define source (path->source path))
  (let loop ()
    ;; Check before constructing the refresh event, which submits target work.
    (check-request!)
    (define result
      (let/ec finish
        (define acquired (wait (source-controller-refresh-evt controller source)))
        (define target-view (Source-Result-view acquired))
        (define changed (Source-View-changed target-view))
        (define (again) (finish 'retry))
        (define (check-target!)
          (when (sync/timeout 0 changed) (again)))
        (define (target-failed message detail) (failed message detail check-target!))
        (define (target-wait event) (wait (choice-evt event changed) check-target!))
        (define (owner proc) (target-wait (source-controller-call-evt controller proc)))
        (define (validate!)
          ;; The refresh result is immutable, but the authority may have
          ;; changed before we received it. Eligibility is owner-decided.
          (unless (owner (lambda (c) (eq? target-view (source-controller-view c source))))
            (again)))
        (validate!)
        (when (Source-Result-error acquired)
          (target-failed (string-append feature " source read failed")
                         (Source-Failure-reason (Source-Result-error acquired))))
        (case (Source-View-state target-view)
          ;; An abandoned read can return an unobserved view. Retry;
          ;; this is not a missing file and has no snapshot to use.
          [(unobserved) (again)]
          [(missing)
           (check-request! check-target!)
           (finish #f)]
          [(unavailable) (target-failed (string-append feature " source unavailable") 'unavailable)])
        (define snapshot (Source-Result-snapshot acquired))
        (define shared? (shared-binding? target))
        (define outcome
          (if shared?
              (target-wait
                (owner (lambda (c) (source-controller-query-analysis! c snapshot))))
              (legacy target target-wait)))
        ;; Validate every shared outcome, including cached success and
        ;; failed/stopped attempts. A save can make that outcome obsolete.
        ;; Legacy disk lookup retains no target-snapshot freshness guarantee.
        (when shared?
          (define refreshed (target-wait (source-controller-refresh-evt controller source)))
          (unless (eq? target-view (Source-Result-view refreshed)) (again))
          (when (Source-Result-error refreshed)
            (target-failed (string-append feature " validation read failed")
                           (Source-Failure-reason (Source-Result-error refreshed)))))
        (validate!)
        (define detail
          (if shared?
              (match outcome
                [(struct* Analysis-Outcome ([state 'succeeded] [value facts]))
                 (lookup facts target)]
                [(struct* Analysis-Outcome ([state 'unsuccessful]))
                 (check-request! check-target!)
                 (log-racket-langserver-info "~a analysis unsuccessful for ~a: ~v" feature
                                             path target)
                 (finish #f)]
                [_ (target-failed (string-append feature " analysis did not succeed") outcome)])
              outcome))
        (check-request! check-target!)
        (unless detail
          (log-racket-langserver-info "~a detail unavailable: ~v" feature target))
        (and detail (finish-target snapshot detail))))
    (if (eq? result 'retry) (loop) result)))

(define (target-query id controller deadline view target feature lookup legacy finish-target
                      [compose values])
  (lambda ()
    (let/ec respond
      (define path (Module-Binding-filepath target))
      (define (failed message detail [check-target! void])
        ;; Failure mapping can follow an owner reply that is already obsolete.
        (check-request! check-target!)
        (log-racket-langserver-error "~a request failed for ~a: ~a: ~v" feature
                                     path message detail)
        (respond (error-response id ErrorCode-RequestFailed message)))
      (define shutdown (source-controller-shutdown-evt controller))
      (define (check-request! [check-target! void])
        (cond
          [(or (not view) (sync/timeout 0 (Source-View-changed view)))
           (respond (error-response id ErrorCode-RequestCancelled "Document changed or closed"))]
          [(>= (current-deadline-milliseconds) deadline)
           (respond
             (error-response id ErrorCode-RequestFailed (string-append feature " request deadline expired")))]
          [(sync/timeout 0 shutdown)
           (respond (error-response id ErrorCode-RequestFailed "Server shut down"))])
        (check-target!))
      ;; Inspect signals again after sync: cancellation and expiry win even
      ;; when a result is simultaneously ready. Target changes precede errors.
      (define (wait event [check-target! void])
        (check-request! check-target!)
        (define result
          (sync event shutdown (deadline-evt deadline) (Source-View-changed view)))
        (check-request! check-target!)
        (when (Source-Failure? result)
          (failed (string-append feature " source or lookup failed") (Source-Failure-reason result)
                  check-target!))
        result)
      (with-handlers ([(lambda (_) #t)
                       (lambda (reason) (failed (string-append feature " request failed") reason))])
        (define value
          (compose (resolve-target controller target feature lookup legacy finish-target
                                   check-request! wait failed)))
        (check-request!)
        (success/enc id value)))))
