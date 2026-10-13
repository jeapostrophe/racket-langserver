#lang racket/base

(require racket/async-channel
         racket/match
         racket/port
         "../../common/debug.rkt"
         "../../common/interfaces.rkt"
         "../../common/path-util.rkt"
         "../../common/time.rkt"
         "../../doclib/doc.rkt"
         "../../doclib/internal-types.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt"
         "../analysis-query.rkt"
         "../responses.rkt"
         "../safedoc.rkt"
         "../source-controller.rkt")

(provide definition-query)

(define (shared-binding? binding)
  (match-define (struct* Module-Binding ([filepath path] [submods submods]
                                         [phase+space phase])) binding)
  ;; Shared analysis needs the exact binding path as its source identity.
  ;; A successful conversion with a different path keeps the legacy lookup.
  (and (null? submods) (equal? phase 0) (equal? path (uri->path (path->uri path)))))

;; Excluded module/phase cases retain the old disk lookup. Its private worker
;; belongs to this request, unlike the coordinator's shared jobs.
(define (legacy-definition binding wait)
  (define custodian (make-custodian))
  (define result (make-async-channel))
  (dynamic-wind
    void
    (lambda ()
      (define worker
        (parameterize ([current-custodian custodian]
                       [current-output-port (open-output-nowhere)])
          (thread
            (lambda ()
              (async-channel-put
                result
                (with-handlers ([(lambda (_) #t) Source-Failure])
                  (doc-get-definition-by-id (Module-Binding-filepath binding)
                                            (Module-Binding-submods binding)
                                            (Module-Binding-id binding))))))))
      (wait (choice-evt result
                        (wrap-evt (thread-dead-evt worker)
                                  (lambda (_)
                                    ;; Tag #f so a completed missing definition
                                    ;; cannot be confused with an unready event.
                                    (define ready (sync/timeout 0 (wrap-evt result list)))
                                    (if ready (car ready) (Source-Failure 'worker-exited)))))))
    (lambda () (custodian-shutdown-all custodian))))

(define (resolve-definition-target controller target check-request! wait failed)
  (define path (Module-Binding-filepath target))
  (let loop ()
    ;; Check before constructing the refresh event, which submits target work.
    (check-request!)
    (define result
      (let/ec finish
        (define acquired (wait (source-controller-refresh-evt controller path)))
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
          (unless (owner (lambda (c) (eq? target-view (source-controller-view c path))))
            (again)))
        (validate!)
        (when (Source-Result-error acquired)
          (target-failed "Definition source read failed"
                         (Source-Failure-reason (Source-Result-error acquired))))
        (case (Source-View-state target-view)
          ;; An abandoned read can return an unobserved view. Retry;
          ;; this is not a missing file and has no snapshot to use.
          [(unobserved) (again)]
          [(missing)
           (check-request! check-target!)
           (finish #f)]
          [(unavailable) (target-failed "Definition source unavailable" 'unavailable)])
        (define snapshot (Source-Result-snapshot acquired))
        (define shared? (shared-binding? target))
        (define outcome
          (if shared?
              (target-wait
                (owner (lambda (c) (source-controller-acquire-analysis! c snapshot))))
              (legacy-definition target target-wait)))
        ;; Validate every shared outcome, including cached success and
        ;; failed/stopped attempts. A save can make that outcome obsolete.
        ;; Legacy disk lookup retains no target-snapshot freshness guarantee.
        (when shared?
          (define refreshed (target-wait (source-controller-refresh-evt controller path)))
          (unless (eq? target-view (Source-Result-view refreshed)) (again))
          (when (Source-Result-error refreshed)
            (target-failed "Definition validation read failed"
                           (Source-Failure-reason (Source-Result-error refreshed)))))
        (validate!)
        (define range
          (if shared?
              (match outcome
                [(struct* Analysis-Outcome ([state 'succeeded] [value facts]))
                 (hash-ref (Analysis-Facts-definitions facts) target #f)]
                [(struct* Analysis-Outcome ([state 'unsuccessful]))
                 (check-request! check-target!)
                 (log-racket-langserver-info "Definition analysis unsuccessful for ~a: ~v"
                                             path target)
                 (finish #f)]
                [_ (target-failed "Definition analysis did not succeed" outcome)])
              outcome))
        (check-request! check-target!)
        (unless range
          (log-racket-langserver-info "Definition span unavailable: ~v" target))
        (and range (Location (source-uri (Source-Snapshot-source snapshot)) range))))
    (if (eq? result 'retry) (loop) result)))

(define (definition-query safe-doc id uri pos)
  (define deadline (+ (current-deadline-milliseconds)
                      (* 1000 (current-analysis-request-timeout))))
  (define controller (current-source-controller))
  (define-values (view target)
    (with-read-safedoc safe-doc
      (lambda (sd)
        (values (SafeDoc-source-view sd) (doc-definition (SafeDoc-doc sd) uri pos)))))
  (cond
    [(not (Module-Binding? target)) (success/enc id target)]
    [else
     (lambda ()
       (let/ec respond
         (define path (Module-Binding-filepath target))
         (define (failed message detail [check-target! void])
           ;; Failure mapping can follow an owner reply that is already obsolete.
           (check-request! check-target!)
           (log-racket-langserver-error "Definition request failed for ~a: ~a: ~v"
                                        path message detail)
           (respond (error-response id ErrorCode-RequestFailed message)))
         (define shutdown (source-controller-shutdown-evt controller))
         (define (check-request! [check-target! void])
           (cond
             [(or (not view) (sync/timeout 0 (Source-View-changed view)))
              (respond (error-response id ErrorCode-RequestCancelled "Document changed or closed"))]
             [(>= (current-deadline-milliseconds) deadline)
              (respond
                (error-response id ErrorCode-RequestFailed "Definition request deadline expired"))]
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
             (failed "Definition source or lookup failed" (Source-Failure-reason result)
                     check-target!))
           result)
         (with-handlers ([(lambda (_) #t)
                          (lambda (reason) (failed "Definition request failed" reason))])
           (success/enc
             id (resolve-definition-target controller target check-request! wait failed)))))]))
