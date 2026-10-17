#lang racket/base

(require racket/async-channel
         racket/match
         racket/port
         "../../common/debug.rkt"
         "../../common/interfaces.rkt"
         "../../common/time.rkt"
         "../../doclib/internal-types.rkt"
         "../../doclib/doc.rkt"
         "target.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt"
         "../analysis-query.rkt"
         "../responses.rkt"
         "../safedoc.rkt"
         "../source-controller.rkt")

(provide definition-query)

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

(define (definition-query safe-doc id uri pos)
  (define deadline (+ (current-deadline-milliseconds)
                      (* 1000 (current-analysis-request-timeout))))
  (define controller (current-source-controller))
  (define-values (view target)
    (with-read-safedoc safe-doc
      (lambda (sd)
        (values (SafeDoc-source-view sd) (doc-definition (SafeDoc-doc sd) uri pos)))))
  (if (Module-Binding? target)
      (target-query id controller deadline view target "Definition"
                    (lambda (facts binding)
                      (hash-ref (Analysis-Facts-definitions facts) binding #f))
                    legacy-definition
                    (lambda (snapshot range)
                      (Location (Source-Snapshot-uri snapshot) range)))
      (success/enc id target)))
