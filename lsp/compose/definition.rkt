#lang racket/base

;; textDocument/definition. A local location, or no binding, is returned
;; immediately. A module binding is resolved by `target-query` after this
;; function returns its thunk.
(require racket/async-channel
         racket/port
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

;; Start the disk lookup on a thread under the current custodian. The event
;; becomes ready with a range, with #f when the binding has no definition, or
;; with `Source-Failure` when the lookup raises or the thread ends without
;; publishing a value.
(define (legacy-definition-evt binding)
  (define result (make-async-channel))
  (define worker
    (thread
      (lambda ()
        (async-channel-put
          result
          (with-handlers ([(lambda (_) #t) Source-Failure])
            (doc-get-definition-by-id (Module-Binding-filepath binding)
                                      (Module-Binding-submods binding)
                                      (Module-Binding-id binding)))))))
  (choice-evt result
              (wrap-evt (thread-dead-evt worker)
                        (lambda (_)
                          ;; `(list #f)` means the channel delivered #f. A
                          ;; timeout result of #f means the thread ended with
                          ;; nothing in the channel.
                          (define ready (sync/timeout 0 (wrap-evt result list)))
                          (if ready (car ready) (Source-Failure 'worker-exited))))))

;; Disk lookup for a binding outside the shared-analysis route. The custodian
;; owns the worker and is shut down when `wait` returns or escapes, so a
;; cancellation or retry does not leave the thread running.
(define (legacy-definition binding wait)
  (define custodian (make-custodian))
  (dynamic-wind
    void
    (lambda ()
      (wait
        (parameterize ([current-custodian custodian]
                       [current-output-port (open-output-nowhere)])
          (legacy-definition-evt binding))))
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
                      (hash-ref (Analysis-Summary-definition-ranges facts) binding #f))
                    legacy-definition
                    (lambda (snapshot range)
                      (and range (Location (Source-Snapshot-uri snapshot) range))))
      (success/enc id target)))
