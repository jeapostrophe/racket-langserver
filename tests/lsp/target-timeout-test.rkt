#lang racket/base

;; Target policies use real source observations and supervised work. Only the
;; owner runs on this test thread; request threads submit ordinary commands.
(require rackunit
         racket/async-channel
         "../../common/interfaces.rkt"
         "../../common/path-util.rkt"
         "../../common/source.rkt"
         "../../common/time.rkt"
         "../../doclib/doc.rkt"
         "../../doclib/internal-types.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/compose/target.rkt"
         "../../lsp/compose/hover.rkt"
         "../../lsp/compose/definition.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/source-controller.rkt"
         (submod "../../lsp/source-controller.rkt" owner)
         "../../lsp/source-controller/completion-event.rkt"
         "../../workspace/source-store.rkt"
         "controller-support.rkt")

(require/expose "../../lsp/compose/target.rkt" (wait-live Target-Request Target-Failure?))

(define text "#lang racket/base\n(define value 1)\n")
(define requester (path->source (build-path scratch "timeout-requester.rkt")))
(define target-path (build-path scratch "timeout-target.rkt"))
(define binding (Module-Binding target-path '() 0 'value))

(define (with-target proc #:read [read (lambda (_) text)]
                     #:analyze [analyze analyze-source]
                     #:read-timeout [read-timeout 3]
                     #:analysis-timeout [analysis-timeout 90])
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (define controller
      (make-source-controller #:read read #:analyze analyze #:resyntax #f
                              #:read-timeout read-timeout #:analysis-timeout analysis-timeout))
    (define sd (new-safedoc (source-uri requester) text 0))
    (parameterize ([current-source-controller controller])
      (dynamic-wind
        (lambda () (source-controller-open! controller requester sd))
        (lambda ()
          (drive-controller controller (with-read-safedoc sd SafeDoc-analysis-completion) 90)
          (define view (with-read-safedoc sd SafeDoc-source-view))
          (define (query seconds feature #:compose [compose (lambda (_ detail) detail)]
                         #:fallback [fallback (lambda () "local")])
            (target-query 1 controller (+ (current-deadline-milliseconds) (* 1000 seconds))
                          view binding feature (lambda (_facts _binding) "detail") compose
                          #:fallback (and (equal? feature "Hover") fallback)))
          (define (run deferred)
            (define replies (make-async-channel))
            (thread (lambda () (async-channel-put replies (deferred))))
            (drive-controller controller replies 90))
          (proc controller sd query run))
        (lambda ()
          (source-controller-shutdown! controller)
          (custodian-shutdown-all custodian))))))

(module+ test
  (test-case "target defaults and already-expired queries start no disk work"
    (check-equal? (current-hover-target-timeout) 0.2)
    (check-equal? (current-definition-target-timeout) 10.0)
    (define reads (make-async-channel))
    (with-target
      (lambda (_controller _sd query run)
        (check-equal? (hash-ref (run (query 0 "Hover")) 'result) "local")
        (check-error (run (query 0 "Definition")) ErrorCode-RequestFailed)
        (check-false (sync/timeout 0 reads)))
      #:read (lambda (_) (async-channel-put reads #t) text)))

  (test-case "only actual read and analysis deadlines permit hover fallback"
    (for ([stage (in-list '(read analysis))])
      (for ([mode (in-list '(deadline raised-symbol permission forged-timeout forged-stop))])
        (define (work)
          (case mode
            [(deadline) (sync never-evt)]
            [(raised-symbol) (raise (if (eq? stage 'read) 'read-timeout 'analysis-timeout))]
            [(forged-timeout) (Analysis-Outcome 'timed-out #f)]
            [(forged-stop) (Analysis-Outcome 'stopped 'forged)]
            [(permission)
             (raise (exn:fail:filesystem:errno "permission denied" (current-continuation-marks)
                                               '(13 . posix)))]))
        (with-target
          (lambda (_controller _sd query run)
            (define hover (run (query 5 "Hover")))
            (if (eq? mode 'deadline)
                (check-equal? (hash-ref hover 'result) "local")
                (check-error hover ErrorCode-RequestFailed))
            (check-error (run (query 5 "Definition")) ErrorCode-RequestFailed))
          #:read-timeout 0.1 #:analysis-timeout 0.5
          #:read (if (eq? stage 'read) (lambda (_) (work)) (lambda (_) text))
          #:analyze (lambda (snapshot)
                      (if (and (eq? stage 'analysis)
                               (equal? (Source-Snapshot-id snapshot) (source-id (path->source target-path))))
                          (work)
                          (analyze-source snapshot)))))))

  (test-case "fallback exceptions stay errors and requester changes win during composition"
    (with-target
      (lambda (controller sd query run)
        (check-error (run (query 0 "Hover" #:fallback (lambda () (raise 'fallback-crashed))))
                     ErrorCode-RequestFailed)
        (define cancelled
          ((query 0 "Hover"
                  #:fallback (lambda ()
                               (source-controller-edit! controller sd
                                                        (lambda (doc) (doc-update-version! doc 1)))
                               "local"))))
        (check-error cancelled ErrorCode-ContentModified)))
    (with-target
      (lambda (controller _sd query _run)
        (check-error
          ((query 0 "Hover"
                  #:fallback (lambda () (source-controller-shutdown! controller) "local")))
          ErrorCode-RequestFailed))))

  (test-case "normal composition failures at expiry cannot become local fallback"
    (with-target
      (lambda (_controller _sd query run)
        (check-error
          (run (query 0.5 "Hover"
                      #:compose (lambda (_ detail)
                                  (sync (deadline-evt (+ (current-deadline-milliseconds) 500)))
                                  (raise 'composition-failed))))
          ErrorCode-RequestFailed))))

  (test-case "ready worker failures beat a simultaneously ready target deadline"
    (with-target
      (lambda (controller sd _query _run)
        (define completion (make-completion-event))
        (resolve-completion! completion (Analysis-Outcome 'failed 'analysis-timeout))
        (define request
          (Target-Request 1 controller (current-deadline-milliseconds)
                          (with-read-safedoc sd SafeDoc-source-view) binding "Hover"
                          (source-controller-shutdown-evt controller)))
        (check-pred Target-Failure?
                    (with-handlers ([(lambda (_) #t) values])
                      (wait-live request completion)))))))
