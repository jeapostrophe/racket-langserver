#lang racket/base

;; Controller observations, queue barriers, and held work for injected-controller tests.
;; Session ownership and protocol helpers live in the existing shared test client.
(require rackunit
         racket/async-channel
         racket/runtime-path
         "../client.rkt"
         "../../common/source.rkt"
         "../../common/json-util.rkt"
         "../../lsp/source-controller.rkt"
         (only-in (submod "../../lsp/source-controller.rkt" owner) source-controller-handle!)
         "../../workspace/source-store.rkt")

(provide await observation acquire view refresh notify open! close! change! request!
         check-error barrier controller-barrier shutdown! response with-session drive-controller
         scratch (struct-out Session) (struct-out Gate) wait-at-gate release-gate!)

(define-runtime-path scratch-path "../../.agent-scratch")
(define scratch (simplify-path scratch-path #f))

(define (observation session target)
  (define result (await (refresh session target)))
  (when (Source-Failure? result)
    (error 'observation "source command failed: ~v" (Source-Failure-reason result)))
  result)

;; Observation of a closed file reads disk. Tests with a held read must use
;; their captured result/view and current-view validation instead.
(define (view session target)
  (Source-Result-view (observation session target)))

(define (acquire session target)
  (define result (observation session target))
  (when (Source-Result-error result)
    (error 'acquire "source observation failed: ~v" (Source-Result-error result)))
  (define acquisition
    (await (source-controller-analysis-evt (Session-controller session)
                                           (Source-Result-snapshot result))))
  (when (Source-Failure? acquisition)
    (error 'acquire "analysis command failed: ~v" (Source-Failure-reason acquisition)))
  (Analysis-Handle-completion-evt acquisition))

;; Validation orders preceding controller commands without reading disk or
;; starting analysis. A different store's revision can never be current in this session.
(define barrier-view
  (source-store-view (make-source-store) (path->source (build-path scratch "barrier.rkt"))))

(define (controller-barrier session)
  (check-false
    (await (source-controller-current-view-evt (Session-controller session) barrier-view))))

(define (refresh session path)
  (source-controller-observe-evt (Session-controller session)
                                 (if (Source? path) path (path->source path))))

;; A client request is an ordering barrier for preceding client notifications.
;; Controller commands are a separate queue and cannot serve that purpose.
(define (barrier session)
  (async-channel-put (Session-clients session)
                     (hasheq 'id 100 'method "textDocument/hover"
                             'params (hasheq)))
  (response session 100))

(define (check-error result code [message #f])
  (define error (hash-ref result 'error))
  (check-equal? (hash-ref error 'code) (->jsexpr code))
  (when message (check-equal? (hash-ref error 'message) message)))

;; Drive only a test-owned controller. Consumer sessions have their own event pump.
(define (drive-controller controller event [timeout 5])
  (let loop ()
    (define next
      (await (choice-evt (wrap-evt event (lambda (value) (cons 'result value)))
                         (wrap-evt (source-controller-events controller)
                                   (lambda (value) (cons 'event value))))
             timeout))
    (if (eq? (car next) 'result)
        (cdr next)
        (begin (source-controller-handle! controller (cdr next)) (loop)))))

;; Receiving a Gate establishes that this invocation reached the held work.
;; Each analyzer or reader interprets the released command in its own way.
(struct Gate (input release) #:transparent)

(define (wait-at-gate gates input)
  (define release (make-async-channel))
  (async-channel-put gates (Gate input release))
  (async-channel-get release))

(define (release-gate! gate [command 'go])
  (async-channel-put (Gate-release gate) command))
