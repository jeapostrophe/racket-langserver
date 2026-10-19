#lang racket/base

(module+ test
  (require rackunit
           racket/async-channel
           "../../common/source.rkt"
           "../../workspace/source-store.rkt"
           "../../lsp/analysis.rkt"
           "../../lsp/source-controller/analysis-scheduler.rkt"
           "../../lsp/source-controller/completion-event.rkt")

  (define (await event)
    (define result (sync/timeout 10 (wrap-evt event list)))
    (unless result (error 'await "event did not complete"))
    (car result))

  (define (snapshot name)
    (Source-Snapshot (path->source (build-path (current-directory) name))
                     (gensym 'revision) 'disk "#lang racket/base\n(define value 1)\nvalue\n"))

  (define (with-scheduler proc)
    (define custodian (make-custodian))
    (define events (make-async-channel))
    (define started (make-async-channel))
    (define release (make-async-channel))
    (define scheduler
      (make-analysis-scheduler
        events #:slots 1 #:resyntax #f
        #:analyze (lambda (snapshot)
                    (async-channel-put started snapshot)
                    (async-channel-get release)
                    (analyze-source snapshot))))
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([current-custodian custodian])
          (proc scheduler events started release)))
      (lambda ()
        (stop-analysis-scheduler! scheduler)
        (custodian-shutdown-all custodian))))

  (define (accept-success snapshot kind completion parent product)
    (check-eq? kind 'analysis)
    (check-false parent)
    (check-false (evt-ready? completion))
    (check-true (Analysis-Product-succeeded? product))
    (Analysis-Outcome 'succeeded (Analysis-Product-facts product)))

  (test-case "FIFO capacity, shared requests, and acceptance before completion"
    (with-scheduler
      (lambda (scheduler events started release)
        (define a (snapshot "schedule-a.rkt"))
        (define b (snapshot "schedule-b.rkt"))
        (define first (request-analysis! scheduler a))
        (define second (request-analysis! scheduler b))
        (check-eq? first (request-analysis! scheduler a))
        (pump-analysis-scheduler! scheduler (lambda (_) #t))
        (check-eq? (await started) a)
        (check-false (sync/timeout 0 started))
        (async-channel-put release #t)
        (define accepted? #f)
        (finish-analysis-task! scheduler (await events)
                               (lambda args
                                 (define result (apply accept-success args))
                                 (set! accepted? #t)
                                 result))
        (check-true accepted?)
        (check-eq? (Analysis-Outcome-state (await first)) 'succeeded)
        (check-false (scheduled-analysis-event scheduler (Source-Snapshot-source a)))
        (pump-analysis-scheduler! scheduler (lambda (_) #t))
        (check-eq? (await started) b)
        (async-channel-put release #t)
        (finish-analysis-task! scheduler (await events) accept-success)
        (check-eq? (Analysis-Outcome-state (await second)) 'succeeded))))

  (test-case "retry rejects the old completed message for the same revision"
    (with-scheduler
      (lambda (scheduler events started release)
        (define a (snapshot "schedule-retry.rkt"))
        (define first (request-analysis! scheduler a))
        (pump-analysis-scheduler! scheduler (lambda (_) #t))
        (await started)
        (async-channel-put release #t)
        (define old (await events))
        (finish-analysis-task! scheduler old
                               (lambda (_snapshot _kind _completion _parent _product)
                                 (Analysis-Outcome 'failed 'rejected)))
        (check-eq? (Analysis-Outcome-state (await first)) 'failed)
        (define retry (request-analysis! scheduler a))
        (check-not-eq? first retry)
        (pump-analysis-scheduler! scheduler (lambda (_) #t))
        (await started)
        (finish-analysis-task! scheduler old (lambda args (error 'stale "old result accepted")))
        (check-false (evt-ready? retry))
        (async-channel-put release #t)
        (finish-analysis-task! scheduler (await events) accept-success)
        (check-eq? (Analysis-Outcome-state (await retry)) 'succeeded))))

  (test-case "queued stale work is settled without starting and shutdown resolves queued work"
    (with-scheduler
      (lambda (scheduler _events started _release)
        (define a (snapshot "schedule-stale.rkt"))
        (define stale (request-analysis! scheduler a))
        (pump-analysis-scheduler! scheduler (lambda (_) #f))
        (check-eq? (Analysis-Outcome-state (await stale)) 'stopped)
        (check-false (sync/timeout 0 started))
        (define queued (request-analysis! scheduler a))
        (stop-analysis-scheduler! scheduler)
        (check-eq? (Analysis-Outcome-state (await queued)) 'stopped)
        (check-eq? (Analysis-Outcome-state (await (request-analysis! scheduler a))) 'stopped)))))
