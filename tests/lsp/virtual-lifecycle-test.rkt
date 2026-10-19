#lang racket/base

(require "../../common/source.rkt"
         rackunit
         "../../common/path-util.rkt"
         "../../doclib/doc.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/source-controller.rkt"
         (submod "../../lsp/source-controller.rkt" owner)
         "../../workspace/source-store.rkt"
         (submod "source-controller-test.rkt" test-support))

(define text "#lang racket/base\n(require racket/list)\n(define value (first '(1 2)))\nvalue\n")

(define (start-analysis controller source)
  (source-controller-ensure-analysis! controller (source-controller-snapshot controller source)))

(define (finish-analysis controller event)
  (source-controller-handle! controller (await (source-controller-events controller)))
  (await event))

(module+ test
  (test-case "virtual sources expand client text with collection imports and original URI syntax sources"
    (define controller
      (make-source-controller #:read (lambda (_) (error 'disk "virtual source reached disk"))
                              #:resyntax #f))
    (define uri "untitled:Untitled-1")
    (define source (uri->source uri))
    (define sd (new-safedoc uri text 0))
    (check-equal? (with-read-doc sd Doc-uri) uri)
    (source-controller-open! controller source sd)
    (define first (start-analysis controller source))
    (check-eq? 'succeeded (Analysis-Outcome-state (finish-analysis controller first)))
    (check-true (with-read-doc sd doc-trace-latest?))
    (define edited "#lang racket/base\n(define value 2)\nvalue\n")
    (source-controller-edit! controller sd
                             (lambda (doc)
                               (doc-reset! doc edited)
                               (doc-update-version! doc 1)))
    (define snapshot (source-controller-snapshot controller source))
    (check-equal? uri (Source-Snapshot-uri snapshot))
    (check-equal? (source-id source) (Source-Snapshot-id snapshot))
    (check-equal? (Source-Snapshot-text snapshot) edited)
    (check-eq? 'succeeded
               (Analysis-Outcome-state
                 (finish-analysis controller (start-analysis controller source))))
    (source-controller-close! controller source sd)
    (check-false (source-controller-view controller source))
    (check-false (source-controller-snapshot controller source))
    (source-controller-shutdown! controller))

  (test-case "virtual close invalidates waiters and rejects late completion after reopen"
    (define controller (make-source-controller #:resyntax #f))
    (define uri "git:/project/main.rkt?rev=old#cell")
    (define source (uri->source uri))
    (define old (new-safedoc uri text 7))
    (define published '())
    (source-controller-set-notifier! controller
                                     (lambda (_ params) (set! published (cons params published))))
    (source-controller-open! controller source old)
    (define previous (source-controller-view controller source))
    (define old-snapshot (source-controller-snapshot controller source))
    (define waiting (start-analysis controller source))
    (define late-message (await (source-controller-events controller)))
    (source-controller-close! controller source old)
    (check-not-false (sync/timeout 0 (Source-View-changed previous)))
    (check-false (source-controller-view controller source))
    (check-false (source-controller-snapshot controller source))
    (check-eq? 'stopped (Analysis-Outcome-state (await waiting)))
    (check-equal? (await (source-controller-ensure-analysis! controller old-snapshot))
                  (Analysis-Outcome 'stopped 'ineligible))
    (source-controller-handle! controller late-message)
    (check-false (source-controller-view controller source))
    (check-false (with-read-safedoc old SafeDoc-source-view))
    (check-false (with-read-safedoc old SafeDoc-analysis-completion))
    (define reopened (new-safedoc uri text 7))
    (check-equal? (with-read-doc old Doc-uri) (with-read-doc reopened Doc-uri))
    (source-controller-open! controller source reopened)
    (check-equal? (Source-View-id previous)
                  (Source-View-id (source-controller-view controller source)))
    (check-not-eq? (Source-View-revision previous)
                   (Source-View-revision (source-controller-view controller source)))
    (check-equal? (await (source-controller-ensure-analysis! controller old-snapshot))
                  (Analysis-Outcome 'stopped 'ineligible))
    (source-controller-handle! controller late-message)
    (check-equal? (length published) 1 "only close clears diagnostics")
    (check-false (with-read-doc reopened doc-trace-latest?))
    (check-eq? 'succeeded
               (Analysis-Outcome-state
                 (finish-analysis controller (start-analysis controller source))))
    (source-controller-handle! controller late-message)
    (check-equal? (length published) 2)
    (source-controller-close! controller source reopened)
    (source-controller-shutdown! controller))

  (test-case "virtual refresh never reads disk and invalidation preserves virtual revisions"
    ;; This thread owns lifecycle transitions and consumes the controller commands.
    (define controller
      (make-source-controller #:read (lambda (_) (error 'disk "virtual source reached disk"))
                              #:resyntax #f))
    (define source (uri->source "notebook:/project/main.rkt#cell"))
    (define (observe)
      (define reply (source-controller-observe-evt controller source))
      (let loop ()
        (unless (sync/timeout 0 reply)
          (source-controller-handle! controller (await (source-controller-events controller)))
          (loop)))
      (await reply))
    (dynamic-wind
      void
      (lambda ()
        (define closed (observe))
        (check-false (Source-Result-view closed))
        (check-equal? (Source-Result-error closed) (Source-Failure 'closed))
        (check-false (Source-Result-snapshot closed))
        (define sd (new-safedoc (source-uri source) text 0))
        (source-controller-open! controller source sd)
        (define before (Source-Result-view (observe)))
        (define opened (observe))
        (check-equal? text (Source-Snapshot-text (Source-Result-snapshot opened)))
        (check-false (Source-Result-error opened))
        (source-controller-invalidate! controller source)
        (check-eq? before (Source-Result-view (observe)))
        (source-controller-close! controller source sd)
        (define after-close (observe))
        (check-false (Source-Result-view after-close))
        (check-false (Source-Result-snapshot after-close))
        (check-equal? (Source-Result-error after-close) (Source-Failure 'closed))
        (void))
      (lambda () (source-controller-shutdown! controller)))))
