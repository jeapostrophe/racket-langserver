#lang racket/base

(require rackunit
         "../../common/path-util.rkt"
         "../../doclib/doc.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/source-controller.rkt"
         "../../workspace/source-store.rkt"
         (submod "../../lsp/source-controller.rkt" test-support)
         (submod "source-controller-test.rkt" test-support))

(define text "#lang racket/base\n(require racket/list)\n(define value (first '(1 2)))\nvalue\n")

(define (start-analysis controller id)
  (source-controller-acquire-analysis! controller (source-controller-snapshot controller id)))

(define (finish-analysis controller event)
  (source-controller-handle! controller (await (source-controller-events controller)))
  (await event))

(module+ test
  (test-case "virtual sources expand client text with collection imports and stable backend paths"
    (define controller
      (make-source-controller #:read (lambda (_) (error 'disk "virtual source reached disk"))
                              #:resyntax #f))
    (define uri "untitled:Untitled-1")
    (define id (uri->source-id uri))
    (define sd (new-safedoc uri text 0))
    (define source (with-read-doc sd Doc-source))
    (source-controller-open! controller id sd)
    (define first (start-analysis controller id))
    (check-eq? 'succeeded (Analysis-Outcome-state (finish-analysis controller first)))
    (check-true (with-read-doc sd doc-trace-latest?))
    (define edited "#lang racket/base\n(define value 2)\nvalue\n")
    (with-write-safedoc sd
      (lambda (sd)
        (doc-reset! (SafeDoc-doc sd) edited)
        (doc-update-version! (SafeDoc-doc sd) 1)
        (source-controller-edited! controller id sd)))
    (define snapshot (source-controller-snapshot controller id))
    (check-eq? source (Source-Snapshot-source snapshot))
    (check-equal? (Source-Snapshot-text snapshot) edited)
    (check-eq? 'succeeded (Analysis-Outcome-state (finish-analysis controller (start-analysis controller id))))
    (check-false (file-exists? (source-backend-path source)))
    (source-controller-close! controller id sd)
    (check-eq? 'closed (Source-View-origin (source-controller-view controller id)))
    (check-false (source-controller-snapshot controller id))
    (source-controller-shutdown! controller))

  (test-case "virtual close invalidates waiters and rejects late completion after reopen"
    (define controller (make-source-controller #:resyntax #f))
    (define uri "git:/project/main.rkt?rev=old#cell")
    (define id (uri->source-id uri))
    (define old (new-safedoc uri text 7))
    (define published '())
    (source-controller-set-notifier! controller
                                     (lambda (_ params) (set! published (cons params published))))
    (source-controller-open! controller id old)
    (define previous (source-controller-view controller id))
    (define waiting (start-analysis controller id))
    (define late-message (await (source-controller-events controller)))
    (source-controller-close! controller id old)
    (check-not-false (sync/timeout 0 (Source-View-changed previous)))
    (check-eq? 'stopped (Analysis-Outcome-state (await waiting)))
    (check-false (with-read-safedoc old SafeDoc-source-view))
    (check-false (with-read-safedoc old SafeDoc-analysis))
    (define reopened (new-safedoc uri text 7))
    (check-not-equal? (with-read-doc old (lambda (doc) (source-backend-path (Doc-source doc))))
                      (with-read-doc reopened (lambda (doc) (source-backend-path (Doc-source doc)))))
    (source-controller-open! controller id reopened)
    (source-controller-handle! controller late-message)
    (check-equal? (length published) 1 "only close clears diagnostics")
    (check-false (with-read-doc reopened doc-trace-latest?))
    (check-eq? 'succeeded (Analysis-Outcome-state (finish-analysis controller (start-analysis controller id))))
    (source-controller-handle! controller late-message)
    (check-equal? (length published) 2)
    (source-controller-close! controller id reopened)
    (source-controller-shutdown! controller))

  (test-case "virtual refresh never reads disk and invalidation preserves virtual revisions"
    (with-session
      (make-source-controller #:read (lambda (_) (error 'disk "virtual source reached disk")) #:resyntax #f)
      (lambda (session)
        (define id (uri->source-id "notebook:/project/main.rkt#cell"))
        (define closed (await (refresh session id)))
        (check-eq? 'closed (Source-View-origin (Source-Result-view closed)))
        (check-equal? (Source-Result-error closed) (Source-Failure 'closed))
        (check-false (Source-Result-snapshot closed))
        (define sd (new-safedoc (Virtual-Id-uri id) text 0))
        (owner session (lambda (controller) (source-controller-open! controller id sd)))
        (define before (view session id))
        (define opened (await (refresh session id)))
        (check-equal? text (Source-Snapshot-text (Source-Result-snapshot opened)))
        (check-false (Source-Result-error opened))
        (owner session (lambda (controller) (source-controller-invalidate! controller id)))
        (check-eq? before (view session id))
        (owner session (lambda (controller) (source-controller-close! controller id sd)))))))
