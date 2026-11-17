#lang racket/base

;; Tests for closed/reopened analysis retention, URI alias transfer,
;; incarnation isolation, and visual query availability across refreshes.
(module+ test
  (require "controller-support.rkt"
           rackunit
           racket/async-channel
           racket/list
           "../../common/interfaces.rkt"
           "../../common/path-util.rkt"
           "../../common/source.rkt"
           "../../doclib/doc.rkt"
           "../../doclib/internal-types.rkt"
           "../../lsp/analysis.rkt"
           "../../lsp/analysis-query.rkt"
           "../../lsp/safedoc.rkt"
           "../../lsp/source-controller.rkt"
           (submod "../../lsp/source-controller.rkt" owner)
           "../../workspace/api.rkt"
           "../../workspace/current.rkt"
           "../../workspace/source-store.rkt")

  (define text "#lang racket/base\n(define value 1)\nvalue\n")
  (define canonical-uri (path->uri (build-path scratch "uri-identity.rkt")))
  (define localhost-uri (regexp-replace #rx"^file://" canonical-uri "file://localhost"))

  (for ([failure (in-list '(raise errors))])
    (test-case (format "visual waiter rechecks availability after a later ~a refresh" failure)
      (define probes (make-async-channel))
      (define controller
        (make-source-controller
          #:resyntax #f
          #:analyze (lambda (snapshot)
                      (case (wait-at-gate probes snapshot)
                        [(raise) (raise 'refresh-failed)]
                        [(errors)
                         (define product (analyze-source snapshot))
                         (struct-copy Analysis-Product product [succeeded? #f] [summary #f])]
                        [else (analyze-source snapshot)]))))
      (define path (build-path scratch "visual-refresh-race.rkt"))
      (with-buffer controller path text
                   (lambda (sd source)
                     (define first-gate (await probes))
                     (define first (with-read-safedoc sd SafeDoc-analysis-completion))
                     (define read-called? #f)
                     (define deferred
                       (visual-query sd 900 'inlay-hints
                                     (lambda (_)
                                       (set! read-called? #t)
                                       "adjusted trace")))
                     (check-true (procedure? deferred))
                     (release-gate! first-gate)
                     (check-eq? 'succeeded
                                (Analysis-Outcome-state (drive-controller controller first)))
                     (source-controller-refresh-analysis! controller source)
                     (define second-gate (await probes))
                     (define second (with-read-safedoc sd SafeDoc-analysis-completion))
                     (release-gate! second-gate failure)
                     (drive-controller controller second)
                     (define response (deferred))
                     (cond
                       [(eq? failure 'raise)
                        (check-false read-called?)
                        (check-error response ErrorCode-RequestFailed)]
                       [else
                        (check-true read-called?)
                        (check-true (Visual-Delivery-degraded? response))
                        (check-equal? (hash-ref (Visual-Delivery-response response) 'result)
                                      "adjusted trace")])))))

  (test-case "closed analyses retain disk authority and reusable facts without publishing"
    (with-session (make-source-controller #:read (lambda (_) text))
                  (lambda (session)
                    (define path (build-path scratch "closed-analysis.rkt"))
                    (await (refresh session path))
                    (define result (await (acquire session path)))
                    (check-eq? 'succeeded (Analysis-Outcome-state result))
                    (check-true (Analysis-Summary? (Analysis-Outcome-value result)))
                    (check-eq? 'disk (Source-View-origin (view session path)))
                    (check-false (workspace-definition-location current-workspace (Module-Binding path '() 0 'value)))
                    (check-equal? (notifications session) '())
                    (check-eq? result (await (acquire session path))))))

  (test-case "delayed completion across edit/close/reopen cannot install or publish"
    (define controller (make-source-controller #:resyntax #f))
    (define path (build-path scratch "late-incarnation.rkt"))
    (define published '())
    (source-controller-set-notifier!
      controller
      (lambda (_method params) (set! published (cons params published))))
    (define sd (new-safedoc (path->uri path) text 7))
    (source-controller-open! controller (path->source path) sd)
    (define first (source-controller-ensure-analysis!
                    controller (source-controller-snapshot controller (path->source path))))
    (define old-message (await (source-controller-events controller)))
    (source-controller-edit! controller sd (lambda (doc) (doc-update-version! doc 8)))
    (source-controller-handle! controller old-message)
    (check-eq? 'stopped (Analysis-Outcome-state (await first)))
    (check-equal? published '())
    (source-controller-close! controller (path->source path) sd)
    (define reopened (new-safedoc (path->uri path) text 7))
    (source-controller-open! controller (path->source path) reopened)
    (define next (source-controller-ensure-analysis!
                   controller (source-controller-snapshot controller (path->source path))))
    (check-eq? 'succeeded (Analysis-Outcome-state (drive-controller controller next)))
    (check-equal? (length published) 2)
    (source-controller-handle! controller old-message)
    (check-equal? (length published) 2)
    (check-true (with-read-doc reopened doc-trace-latest?))
    (check-false (with-read-safedoc sd SafeDoc-source-view))
    (source-controller-close! controller (path->source path) reopened)
    (source-controller-shutdown! controller))

  (test-case "revision invalidation with unchanged client version waits for the new attempt"
    (define probes (make-async-channel))
    (define fail-next? #f)
    (with-session
      (make-source-controller
        #:analyze (lambda (snapshot)
                    (define product ((controlled-analyzer probes) snapshot))
                    (if fail-next?
                        (Analysis-Product #f #f '() (Analysis-Product-trace product) #f #f)
                        product)))
      (lambda (session)
        (define path (build-path scratch "unchanged-client-version.rkt"))
        (open! session path text 7)
        (release-gate! (await probes))
        (barrier session)
        (await (acquire session path))
        (request! session 11 "textDocument/semanticTokens/full" path)
        (define previous-tokens (hash-ref (response session 11) 'result))
        (set! fail-next? #t)
        (notify session "workspace/didRenameFiles"
                (hasheq 'files (list (hasheq 'oldUri (path->uri path)
                                             'newUri (path->uri (build-path scratch "renamed.rkt"))))))
        (define probe (await probes))
        (define range-params
          (hasheq 'range (hasheq 'start (hasheq 'line 0 'character 0)
                                 'end (hasheq 'line 3 'character 0))))
        (request! session 12 "textDocument/inlayHint" path range-params)
        (request! session 13 "textDocument/semanticTokens/full" path)
        (barrier session)
        (check-false (sync/timeout 0 (Session-responses session)))
        (release-gate! probe)
        (check-equal? (hash-ref (response session 12) 'result) '())
        (check-equal? (hash-ref (response session 13) 'result) previous-tokens)
        (close! session path)
        (barrier session))))

  (test-case "available analysis answers visual queries during a held same-revision refresh"
    (define probes (make-async-channel))
    (define controller (make-source-controller #:analyze (controlled-analyzer probes) #:resyntax #f))
    (with-buffer controller (build-path scratch "available-refresh.rkt") text
                 (lambda (sd source)
                   (define first (with-read-safedoc sd SafeDoc-analysis-completion))
                   (release-gate! (await probes))
                   (drive-controller controller first)
                   (source-controller-refresh-analysis! controller source)
                   (define held (await probes))
                   (define fresh (with-read-safedoc sd SafeDoc-analysis-completion))
                   (define snapshot (source-controller-snapshot controller source))
                   (define command (source-controller-analysis-evt controller snapshot))
                   (source-controller-handle! controller (await (source-controller-events controller)))
                   (define acquisition (await command))
                   (check-true (Analysis-Summary? (Analysis-Handle-reusable-summary acquisition)))
                   (check-eq? fresh (Analysis-Handle-completion-evt acquisition))
                   (check-false (sync/timeout 0 fresh))
                   (define response (visual-query sd 202 'inlay-hints (lambda (_) "available")))
                   (check-false (Visual-Delivery-degraded? response))
                   (check-equal? (hash-ref (Visual-Delivery-response response) 'result) "available")
                   (check-false (sync/timeout 0 fresh))
                   (release-gate! held)
                   (check-eq? 'succeeded (Analysis-Outcome-state (drive-controller controller fresh))))))

  (test-case "retained contributions use the reopened URI before fresh analysis completes"
    (with-temp-dir
      "retained-uri"
      (lambda (root)
        (define target-path (build-path root "target.rkt"))
        (define client-path (build-path root "client.rkt"))
        (define target-text "#lang racket/base\n(provide foo)\n(define (foo) 42)\n(foo)\n")
        (define client-text "#lang racket/base\n(require \"target.rkt\")\n(foo)\n")
        (define canonical (path->uri target-path))
        (define localhost (regexp-replace #rx"^file://" canonical "file://localhost"))
        (define folder (hasheq 'uri (path->uri root) 'name "retained-uri"))
        (define probes (make-async-channel))
        (define (check-restored-locations session uri)
          (request! session 220 "textDocument/references" client-path
                    (hasheq 'position (hasheq 'line 2 'character 2)
                            'context (hasheq 'includeDeclaration #t)))
          (define locations (hash-ref (response session 220) 'result))
          (define target-locations
            (filter (lambda (location) (equal? (uri->path (hash-ref location 'uri)) target-path))
                    locations))
          (check-equal? (length target-locations) 3)
          (for ([location (in-list target-locations)])
            (check-equal? (hash-ref location 'uri) uri)))
        (dynamic-wind
          (lambda ()
            (write-text target-path target-text)
            (workspace-add-folder! current-workspace root))
          (lambda ()
            (with-session (make-source-controller #:analyze (controlled-analyzer probes) #:resyntax #f)
                          (lambda (session)
                            (for ([path (in-list (list target-path client-path))]
                                  [source-text (in-list (list target-text client-text))])
                              (open! session path source-text)
                              (release-gate! (await probes))
                              (barrier session)
                              (check-eq? 'succeeded
                                         (Analysis-Outcome-state (await (acquire session path) 90))))
                            ;; Repeatedly close during refresh so each reopen transfers the same trace.
                            (for ([old-uri (in-list (list canonical localhost canonical localhost))]
                                  [new-uri (in-list (list localhost canonical localhost localhost))])
                              (close! session old-uri)
                              (open! session new-uri target-text 0)
                              (await probes)
                              (barrier session)
                              (check-false (sync/timeout 0 (acquire session target-path)))
                              (check-restored-locations session new-uri)
                              (notify session "workspace/didChangeWorkspaceFolders"
                                      (hasheq 'event (hasheq 'added '() 'removed (list folder))))
                              (notify session "workspace/didChangeWorkspaceFolders"
                                      (hasheq 'event (hasheq 'added (list folder) 'removed '())))
                              (barrier session)
                              (check-restored-locations session new-uri)))))
          (lambda ()
            (workspace-remove-folder! current-workspace root))))))

  (test-case "unchanged close/reopen transfers analysis through new lifetimes and original URIs"
    (define probes (make-async-channel))
    (with-session (make-source-controller #:analyze (controlled-analyzer probes) #:resyntax #f)
                  (lambda (session)
                    (define path (uri->path canonical-uri))
                    (open! session path text 7)
                    (release-gate! (await probes))
                    (barrier session)
                    (await (acquire session path))
                    (request! session 210 "textDocument/semanticTokens/full" path)
                    (define expected (hash-ref (response session 210) 'result))
                    (define old-view (view session path))
                    (close! session path)
                    (barrier session)
                    (open! session localhost-uri text 7)
                    (define held (await probes))
                    (barrier session)
                    (define fresh (acquire session path))
                    (check-false (sync/timeout 0 fresh))
                    (check-not-eq? old-view (view session path))
                    (request! session 211 "textDocument/semanticTokens/full" localhost-uri)
                    (check-equal? (hash-ref (response session 211) 'result) expected)
                    (check-equal? (hash-ref (hash-ref (last (notifications session)) 'params) 'uri)
                                  localhost-uri)
                    (release-gate! held)
                    (check-eq? 'succeeded (Analysis-Outcome-state (await fresh)))
                    (close! session localhost-uri)
                    (barrier session))))

  (test-case "different reopened text and unfinished edits never restore completed analysis"
    (define probes (make-async-channel))
    (with-session (make-source-controller #:analyze (controlled-analyzer probes) #:resyntax #f)
                  (lambda (session)
                    (define path (build-path scratch "retention-mismatch.rkt"))
                    (open! session path text 7)
                    (release-gate! (await probes))
                    (barrier session)
                    (await (acquire session path))
                    (close! session path)
                    (barrier session)
                    (define changed (string-append text "; changed\n"))
                    (open! session path changed 7)
                    (define held (await probes))
                    (barrier session)
                    (define acquisition
                      (await (source-controller-analysis-evt
                               (Session-controller session)
                               (Source-Result-snapshot (observation session path)))))
                    (check-false (Analysis-Handle-reusable-summary acquisition))
                    (release-gate! held)
                    (await (acquire session path))
                    (change! session path text 7)
                    (await probes)
                    (close! session path)
                    (barrier session)
                    (open! session path text 7)
                    (define cold (await probes))
                    (barrier session)
                    (request! session 212 "textDocument/semanticTokens/full" path)
                    (barrier session)
                    (check-false (sync/timeout 0 (Session-responses session)))
                    (release-gate! cold)
                    (check-not-false (hash-ref (response session 212) 'result #f))
                    (close! session path)
                    (barrier session))))

  (test-case "editing a retired Doc cannot alter the trace transferred into a reopened buffer"
    (define controller (make-source-controller #:resyntax #f))
    (with-buffer controller (build-path scratch "retired-doc.rkt") text
                 (lambda (old source)
                   (drive-controller controller (with-read-safedoc old SafeDoc-analysis-completion))
                   (source-controller-close! controller source old)
                   (define reopened (new-safedoc (with-read-doc old Doc-uri) text 7))
                   (source-controller-open! controller source reopened)
                   (check-false (with-read-safedoc old SafeDoc-available))
                   (check-false (with-read-safedoc old SafeDoc-source-view))
                   (check-false (with-read-safedoc old SafeDoc-analysis-completion))
                   (with-write-doc old (lambda (doc) (doc-reset! doc "")))
                   (check-true (with-read-doc reopened doc-trace-latest?))
                   (check-true (Available-Analysis? (with-read-safedoc reopened SafeDoc-available)))
                   (check-equal? (with-read-doc reopened doc-get-text) text)))))
