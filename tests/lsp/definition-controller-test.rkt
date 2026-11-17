#lang racket/base

;; Cross-file definition requests use the production serial consumer. Gates
;; mark actual reads and target analysis starts; no sleep establishes ordering.
(module+ test
  (require "controller-support.rkt"
           drracket/check-syntax
           json
           rackunit
           racket/async-channel
           racket/class
           racket/file
           racket/match
           racket/path
           racket/string
           "../../common/debug.rkt"
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../../common/path-util.rkt"
           "../../common/source.rkt"
           "../../doclib/doc.rkt"
           "../../doclib/internal-types.rkt"
           "../../lsp/analysis.rkt"
           "../../lsp/compose/definition.rkt"
           (prefix-in text-document/ "../../lsp/text-document.rkt")
           "../../lsp/lsp.rkt"
           "../../lsp/safedoc.rkt"
           "../../lsp/source-controller.rkt"
           (only-in (submod "../../lsp/source-controller.rkt" owner)
                    source-controller-handle!
                    source-controller-shutdown!
                    source-controller-snapshot
                    source-controller-ensure-analysis!)
           "../../workspace/api.rkt"
           "../../workspace/current.rkt"
           "../../workspace/source-store.rkt")

  (define (request session id path [pos (Pos 3 7)])
    (request! session id "textDocument/definition" path (hasheq 'position (->jsexpr pos))))

  (define (check-failure-log logs path detail)
    (define message (vector-ref (await logs) 1))
    (check-true (string-contains? message (path->string path)))
    (check-true (string-contains? message detail)))

  ;; This fixture owns its controller on the test thread for direct handler tests.
  (define (with-local-definition proc #:make-analyzer [make-analyzer (lambda (_a _b) analyze-source)]
                                 #:analysis-slots [slots 2])
    (with-temp-dir
      "deferred-definition"
      (lambda (root)
        (define a (build-path root "a.rkt"))
        (define b (build-path root "b.rkt"))
        (define reads (make-async-channel))
        (define controller
          (make-source-controller #:resyntax #f
                                  #:workspace (make-workspace)
                                  #:analyze (make-analyzer a b)
                                  #:analysis-slots slots
                                  #:read (lambda (path)
                                           (async-channel-put reads path)
                                           (file->string path))))
        (define (drive event)
          (drive-controller controller event 90))
        (define (accept-command!)
          (source-controller-handle! controller (await (source-controller-events controller))))
        (define (capture timeout)
          (parameterize ([current-definition-target-timeout timeout])
            (text-document/definition
              1 (hasheq 'textDocument (hasheq 'uri (path->uri a))
                        'position (->jsexpr (Pos 3 7))))))
        (define (run deferred)
          (drive (start-deferred deferred)))
        (dynamic-wind
          (lambda () (write-text b saved))
          (lambda ()
            (parameterize ([current-source-controller controller]
                           [current-open-docs (make-hasheq)])
              (lsp-open-doc! (path->uri a)
                             (string-replace a-text "\"b.rkt\"" (format "(file ~s)" (path->string b)))
                             0)
              (check-eq? (Analysis-Outcome-state
                           (drive (source-controller-ensure-analysis!
                                    controller (source-controller-snapshot controller (path->source a)))))
                         'succeeded)
              (proc a b reads capture run drive accept-command!)))
          (lambda ()
            (source-controller-shutdown! controller))))))

  (for ([binding-spec (in-list '((() 1)
                                 (() -1)
                                 (() #f)
                                 ((inner) 0)
                                 ((outer inner) (0 . custom))
                                 ((inner) (1 . custom))))])
    (match-define (list submods phase+space) binding-spec)
    (test-case (format "definition queries preserve submodules ~s and phase+space ~s"
                       submods phase+space)
      (define expected-range (Range (Pos 2 8) (Pos 2 14)))
      (with-local-definition
        (lambda (a b _reads capture run _drive _accept-command!)
          (define (supply-jump! target-submods)
            (with-write-safedoc (lsp-get-doc (path->uri a))
              (lambda (sd)
                (define start (doc-pos->abs-pos (SafeDoc-doc sd) (Pos 3 6)))
                (define trace
                  (Analysis-Product-trace (Available-Analysis-product (SafeDoc-available sd))))
                (send trace syncheck:add-jump-to-definition/phase-level+space
                      a start (+ start 6) 'shared b target-submods phase+space))))
          (supply-jump! submods)
          (check-equal? (hash-ref (run (capture 90)) 'result)
                        (->jsexpr (Location (path->uri b) expected-range)))
          (supply-jump! '(missing))
          (check-equal? (hash-ref (run (capture 90)) 'result) (json-null)))
        #:make-analyzer
        (lambda (_a b)
          (define binding (Module-Binding b submods phase+space 'shared))
          (define target-id (source-id (path->source b)))
          (lambda (snapshot)
            (define product (analyze-source snapshot))
            (if (equal? (Source-Snapshot-id snapshot) target-id)
                (struct-copy Analysis-Product product
                  [summary
                   (Analysis-Summary
                     (hash binding expected-range
                           (Module-Binding b '() 0 'shared)
                           (Range (Pos 0 0) (Pos 0 1)))
                     (hash))])
                product))))))

  (test-case "closed targets share/reuse outside folders and publish nothing"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b _root)
        (request session 1 a)
        (define gate (await gates))
        (request session 2 a)
        (barrier session)
        (request session 3 a (Pos 3 14))
        (check-equal? (hash-ref (response session 3) 'result)
                      (->jsexpr (Location (path->uri a) (Range (Pos 2 8) (Pos 2 13)))))
        (release-gate! gate)
        (check-location session 1 b)
        (check-location session 2 b)
        (request session 4 a)
        (check-location session 4 b)
        (check-false (sync/timeout 0 gates))
        (check-false
          (workspace-definition-location current-workspace (Module-Binding b '() 0 'shared)))
        (for ([notification (in-list (notifications session))])
          (check-not-equal? (hash-ref (hash-ref notification 'params) 'uri) (path->uri b))))))

  (test-case "queries need no folder; folder removal preserves held queries"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b root)
        (check-false (workspace-contains? current-workspace b))
        (request session 1 a)
        (define gate (await gates))
        (define before (view session b))
        (workspace-add-folder! current-workspace root)
        (notify session "workspace/didChangeWorkspaceFolders"
                (hasheq 'event (hasheq 'added '() 'removed
                                       (list (hasheq 'uri (path->uri root) 'name "example")))))
        (barrier session)
        (check-eq? before (view session b))
        (release-gate! gate)
        (check-location session 1 b)
        (check-false (sync/timeout 0 gates)))))

  (test-case "save during analysis affects the next query, not the acquired snapshot"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b _root)
        (request session 1 a)
        (define old (await gates))
        (write-text b unsaved)
        (release-gate! old)
        (check-location session 1 b)
        (check-false (sync/timeout 0 gates))
        (request session 2 a)
        (define next (await gates))
        (check-equal? (Source-Snapshot-text (Gate-input next)) unsaved)
        (release-gate! next)
        (check-location session 2 b 4))))

  (test-case "new and cached queries each observe disk exactly once"
    (define reads (make-async-channel))
    (with-example
      (gated-reader reads)
      (lambda (session a b _root)
        (request session 1 a)
        (release-gate! (await reads))
        (check-location session 1 b)
        (check-false (sync/timeout 0 reads))
        (write-text b unsaved)
        (request session 2 a)
        (release-gate! (await reads))
        (check-location session 2 b 4)
        (check-false (sync/timeout 0 reads))
        (request session 3 a)
        (release-gate! (await reads))
        (check-location session 3 b 4)
        (check-false (sync/timeout 0 reads)))))

  (test-case "peer discovery ends an obsolete query without restarting it"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b _root)
        (request session 1 a)
        (await gates)
        (write-text b unsaved)
        (request session 2 a)
        (define next (await gates))
        (check-error (response session 1) ErrorCode-RequestFailed "Definition target changed")
        (release-gate! next)
        (check-location session 2 b 4)
        (check-false (sync/timeout 0 gates)))))

  (for* ([operation (in-list '(edit close))]
         [stage (in-list '(read analysis))])
    (test-case (format "A ~a cancels during held target ~a" operation stage)
      (define gates (make-async-channel))
      (with-example
        (if (eq? stage 'read) (gated-reader gates) (gated-analyzer gates))
        (lambda (session a _b _root)
          (request session 1 a)
          (define held (await gates))
          (if (eq? operation 'edit)
              (change! session a (file->string a))
              (close! session a))
          (barrier session)
          (check-error (response session 1) ErrorCode-ContentModified)
          (release-gate! held)))))

  (test-case "opening B ends obsolete analysis; later queries use buffer then disk"
    (define gates (make-async-channel))
    (define read-count 0)
    (with-example
      (lambda (b)
        (make-source-controller #:analyze (controlled-analyzer gates b) #:resyntax #f
                                #:read (lambda (path)
                                         (set! read-count (add1 read-count))
                                         (file->string path))))
      (lambda (session a b _root)
        (request session 1 a)
        (await gates)
        (open! session b unsaved 7)
        (define buffer-gate (await gates))
        (barrier session)
        (check-eq? (Source-Snapshot-origin (Gate-input buffer-gate)) 'buffer)
        (define count-at-open read-count)
        (check-error (response session 1) ErrorCode-RequestFailed "Definition target changed")
        (release-gate! buffer-gate)
        (request session 2 a)
        (check-location session 2 b 4)
        (check-equal? read-count count-at-open)
        (close! session b)
        (barrier session)
        (check-equal? read-count count-at-open)
        (request session 3 a)
        (define disk-gate (await gates))
        (check-eq? (Source-Snapshot-origin (Gate-input disk-gate)) 'disk)
        (release-gate! disk-gate)
        (check-location session 3 b))))

  (for ([operation (in-list '(close reopen))])
    (test-case (format "~a after buffer acquisition ends the query without reacquiring" operation)
      (define gates (make-async-channel))
      (with-local-definition
        (lambda (_a b reads capture run drive accept-command!)
          (lsp-open-doc! (path->uri b) unsaved 7)
          (await gates)
          (define obsolete (start-deferred (capture 90)))
          (accept-command!)
          (accept-command!)
          (lsp-close-doc! (path->uri b))
          (define reopened
            (and (eq? operation 'reopen)
                 (begin
                   (lsp-open-doc! (path->uri b) (string-append "\n" unsaved) 7)
                   (await gates))))
          (check-error (drive obsolete) ErrorCode-RequestFailed "Definition target changed")
          (check-false (sync/timeout 0 reads))
          (check-false (sync/timeout 0 gates))
          (when reopened
            (release-gate! reopened)
            (check-equal? (hash-ref (run (capture 90)) 'result)
                          (->jsexpr (Location (path->uri b) (Range (Pos 5 8) (Pos 5 14)))))
            (lsp-close-doc! (path->uri b))))
        #:make-analyzer (lambda (_a b) (controlled-analyzer gates b)))))

  (for ([failure (in-list '(raise die))])
    (test-case (format "runner ~a fails; later requests retry" failure)
      (define logs (make-log-receiver racket-langserver-logger 'error))
      (define gates (make-async-channel))
      (with-example
        (gated-analyzer gates)
        (lambda (session a b _root)
          (request session 1 a)
          (release-gate! (await gates) failure)
          (check-error (response session 1) ErrorCode-RequestFailed
                       "Definition analysis did not succeed")
          (check-failure-log logs b (if (eq? failure 'raise) "target-failed" "worker-exited"))
          (request session 2 a)
          (release-gate! (await gates))
          (check-location session 2 b)))))

  (test-case "unsuccessful/missing definitions return null; later requests retry"
    (with-example plain-controller
                  (lambda (session a b _root)
                    (define logs (make-log-receiver racket-langserver-logger 'info))
                    (write-text b "#lang racket/base\n(")
                    (request session 1 a)
                    (check-equal? (hash-ref (response session 1) 'result) (json-null))
                    (check-failure-log logs b "Definition analysis reported errors")
                    (check-false (sync/timeout 0 logs))
                    (write-text b "#lang racket/base\n(define other 1)\n")
                    (request session 2 a)
                    (check-equal? (hash-ref (response session 2) 'result) (json-null))
                    (delete-file b)
                    (request session 3 a)
                    (check-equal? (hash-ref (response session 3) 'result) (json-null))
                    (write-text b saved)
                    (request session 4 a)
                    (check-location session 4 b))))

  (test-case "read failure rejects cached facts and later refresh remains possible"
    (define logs (make-log-receiver racket-langserver-logger 'error))
    (define reads (make-async-channel))
    (with-example
      (gated-reader reads)
      (lambda (session a b _root)
        (request session 1 a)
        (release-gate! (await reads))
        (check-location session 1 b)
        (request session 2 a)
        (release-gate! (await reads) 'raise)
        (check-error (response session 2) ErrorCode-RequestFailed "Definition source read failed")
        (check-failure-log logs b "read-failed")
        (request session 3 a)
        (release-gate! (await reads))
        (check-location session 3 b))))

  (test-case "for-syntax binding queries target analysis regardless of upstream identity accuracy"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b _root)
        (request session 1 a (Pos 3 12))
        (release-gate! (await gates))
        (define result (hash-ref (response session 1) 'result))
        (check-true
          (or (equal? result (json-null))
              (equal? result
                      (->jsexpr (Location (path->uri b) (Range (Pos 2 8) (Pos 2 14)))))))
        (check-false (sync/timeout 0 gates)))
      (string-append
        "#lang racket/base\n"
        "(require (for-syntax racket/base \"b.rkt\"))\n"
        "(begin-for-syntax\n  (define y shared))\n")))

  (test-case "request deadline covers held reads"
    (define reads (make-async-channel))
    (with-example
      (gated-reader reads)
      (lambda (session a _b _root)
        (request session 1 a)
        (define held (await reads))
        (check-error (response session 1) ErrorCode-RequestFailed)
        (release-gate! held))
      #:request-timeout 0.1))

  (test-case "expired before thunk invocation starts no target work"
    (with-local-definition
      (lambda (_a _b reads capture _run _drive _accept-command!)
        (define deferred (capture 0))
        (define result (parameterize ([current-definition-target-timeout 90]) (deferred)))
        (check-error result ErrorCode-RequestFailed)
        (check-false (sync/timeout 0 reads)))))

  (test-case "cancellation wins over ready deadline and cache"
    (with-local-definition
      (lambda (a b _reads capture run _drive _accept-command!)
        (check-equal? (hash-ref (run (capture 90)) 'result)
                      (->jsexpr (Location (path->uri b) (Range (Pos 2 8) (Pos 2 14)))))
        (define deferred (capture 0))
        (lsp-close-doc! (path->uri a))
        (check-error (deferred) ErrorCode-ContentModified))))

  (test-case "one waiter expiring leaves the shared job available to its peer"
    (define gates (make-async-channel))
    (with-local-definition
      (lambda (_a b _reads capture _run drive _accept-command!)
        (define short (start-deferred (capture 0.2)))
        (define held (drive gates))
        (define peer (start-deferred (capture 90)))
        (check-error (drive short) ErrorCode-RequestFailed)
        (release-gate! held)
        (check-equal? (hash-ref (drive peer) 'result)
                      (->jsexpr (Location (path->uri b) (Range (Pos 2 8) (Pos 2 14)))))
        (check-false (sync/timeout 0 gates)))
      #:make-analyzer (lambda (_a b) (controlled-analyzer gates b))))

  (test-case "shutdown settles a deferred definition without waiting for its deadline"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a _b _root)
        (request session 1 a)
        (await gates)
        (shutdown! session)
        (check-error (response session 1) ErrorCode-RequestFailed))))

  (test-case "opening B abandons a held disk read"
    (define reads (make-async-channel))
    (with-example
      (gated-reader reads)
      (lambda (session a b _root)
        (request session 1 a)
        (await reads)
        (open! session b unsaved)
        (check-location session 1 b 4)
        (check-false (sync/timeout 0 reads))
        (close! session b)
        (barrier session))))

  (test-case "cancelling one A leaves another A's shared target request alive"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b root)
        (define c (build-path root "c.rkt"))
        (open! session c (file->string a))
        (barrier session)
        (check-eq? (Analysis-Outcome-state (await (acquire session c))) 'succeeded)
        (request session 1 a)
        (define held (await gates))
        (request session 2 c)
        (change! session a (file->string a))
        (barrier session)
        (check-error (response session 1) ErrorCode-ContentModified)
        (release-gate! held)
        (check-location session 2 b)
        (check-false (sync/timeout 0 gates))
        (close! session c)
        (barrier session))))

  (test-case "deadline covers queueing without cancelling shared work"
    (define gates (make-async-channel))
    (with-local-definition
      (lambda (a b _reads capture _run drive _accept-command!)
        (define c (build-path (path-only a) "c.rkt"))
        (lsp-open-doc! (path->uri c) "#lang racket/base\n(define other 1)\n" 0)
        (define blocking (drive gates))
        (define short (start-deferred (capture 0.2)))
        (define peer (start-deferred (capture 90)))
        (check-error (drive short) ErrorCode-RequestFailed)
        (check-false (sync/timeout 0 gates))
        (release-gate! blocking)
        (define target (drive gates))
        (check-equal? (Source-Snapshot-id (Gate-input target)) (source-id (path->source b)))
        (release-gate! target)
        (check-equal? (hash-ref (drive peer) 'result)
                      (->jsexpr (Location (path->uri b) (Range (Pos 2 8) (Pos 2 14)))))
        (check-false (sync/timeout 0 gates))
        (lsp-close-doc! (path->uri c)))
      #:analysis-slots 1
      #:make-analyzer
      (lambda (a b)
        (define held-paths (list b (build-path (path-only a) "c.rkt")))
        (lambda (snapshot)
          (define path
            (for/or ([path (in-list held-paths)])
              (and (equal? (Source-Snapshot-id snapshot) (source-id (path->source path)))
                   path)))
          (if path
              ((controlled-analyzer gates path) snapshot)
              (analyze-source snapshot))))))

  (test-case "rename invalidation wakes held queries and missing old paths return null"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b root)
        (request session 1 a)
        (await gates)
        (define renamed (build-path root "renamed.rkt"))
        (rename-file-or-directory b renamed)
        (notify session "workspace/didRenameFiles"
                (hasheq 'files (list (hasheq 'oldUri (path->uri b) 'newUri (path->uri renamed)))))
        (check-error (response session 1) ErrorCode-RequestFailed "Definition target changed")
        (request session 2 a)
        (check-equal? (hash-ref (response session 2) 'result) (json-null)))))

  (test-case "rename during acquisition fails without starting another read"
    (define reads (make-async-channel))
    (with-example
      (gated-reader reads)
      (lambda (session a b root)
        (request session 1 a)
        (await reads)
        (define renamed (build-path root "renamed.rkt"))
        (rename-file-or-directory b renamed)
        (notify session "workspace/didRenameFiles"
                (hasheq 'files (list (hasheq 'oldUri (path->uri b) 'newUri (path->uri renamed)))))
        (check-error (response session 1) ErrorCode-RequestFailed "Definition target changed")
        (check-false (sync/timeout 0 reads)))))

  (test-case "ordinary relative imports use shared disk and unsaved buffer analysis"
    (with-example plain-controller
                  (lambda (session a b _root)
                    (request session 1 a)
                    (check-location session 1 b)
                    (open! session b unsaved)
                    (request session 2 a)
                    (check-location session 2 b 4)
                    (close! session b)
                    (barrier session))
                  #:relative? #t))

  (test-case "dot URI alias joins imported target to unsaved buffer and retains its address"
    (with-local-definition
      (lambda (_a b reads capture run _drive _accept-command!)
        (define alias-uri
          (string-replace (path->uri b) "/b.rkt" "/missing/.././%62.rkt"))
        (lsp-open-doc! alias-uri unsaved 7)
        (check-equal? (hash-ref (run (capture 90)) 'result)
                      (->jsexpr (Location alias-uri (Range (Pos 4 8) (Pos 4 14)))))
        (check-false (sync/timeout 0 reads) "open alias must prevent a disk read")
        (lsp-close-doc! alias-uri)
        (check-equal? (hash-ref (run (capture 90)) 'result)
                      (->jsexpr (Location alias-uri (Range (Pos 2 8) (Pos 2 14)))))
        (check-equal? (await reads) b)))))
