#lang racket/base

;; Cross-file definition requests use the production serial consumer. Gates
;; mark actual reads and target analysis starts; no sleep establishes ordering.
(require "../../common/source.rkt"
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
         "../../doclib/doc.rkt"
         "../../doclib/internal-types.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/analysis-query.rkt"
         (prefix-in text-document/ "../../lsp/text-document.rkt")
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/source-controller.rkt"
         (only-in (submod "../../lsp/source-controller.rkt" owner)
                  source-controller-handle! source-controller-shutdown!
                  source-controller-snapshot source-controller-ensure-analysis!)
         "../../workspace/api.rkt"
         "../../workspace/current.rkt"
         "../../workspace/source-store.rkt"
         (submod "source-controller-test.rkt" test-support))

(struct Gate (input release) #:transparent)

(define saved "#lang racket/base\n(provide shared)\n(define shared 1)\n")
(define unsaved "#lang racket/base\n(provide shared)\n\n;; Unsaved\n(define shared 2)\n")
(define a-text "#lang racket/base\n(require \"b.rkt\")\n(define local 1)\n(list shared local)\n")

(define (write-text path text)
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

(define (release! gate [value 'go])
  (async-channel-put (Gate-release gate) value))

(define (controlled-analyzer path gates)
  (lambda (snapshot)
    (when (equal? (source-id (path->source path)) (Source-Snapshot-id snapshot))
      (define release (make-async-channel))
      (async-channel-put gates (Gate snapshot release))
      (case (async-channel-get release)
        [(raise) (raise 'target-failed)]
        [(die)
         ;; Self-killing parallel workers can delay completion on some runtimes.
         ;; Kill from a separate thread to exercise abrupt death.
         (define worker (current-thread))
         (thread (lambda () (kill-thread worker)))
         (sync never-evt)]))
    (analyze-source snapshot)))

(define (controlled-reader gates)
  (lambda (path)
    (define release (make-async-channel))
    (async-channel-put gates (Gate path release))
    (case (async-channel-get release)
      [(raise) (raise 'read-failed)]
      [(missing) (Disk-Missing)]
      [else (file->string path)])))

(define (request session id path [pos (Pos 3 7)])
  (async-channel-put (Session-clients session)
                     (hasheq 'id id 'method "textDocument/definition"
                             'params (hasheq 'textDocument (hasheq 'uri (path->uri path))
                                             'position (->jsexpr pos)))))

(define (edit! session path text)
  (notify session "textDocument/didChange"
          (hasheq 'textDocument (hasheq 'uri (path->uri path) 'version 1)
                  'contentChanges (list (hasheq 'text text)))))

;; Columns 8 through 14 span the shared identifier in the target fixtures.
(define (check-location session id b [line 2])
  (check-equal? (hash-ref (response session id) 'result)
                (->jsexpr (Location (path->uri b) (Range (Pos line 8) (Pos line 14))))))

(define (check-error session id code [message #f])
  (define error (hash-ref (response session id) 'error))
  (check-equal? (hash-ref error 'code) (->jsexpr code))
  (when message (check-equal? (hash-ref error 'message) message)))

(define (check-failure-log logs path detail)
  (define message (vector-ref (await logs) 1))
  (check-true (string-contains? message (path->string path)))
  (check-true (string-contains? message detail)))

(define (with-example make-controller proc [source a-text]
                      #:relative? [relative? #f] #:target-text [target-text saved]
                      #:request-timeout [request-timeout 90])
  (make-directory* scratch)
  (define root (make-temporary-file "definition-controller~a" 'directory scratch))
  (define a (build-path root "a.rkt"))
  (define b (build-path root "b.rkt"))
  ;; Relative import resolution in sequential expansions of identical text
  ;; across directories currently retains the first target (see plan evidence).
  ;; Give each fixture an explicit direct import; target freshness is the scope.
  (define direct-source
    (if relative?
        source
        (string-replace source "\"b.rkt\"" (format "(file ~s)" (path->string b)))))
  (dynamic-wind
    (lambda () (write-text a direct-source) (write-text b target-text))
    (lambda ()
      (with-session (make-controller b)
                    (lambda (session)
                      (open! session a direct-source)
                      (barrier session)
                      ;; Real expansion, including Typed Racket fixtures, can
                      ;; exceed the short timeout used for gate coordination.
                      (check-eq? (Analysis-Outcome-state (await (acquire session a) 90)) 'succeeded)
                      (proc session a b root)
                      (close! session a)
                      (barrier session))
                    #:request-timeout request-timeout))
    (lambda () (delete-directory/files root))))

;; This fixture owns its controller on the test thread. Tests that control
;; independent request budgets or buffer transitions use the handler directly; feature
;; integration tests above and below use the production serial consumer.
(define (with-local-definition proc #:analyze [analyzer analyze-source]
                               #:analysis-slots [slots 2])
  (make-directory* scratch)
  (define root (make-temporary-file "deferred-definition~a" 'directory scratch))
  (define a (build-path root "a.rkt"))
  (define b (build-path root "b.rkt"))
  (define reads (make-async-channel))
  (define controller
    (make-source-controller #:resyntax #f #:workspace (make-workspace)
                            #:analyze analyzer #:analysis-slots slots
                            #:read (lambda (path)
                                     (async-channel-put reads path)
                                     (file->string path))))
  (define (drive event)
    (let loop ()
      (define next
        (await (choice-evt (wrap-evt event (lambda (value) (cons 'result value)))
                           (wrap-evt (source-controller-events controller)
                                     (lambda (value) (cons 'event value))))
               90))
      (if (eq? (car next) 'result)
          (cdr next)
          (begin (source-controller-handle! controller (cdr next)) (loop)))))
  (define (accept-command!)
    (source-controller-handle! controller (await (source-controller-events controller))))
  (define (capture timeout)
    (parameterize ([current-analysis-request-timeout timeout])
      (text-document/definition
        1 (hasheq 'textDocument (hasheq 'uri (path->uri a))
                  'position (->jsexpr (Pos 3 7))))))
  (define (run deferred)
    (define responses (make-async-channel))
    (thread (lambda () (async-channel-put responses (deferred))))
    (drive responses))
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
      (source-controller-shutdown! controller)
      (delete-directory/files root))))

(define (start-deferred deferred)
  (define responses (make-async-channel))
  (thread (lambda () (async-channel-put responses (deferred))))
  responses)

(define (plain-controller _b) (make-source-controller #:resyntax #f))

(define (gated-analyzer gates)
  (lambda (b)
    (make-source-controller #:analyze (controlled-analyzer b gates) #:resyntax #f)))

(define (gated-reader reads)
  (lambda (_b)
    (make-source-controller #:read (controlled-reader reads) #:resyntax #f)))

(module+ test-support
  (provide with-example plain-controller gated-analyzer gated-reader
           release! edit! check-error check-location saved unsaved
           (struct-out Gate)))

(module+ test
  ;; Supply query inputs independently of upstream callback accuracy. Equal
  ;; identifiers in other submodules/phases/spaces must never be a fallback.
  (for ([binding-spec (in-list '((() 1)
                                 (() -1)
                                 (() #f)
                                 ((inner) 0)
                                 ((outer inner) (0 . custom))
                                 ((inner) (1 . custom))))])
    (match-define (list submods phase+space) binding-spec)
    (test-case (format "definition queries preserve submodules ~s and phase+space ~s"
                       submods phase+space)
      (define binding #f)
      (define expected-range (Range (Pos 2 8) (Pos 2 14)))
      (with-local-definition
        (lambda (a b _reads capture run _drive _accept-command!)
          (set! binding (Module-Binding b submods phase+space 'shared))
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
        #:analyze
        (lambda (snapshot)
          (define product (analyze-source snapshot))
          (if binding
              (struct-copy Analysis-Product product
                [summary
                 (Analysis-Summary
                   (hash binding expected-range
                         (Module-Binding (Module-Binding-filepath binding) '() 0 'shared)
                         (Range (Pos 0 0) (Pos 0 1)))
                   (hash))])
              product)))))

  (test-case "closed targets share/reuse outside folders and publish nothing"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b _root)
        (request session 1 a)
        (define gate (await gates))
        (request session 2 a)
        (barrier session)
        ;; A's local definition and the real consumer progress during held B.
        (request session 3 a (Pos 3 14))
        (check-equal? (hash-ref (response session 3) 'result)
                      (->jsexpr (Location (path->uri a) (Range (Pos 2 8) (Pos 2 13)))))
        (release! gate)
        (check-location session 1 b)
        (check-location session 2 b)
        (request session 4 a)
        (check-location session 4 b)
        (check-false (sync/timeout 0 gates))
        (check-false
          (workspace-definition-location current-workspace (Module-Binding b '() 0 'shared)))
        (let loop ()
          (define notification (sync/timeout 0 (Session-notifications session)))
          (when notification
            (check-not-equal? (hash-ref (hash-ref notification 'params) 'uri) (path->uri b))
            (loop))))))

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
        (release! gate)
        (check-location session 1 b)
        (check-false (sync/timeout 0 gates)))))

  (test-case "same-size save with restored mtime invalidates completed facts"
    (with-example plain-controller
                  (lambda (session a b _root)
                    (request session 1 a)
                    (check-location session 1 b)
                    (define before (view session b))
                    (define mtime (file-or-directory-modify-seconds b))
                    (define shifted "#lang racket/base\n(provide shared) (define shared 2)\n")
                    (check-equal? (string-length saved) (string-length shifted))
                    (write-text b shifted)
                    (file-or-directory-modify-seconds b mtime)
                    (request session 2 a)
                    (check-equal? (hash-ref (response session 2) 'result)
                                  (->jsexpr (Location (path->uri b) (Range (Pos 1 25) (Pos 1 31)))))
                    (check-not-eq? before (view session b)))))

  (test-case "save during analysis affects the next query, not the acquired snapshot"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b _root)
        (request session 1 a)
        (define old (await gates))
        (write-text b unsaved)
        (release! old)
        (check-location session 1 b)
        (check-false (sync/timeout 0 gates))
        (request session 2 a)
        (define next (await gates))
        (check-equal? (Source-Snapshot-text (Gate-input next)) unsaved)
        (release! next)
        (check-location session 2 b 4))))

  (test-case "new and cached queries each observe disk exactly once"
    (define reads (make-async-channel))
    (with-example
      (gated-reader reads)
      (lambda (session a b _root)
        (request session 1 a)
        (release! (await reads))
        (check-location session 1 b)
        (check-false (sync/timeout 0 reads))
        (write-text b unsaved)
        (request session 2 a)
        (release! (await reads))
        (check-location session 2 b 4)
        (check-false (sync/timeout 0 reads))
        (request session 3 a)
        (release! (await reads))
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
        (check-error session 1 ErrorCode-RequestFailed "Definition target changed")
        (release! next)
        (check-location session 2 b 4)
        (check-false (sync/timeout 0 gates)))))

  (for ([operation (in-list '(edit close))])
    (for ([stage (in-list '(read analysis))])
      (test-case (format "A ~a cancels during held target ~a" operation stage)
        (define gates (make-async-channel))
        (with-example
          (if (eq? stage 'read) (gated-reader gates) (gated-analyzer gates))
          (lambda (session a _b _root)
            (request session 1 a)
            (define held (await gates))
            (if (eq? operation 'edit) (edit! session a (file->string a)) (close! session a))
            (barrier session)
            (check-error session 1 ErrorCode-RequestCancelled)
            (release! held))))))

  (test-case "opening B ends obsolete analysis; later queries use buffer then disk"
    (define gates (make-async-channel))
    (define read-count 0)
    (with-example
      (lambda (b)
        (make-source-controller #:analyze (controlled-analyzer b gates) #:resyntax #f
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
        (check-error session 1 ErrorCode-RequestFailed "Definition target changed")
        (release! buffer-gate)
        (request session 2 a)
        (check-location session 2 b 4)
        (check-equal? read-count count-at-open)
        (close! session b)
        (barrier session)
        (check-equal? read-count count-at-open)
        (request session 3 a)
        (define disk-gate (await gates))
        (check-eq? (Source-Snapshot-origin (Gate-input disk-gate)) 'disk)
        (release! disk-gate)
        (check-location session 3 b))))

  (for ([operation (in-list '(close reopen))])
    (test-case (format "~a after buffer acquisition ends the query without reacquiring" operation)
      (define gates (make-async-channel))
      (define target #f)
      (with-local-definition
        (lambda (_a b reads capture run drive accept-command!)
          (set! target b)
          (lsp-open-doc! (path->uri b) unsaved 7)
          (await gates)
          (define obsolete (start-deferred (capture 90)))
          ;; This test owns the consumer. Accept observation and analysis
          ;; acquisition before closing; a client-notification barrier cannot
          ;; establish that a deferred request has acquired the buffer.
          (accept-command!)
          (accept-command!)
          (lsp-close-doc! (path->uri b))
          (define reopened
            (and (eq? operation 'reopen)
                 (begin
                   (lsp-open-doc! (path->uri b) (string-append "\n" unsaved) 7)
                   (await gates))))
          (define error (hash-ref (drive obsolete) 'error))
          (check-equal? (hash-ref error 'code) (->jsexpr ErrorCode-RequestFailed))
          (check-equal? (hash-ref error 'message) "Definition target changed")
          (check-false (sync/timeout 0 reads))
          (check-false (sync/timeout 0 gates))
          (when reopened
            (release! reopened)
            (check-equal? (hash-ref (run (capture 90)) 'result)
                          (->jsexpr (Location (path->uri b) (Range (Pos 5 8) (Pos 5 14)))))
            (lsp-close-doc! (path->uri b))))
        #:analyze (lambda (snapshot)
                    (if target
                        ((controlled-analyzer target gates) snapshot)
                        (analyze-source snapshot))))))

  (for ([failure (in-list '(raise die))])
    (test-case (format "runner ~a fails; later requests retry" failure)
      (define logs (make-log-receiver racket-langserver-logger 'error))
      (define gates (make-async-channel))
      (with-example
        (gated-analyzer gates)
        (lambda (session a b _root)
          (request session 1 a)
          (release! (await gates) failure)
          (check-error session 1 ErrorCode-RequestFailed "Definition analysis did not succeed")
          (check-failure-log logs b (if (eq? failure 'raise) "target-failed" "worker-exited"))
          (request session 2 a)
          (release! (await gates))
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
        (release! (await reads))
        (check-location session 1 b)
        (request session 2 a)
        (release! (await reads) 'raise)
        (check-error session 2 ErrorCode-RequestFailed "Definition source read failed")
        (check-failure-log logs b "read-failed")
        (request session 3 a)
        (release! (await reads))
        (check-location session 3 b))))

  (test-case "for-syntax binding queries target analysis regardless of upstream identity accuracy"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b _root)
        (request session 1 a (Pos 3 12))
        (release! (await gates))
        ;; Upstream versions may disagree on the jump and target phases.
        ;; A matching identity resolves; a mismatch remains absent.
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
        (check-error session 1 ErrorCode-RequestFailed)
        (release! held))
      #:request-timeout 0.1))

  (test-case "expired before thunk invocation starts no target work"
    (with-local-definition
      (lambda (_a _b reads capture _run _drive _accept-command!)
        (define deferred (capture 0))
        ;; Invocation has a generous parameter; the captured budget still wins.
        (define result (parameterize ([current-analysis-request-timeout 90]) (deferred)))
        (check-equal? (hash-ref (hash-ref result 'error) 'code)
                      (->jsexpr ErrorCode-RequestFailed))
        (check-false (sync/timeout 0 reads)))))

  (test-case "cancellation wins over ready deadline and cache"
    (with-local-definition
      (lambda (a b _reads capture run _drive _accept-command!)
        (check-equal? (hash-ref (run (capture 90)) 'result)
                      (->jsexpr (Location (path->uri b) (Range (Pos 2 8) (Pos 2 14)))))
        (define deferred (capture 0))
        (lsp-close-doc! (path->uri a))
        (check-equal? (hash-ref (hash-ref (deferred) 'error) 'code)
                      (->jsexpr ErrorCode-RequestCancelled)))))

  (test-case "one waiter expiring leaves the shared job available to its peer"
    (define gates (make-async-channel))
    (define target #f)
    (with-local-definition
      (lambda (_a b _reads capture _run drive _accept-command!)
        (set! target b)
        (define short (start-deferred (capture 0.2)))
        (define held (drive gates))
        (define peer (start-deferred (capture 90)))
        (check-equal? (hash-ref (hash-ref (drive short) 'error) 'code)
                      (->jsexpr ErrorCode-RequestFailed))
        (release! held)
        (check-equal? (hash-ref (drive peer) 'result)
                      (->jsexpr (Location (path->uri b) (Range (Pos 2 8) (Pos 2 14)))))
        (check-false (sync/timeout 0 gates)))
      #:analyze (lambda (snapshot)
                  (if (and target (equal? (Source-Snapshot-id snapshot)
                                          (source-id (path->source target))))
                      ((controlled-analyzer target gates) snapshot)
                      (analyze-source snapshot)))))

  (test-case "shutdown settles a deferred definition without waiting for its deadline"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a _b _root)
        (request session 1 a)
        (await gates)
        (shutdown! session)
        (check-error session 1 ErrorCode-RequestFailed))))

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
        (edit! session a (file->string a))
        (barrier session)
        (check-error session 1 ErrorCode-RequestCancelled)
        (release! held)
        (check-location session 2 b)
        (check-false (sync/timeout 0 gates))
        (close! session c)
        (barrier session))))

  (test-case "deadline covers queueing without cancelling shared work"
    (define gates (make-async-channel))
    (define held-paths '())
    (with-local-definition
      (lambda (a b _reads capture _run drive _accept-command!)
        (define c (build-path (path-only a) "c.rkt"))
        (set! held-paths (list b c))
        (lsp-open-doc! (path->uri c) "#lang racket/base\n(define other 1)\n" 0)
        (define blocking (drive gates))
        (define short (start-deferred (capture 0.2)))
        (define peer (start-deferred (capture 90)))
        (check-equal? (hash-ref (hash-ref (drive short) 'error) 'code)
                      (->jsexpr ErrorCode-RequestFailed))
        (check-false (sync/timeout 0 gates))
        (release! blocking)
        (define target (drive gates))
        (check-equal? (Source-Snapshot-id (Gate-input target)) (source-id (path->source b)))
        (release! target)
        (check-equal? (hash-ref (drive peer) 'result)
                      (->jsexpr (Location (path->uri b) (Range (Pos 2 8) (Pos 2 14)))))
        (check-false (sync/timeout 0 gates))
        (lsp-close-doc! (path->uri c)))
      #:analysis-slots 1
      #:analyze (lambda (snapshot)
                  (define path
                    (for/or ([path (in-list held-paths)])
                      (and (equal? (Source-Snapshot-id snapshot) (source-id (path->source path)))
                           path)))
                  (if path
                      ((controlled-analyzer path gates) snapshot)
                      (analyze-source snapshot)))))

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
        (check-error session 1 ErrorCode-RequestFailed "Definition target changed")
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
        (check-error session 1 ErrorCode-RequestFailed "Definition target changed")
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
                  #:relative? #t)))

(module+ test
  (test-case "closed target definitions use retained facts while one refresh is held"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b _root)
        (open! session b saved 7)
        (release! (await gates))
        (barrier session)
        (await (acquire session b))
        (close! session b)
        (barrier session)
        (request session 301 a)
        (define refresh (await gates))
        (check-location session 301 b)
        (request session 302 a)
        (check-location session 302 b)
        (check-false (sync/timeout 0 gates) "requests share the pending refresh")
        (define fresh (acquire session b))
        (check-false (sync/timeout 0 fresh))
        (open! session b saved 7)
        (define reopened (await gates))
        (barrier session)
        (check-eq? 'stopped (Analysis-Outcome-state (await fresh)))
        (request session 303 a)
        (check-location session 303 b)
        (check-false (sync/timeout 0 gates))
        (release! refresh) ; invalidated completion cannot replace the new lifetime
        (release! reopened)
        (check-eq? 'succeeded (Analysis-Outcome-state (await (acquire session b))))
        (close! session b)
        (barrier session)))))
