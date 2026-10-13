#lang racket/base

;; Manual characterization for Step 1 of background-analysis-plan.md.
;; Run with `racket tests/experiments/background-analysis-baseline.rkt`.
;; These observations describe the old path, not the prototype's requirements;
;; keep this out of module+ test so the suite does not require repeated work or
;; blocked edits after the prototype changes those behaviors.

(require json
         racket/match
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../common/path-util.rkt"
         "../../doclib/doc.rkt"
         "../../doclib/internal-types.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         "../../workspace/api.rkt"
         "../../workspace/current.rkt"
         racket/async-channel
         racket/file
         racket/path
         racket/runtime-path
         racket/string
         rackunit
         rackunit/text-ui)

(define-runtime-path project-root "../..")

(define a-text
  (string-append "#lang racket/base\n"
                 "(require \"b.rkt\")\n"
                 "(define local-value 1)\n"
                 "(list shared-value local-value)\n"))

(define (write-text path text)
  (call-with-output-file path #:exists 'truncate
    (lambda (out) (display text out))))

;; Count macro invocations during source expansion, rather than instantiation
;; of B's compile-time module. File rendezvous also works in fresh namespaces.
(define (b-text root [saved? #f])
  (string-append
    "#lang racket/base\n(provide shared-value)\n"
    (if saved? "\n;; Saved revision\n" "")
    ";; Target-only documentation\n"
    (if saved? "(define shared-value 20)\n" "(define shared-value 10)\n")
    "(require (for-syntax racket/base))\n"
    "(define-syntax (record-expansion stx)\n"
    (format "  (call-with-output-file ~s #:exists 'append\n"
            (path->string (build-path root "expansions")))
    "    (lambda (out) (displayln 'expanded out)))\n"
    (format "  (when (file-exists? ~s)\n" (path->string (build-path root "hold")))
    (format "    (call-with-output-file ~s #:exists 'truncate void)\n"
            (path->string (build-path root "entered")))
    "    (let wait ()\n"
    (format "      (unless (file-exists? ~s) (sleep 0.01) (wait))))\n"
            (path->string (build-path root "release")))
    "  #'(void))\n(record-expansion)\n"))

(define (expansion-count root)
  (define path (build-path root "expansions"))
  (if (file-exists? path) (length (file->lines path)) 0))

(define (start proc)
  (define result (make-async-channel))
  (thread
    (lambda ()
      (with-handlers ([exn? (lambda (exn) (async-channel-put result exn))])
        (async-channel-put result (proc)))))
  result)

(define (await event)
  (define result (sync/timeout 20 event))
  (cond
    [(exn? result) (raise result)]
    [result result]
    [else (error 'background-analysis-baseline "timed out waiting for ~v" event)]))

(define (params uri pos)
  (hasheq 'textDocument (hasheq 'uri uri) 'position (->jsexpr pos)))

;; Preserve the characterized pre-Step-4 handler after production migrates.
;; Its imported lookup deliberately runs inside A's read lock.
(define (baseline-definition id params)
  (define uri (hash-ref (hash-ref params 'textDocument) 'uri))
  (match-define (as-Pos pos) (hash-ref params 'position))
  (define location
    (with-read-doc (lsp-get-doc uri)
      (lambda (doc)
        (match (doc-definition doc uri pos)
          [(struct* Module-Binding ([filepath path] [submods submods] [id name]))
           (define range (doc-get-definition-by-id path submods name))
           (and range (Location (path->uri path) range))]
          [local local]))))
  (hasheq 'jsonrpc "2.0" 'id id 'result (if location (->jsexpr location) (json-null))))

(define (observe root)
  (define a-path (build-path root "a.rkt"))
  (define b-path (build-path root "b.rkt"))
  (define a-uri (path->uri a-path))
  (define b-uri (path->uri b-path))
  (define import-params (params a-uri (Pos 3 7)))
  (define local-params (params a-uri (Pos 3 20)))
  (write-text a-path a-text)
  (write-text b-path (b-text root))
  (workspace-add-folder! current-workspace root)
  (define safe-a (lsp-open-doc! a-uri a-text 0))
  (dynamic-wind
    void
    (lambda ()
      (with-write-doc safe-a
        (lambda (doc)
          (check-true (doc-expand! doc))
          (workspace-set-contribution! current-workspace (Doc-contribution doc))))
      (define binding
        (with-read-doc safe-a (lambda (doc) (doc-module-binding-at doc (Pos 3 7)))))
      (check-equal? binding (Module-Binding b-path '() 0 'shared-value))
      (check-false (lsp-get-doc b-uri #f))
      (check-false (workspace-definition-location current-workspace binding))
      (printf "A binding: ~v\nA analysis expanded B: ~a\n" binding (expansion-count root))

      (define expected-location
        (->jsexpr (Location b-uri (Range (Pos 3 8) (Pos 3 20)))))
      (for ([request-id (in-range 1 3)])
        (define before (expansion-count root))
        (check-equal? (hash-ref (baseline-definition request-id import-params) 'result)
                      expected-location)
        (check-equal? (- (expansion-count root) before) 1))
      (check-false (workspace-definition-location current-workspace binding))
      (displayln "Two repeated definitions: 2 target expansions; no B contribution")

      (define before-hover (expansion-count root))
      ;; Preserve the historical document-local hover after Step 5 adds
      ;; imported detail through deferred LSP composition.
      (define hover-result
        (->jsexpr (with-read-doc safe-a (lambda (doc) (doc-hover doc (Pos 3 7))))))
      (define contents (hash-ref hover-result 'contents))
      (check-true (string-contains? contents "imported from"))
      (check-false (string-contains? contents "**Source**"))
      (check-false (string-contains? contents "Target-only documentation"))
      (check-equal? (expansion-count root) before-hover)
      (printf "Imported hover (0 target expansions): ~s\n" hover-result)

      ;; Explicitly analyze and publish closed B to isolate index eligibility
      ;; from loading it as a dependency of A or for a definition request.
      (define target-doc (make-doc b-uri (b-text root)))
      (check-true (doc-expand! target-doc))
      (workspace-set-contribution! current-workspace (Doc-contribution target-doc))
      (check-equal? (->jsexpr (workspace-definition-location current-workspace binding))
                    expected-location)
      (define before-indexed (expansion-count root))
      (check-equal? (hash-ref (baseline-definition 4 import-params) 'result)
                    expected-location)
      (check-equal? (- (expansion-count root) before-indexed) 1)
      (displayln "Publishing B supplies its indexed definition; lookup still expands B")

      (write-text b-path (b-text root #t))
      (define saved-location
        (->jsexpr (Location b-uri (Range (Pos 5 8) (Pos 5 20)))))
      (check-equal? (hash-ref (baseline-definition 5 import-params) 'result)
                    saved-location)
      (check-equal? (->jsexpr (workspace-definition-location current-workspace binding))
                    expected-location)
      (displayln "Saved B: lookup moves from line 3 to 5; indexed contribution stays old")

      (write-text (build-path root "hold") "")
      (define before-concurrent (expansion-count root))
      (define pending-definition
        (start (lambda () (baseline-definition 6 import-params))))
      (await
        (start
          (lambda ()
            (let wait ()
              (unless (file-exists? (build-path root "entered")) (sleep 0.01) (wait)))
            #t)))
      (define edit-started (make-semaphore 0))
      (define pending-edit
        (start
          (lambda ()
            (semaphore-post edit-started)
            ;; Isolate did-change!'s editor mutation and write lock. Scheduling
            ;; Check Syntax here would expand dependencies and change the count.
            (with-write-doc safe-a
              (lambda (doc)
                (doc-apply-edit! doc (Range (Pos 2 20) (Pos 2 21)) "2")
                (doc-update-version! doc 1)))
            #t)))
      (await edit-started)
      (check-false (sync/timeout 0.1 pending-edit))
      (define local-result
        (await (start (lambda () (baseline-definition 7 local-params)))))
      (check-equal? (hash-ref local-result 'result)
                    (->jsexpr (Location a-uri (Range (Pos 2 8) (Pos 2 19)))))
      (check-false (file-exists? (build-path root "release")))
      (displayln "Held B: concurrent local definition completes; A edit remains pending")

      (define second-definition
        (start (lambda () (baseline-definition 8 import-params))))
      (await
        (start
          (lambda ()
            (let wait ()
              (unless (= 2 (- (expansion-count root) before-concurrent))
                (sleep 0.01)
                (wait)))
            #t)))
      (write-text (build-path root "release") "")
      (check-equal? (hash-ref (await pending-definition) 'result) saved-location)
      (check-equal? (hash-ref (await second-definition) 'result) saved-location)
      (await pending-edit)
      (check-equal? (- (expansion-count root) before-concurrent) 2)
      (check-equal? (with-read-doc safe-a doc-get-text)
                    (string-replace a-text "local-value 1" "local-value 2"))
      (displayln "Release: both definitions and A edit complete; concurrent requests expand twice"))
    (lambda ()
      ;; Release before closing, including assertion/exception paths.
      (write-text (build-path root "release") "")
      (lsp-close-doc! a-uri)
      (workspace-remove-folder! current-workspace root))))

(define baseline
  (test-suite
    "background analysis baseline (current implementation)"
    (test-case
      "direct saved import, reuse, index, save, and lock ordering"
      (define scratch (build-path (simple-form-path project-root) ".agent-scratch"))
      (make-directory* scratch)
      (define root (make-temporary-file "background-analysis~a" 'directory scratch))
      (define custodian (make-custodian))
      (dynamic-wind
        void
        (lambda () (parameterize ([current-custodian custodian]) (observe root)))
        (lambda ()
          (custodian-shutdown-all custodian)
          (delete-directory/files root))))))

(module+ main
  (exit (run-tests baseline)))
