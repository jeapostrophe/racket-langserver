#lang racket/base

;; This module provides `SafeDoc`, a thread-safe document representation for the
;; LSP server. It wraps a `Doc` object and uses a read-write lock to allow for
;; safe concurrent access from multiple threads, making it suitable for being
;; managed by the language server.

(require "../common/rwlock.rkt"
         "../common/path-util.rkt"
         "../doclib/doc.rkt"
         "../doclib/check-syntax.rkt"
         "../doclib/lexer.rkt"
         "../doclib/external/resyntax.rkt"
         "../workspace/current.rkt"
         "../workspace/state.rkt"
         "resyntax-place.rkt"
         "scheduler.rkt"
         net/url
         racket/set
         "../common/json-util.rkt"
         "../common/settings.rkt"
         racket/class
         racket/contract)

;; Tracks a check-syntax run for a specific document version.
;; state: 'running, 'succeeded, or 'failed
;; version: document version the run was started on
(struct/contract Check-Syntax-Status
  ([state (or/c 'running 'succeeded 'failed)]
   [version exact-nonnegative-integer?])
  #:transparent)

;; SafeDoc eliminators:
;;   with-read-safedoc / with-write-safedoc — get safe-doc, access any field
;;   with-read-doc / with-write-doc — get doc only (legacy)
;; Exported field accessors: SafeDoc-doc, SafeDoc-check-syntax-status.
;; Access fields only inside an eliminator that acquired the lock.
(struct SafeDoc
  (doc rwlock token check-syntax-status closed? disk-changed? contribution-matches-disk?)
  #:mutable
  #:transparent)

(define (new-safedoc uri text version)
  (define doc (make-doc uri text version))
  ;; Token identifies this opened document instance in scheduler/query state.
  (define token (gensym 'doc-token))
  (scheduler-register-doc! token)
  (SafeDoc doc (make-rwlock) token #f #f #f #f))

(define (with-read-doc safe-doc proc)
  (call-with-read-lock
    (SafeDoc-rwlock safe-doc)
    (λ () (proc (SafeDoc-doc safe-doc)))))

(define (with-write-doc safe-doc proc)
  (call-with-write-lock
    (SafeDoc-rwlock safe-doc)
    (λ () (proc (SafeDoc-doc safe-doc)))))

(define (with-read-safedoc safe-doc proc)
  (call-with-read-lock
    (SafeDoc-rwlock safe-doc)
    (λ () (proc safe-doc))))

(define (with-write-safedoc safe-doc proc)
  (call-with-write-lock
    (SafeDoc-rwlock safe-doc)
    (λ () (proc safe-doc))))

;; Retirement and publication use the same lock, so work from a closed
;; document cannot restore diagnostics or workspace contributions.
(define (safedoc-close! safe-doc)
  (with-write-safedoc safe-doc
    (lambda (sd)
      (set-SafeDoc-closed?! sd #t)
      (or (SafeDoc-disk-changed? sd)
          (not (SafeDoc-contribution-matches-disk? sd))))))

(define (safedoc-disk-changed! safe-doc path)
  (with-write-safedoc safe-doc
    (lambda (sd)
      (and (equal? path (uri->path (Doc-uri (SafeDoc-doc sd))))
           (begin
             (set-SafeDoc-disk-changed?! sd #t)
             #t)))))

(define (with-current-safedoc safe-doc version proc)
  (with-write-safedoc safe-doc
    (lambda (sd)
      (and (not (SafeDoc-closed? sd))
           (equal? version (Doc-version (SafeDoc-doc sd)))
           (begin (proc sd) #t)))))

(define (safedoc-check-syntax-running? sd)
  (define doc (SafeDoc-doc sd))
  (define status (SafeDoc-check-syntax-status sd))
  (and (Check-Syntax-Status? status)
       (eq? 'running (Check-Syntax-Status-state status))
       (equal? (Check-Syntax-Status-version status) (Doc-version doc))))

;; TODO: add uri to each Diagnostic struct when make them, and remove uri here
;; Currently it uses the `uri` of the document that triggers
;; the check-syntax. But some diagnostics may come from other files.
;; In this case, it sends them with a wrong uri.
(define (send-diagnostics notify-client uri diag-lst)
  (notify-client "textDocument/publishDiagnostics"
                 (hasheq 'uri uri
                         'diagnostics (->jsexpr diag-lst))))

(define (send-doc-diagnostics notify-client doc)
  (send-diagnostics notify-client
                    (Doc-uri doc)
                    (doc-diagnostics doc)))

;; Compare decoded text, consuming at most its length plus one character.
;; The extra character distinguishes an exact match from a longer disk file.
(define (port-matches-text? in text)
  (and (equal? text (read-string (string-length text) in))
       (eof-object? (read-char in))))

(define (file-matches-text? uri text)
  (and (equal? (url-scheme (string->url uri)) "file")
       (with-handlers ([exn:fail:filesystem? (lambda (_e) #f)])
         (call-with-input-file* (uri->path uri)
           (lambda (in) (port-matches-text? in text))))))

;; The only place that actually runs check-syntax.
(define (safedoc-run-check-syntax! notify-client safe-doc)
  (define-values (uri working-version text-buffer-copy token)
    (with-read-safedoc safe-doc
      (lambda (sd)
        (define doc (SafeDoc-doc sd))
        (values (Doc-uri doc)
                (Doc-version doc)
                (doc-copy-text-buffer doc)
                (SafeDoc-token sd)))))

  (with-current-safedoc safe-doc working-version
    (lambda (sd)
      (set-SafeDoc-check-syntax-status!
        sd
        (Check-Syntax-Status 'running working-version))))

  (define (resyntax-task)
    (define text (send text-buffer-copy get-text))
    (run-resyntax/in-place text uri))

  (define (publish-resyntax resyntax-results)
    (with-current-safedoc safe-doc working-version
      (lambda (sd)
        (define doc (SafeDoc-doc sd))
        (doc-update-resyntax-result! doc resyntax-results)
        (send-doc-diagnostics notify-client doc))))

  (define (check-syntax-task)
    (define text (send text-buffer-copy get-text))
    (define lexer-state (build-lexer-state text uri))
    (define result (doc-expand uri text-buffer-copy lexer-state))
    (define trace (CSResult-trace result))
    ;; Contribution derivation reads the candidate trace and frozen text. Keep
    ;; it outside the SafeDoc write lock so installation remains a short swap.
    (define contribution
      (and (CSResult-succeed? result)
           (send trace get-contribution)))
    (values result contribution (set->list (send trace get-warn-diags))))

  (define (publish-check-syntax result contribution diags)
    (define trace (CSResult-trace result))
    (define (publish-provenance matches-disk?)
      (when matches-disk?
        (with-current-safedoc safe-doc working-version
          (lambda (sd)
            (when (eq? contribution (Doc-contribution (SafeDoc-doc sd)))
              (set-SafeDoc-contribution-matches-disk?! sd #t))))))
    (when (with-current-safedoc safe-doc working-version
            (lambda (sd)
              (define doc (SafeDoc-doc sd))
              (send-diagnostics notify-client uri diags)

              (when (CSResult-succeed? result)
                (doc-update-trace! doc trace contribution working-version)
                ;; Provenance belongs to this accepted contribution. Optional
                ;; disk I/O must never withhold completed analysis or queries.
                (set-SafeDoc-contribution-matches-disk?! sd #f)
                (workspace-set-contribution! current-workspace contribution)
                (unless (SafeDoc-disk-changed? sd)
                  (scheduler-push-task! token 'disk-provenance
                                        (lambda () (file-matches-text? uri (CSResult-text result)))
                                        #:publish publish-provenance))
                (when (and (get-resyntax-enabled) (resyntax-available?))
                  (scheduler-push-task! token 'resyntax resyntax-task
                                        #:publish publish-resyntax)))
              (set-SafeDoc-check-syntax-status!
                sd
                (Check-Syntax-Status
                  (if (CSResult-succeed? result) 'succeeded 'failed)
                  working-version))))
      (clear-old-queries/check-syntax-finished token)))

  (scheduler-stop-all-tasks! token)
  (scheduler-push-task! token 'check-syntax check-syntax-task
                        #:publish publish-check-syntax))

(provide SafeDoc-token
         SafeDoc?
         SafeDoc-doc
         SafeDoc-check-syntax-status
         (struct-out Check-Syntax-Status)
         new-safedoc
         safedoc-close!
         safedoc-disk-changed!
         safedoc-run-check-syntax!
         safedoc-check-syntax-running?
         with-read-doc
         with-write-doc
         with-read-safedoc
         with-write-safedoc)

(module+ test-support
  (provide with-current-safedoc
           SafeDoc-contribution-matches-disk?
           port-matches-text?
           file-matches-text?))
