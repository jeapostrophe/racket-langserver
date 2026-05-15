#lang racket/base

;; This module provides `SafeDoc`, a thread-safe document representation for the
;; LSP server. It wraps a `Doc` object and uses a read-write lock to allow for
;; safe concurrent access from multiple threads, making it suitable for being
;; managed by the language server.

(require "../common/rwlock.rkt"
         "../doclib/doc.rkt"
         "../doclib/check-syntax.rkt"
         "../doclib/external/resyntax.rkt"
         "resyntax-place.rkt"
         "scheduler.rkt"
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
  (doc rwlock token check-syntax-status)
  #:mutable
  #:transparent)

(define (new-safedoc uri text version)
  (define doc (make-doc uri text version))
  ;; Token identifies this opened document instance in scheduler/query state.
  (define token (gensym 'doc-token))
  (scheduler-register-doc! token)
  (SafeDoc doc (make-rwlock) token #f))

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

  (with-write-safedoc safe-doc
    (lambda (sd)
      (define doc (SafeDoc-doc sd))
      (when (equal? working-version (Doc-version doc))
        (set-SafeDoc-check-syntax-status!
          sd
          (Check-Syntax-Status 'running working-version)))))

  (define (resyntax-task)
    (define text (send text-buffer-copy get-text))
    (define resyntax-results (run-resyntax/in-place text uri))
    (with-write-doc safe-doc
      (lambda (doc)
        (when (equal? working-version (Doc-version doc))
          (doc-update-resyntax-result! doc resyntax-results)
          (send-doc-diagnostics notify-client doc)))))

  (define (check-syntax-task)
    (define result (doc-expand uri text-buffer-copy))

    (with-write-safedoc safe-doc
      (lambda (sd)
        (define doc (SafeDoc-doc sd))
        (define cur-version (Doc-version doc))
        (define trace (CSResult-trace result))
        (define diags (set->list (send trace get-warn-diags)))
        (send-diagnostics notify-client uri diags)

        (when (and (CSResult-succeed? result)
                   (equal? working-version cur-version))
          (doc-update-trace! doc trace cur-version)
          (when (and (get-resyntax-enabled) (resyntax-available?))
            (scheduler-push-task! token 'resyntax resyntax-task)))
        (when (equal? working-version (Doc-version doc))
          (set-SafeDoc-check-syntax-status!
            sd
            (Check-Syntax-Status
              (if (CSResult-succeed? result)
                  'succeeded
                  'failed)
              working-version)))))
    (clear-old-queries/check-syntax-finished token))

  (scheduler-stop-all-tasks! token)
  (scheduler-push-task! token 'check-syntax check-syntax-task))

(provide SafeDoc-token
         SafeDoc?
         SafeDoc-doc
         SafeDoc-check-syntax-status
         (struct-out Check-Syntax-Status)
         new-safedoc
         safedoc-run-check-syntax!
         safedoc-check-syntax-running?
         with-read-doc
         with-write-doc
         with-read-safedoc
         with-write-safedoc)

