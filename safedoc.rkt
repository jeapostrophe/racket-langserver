#lang racket/base

;; This module provides `SafeDoc`, a thread-safe document representation for the
;; LSP server. It wraps a `Doc` object and uses a read-write lock to allow for
;; safe concurrent access from multiple threads, making it suitable for being
;; managed by the language server.

(require "base/rwlock.rkt"
         "doc.rkt"
         "check-syntax.rkt"
         "scheduler.rkt"
         "json-util.rkt"
         racket/set
         racket/match
         racket/class)

;; SafeDoc has two eliminators:
;; with-read-doc: access Doc within a reader lock.
;; with-write-doc: access Doc within a writer lock.
;; Access its fields without protection should not be allowed.
(struct SafeDoc
  (doc rwlock token)
  #:transparent)

(define (new-safedoc uri text version)
  (define doc (make-doc uri text version))
  ;; Token identifies this opened document instance in scheduler/query state.
  (define token (gensym 'doc-token))
  (scheduler-register-doc! token)
  (SafeDoc doc (make-rwlock) token))

(define (with-read-doc safe-doc proc)
  (call-with-read-lock
    (SafeDoc-rwlock safe-doc)
    (λ () (proc (SafeDoc-doc safe-doc)))))

(define (with-write-doc safe-doc proc)
  (call-with-write-lock
    (SafeDoc-rwlock safe-doc)
    (λ () (proc (SafeDoc-doc safe-doc)))))

;; TODO: add uri to each Diagnostic struct when make them, and remove uri here
;; Currently it uses the `uri` of the document that triggers
;; the check-syntax. But some diagnostics may come from other files.
;; In this case, it sends them with a wrong uri.
(define (send-diagnostics notify-client uri diag-lst)
  (notify-client "textDocument/publishDiagnostics"
                 (hasheq 'uri uri
                         'diagnostics (->jsexpr (set->list diag-lst)))))

;; The only place that actually runs check-syntax.
(define (safedoc-run-check-syntax! notify-client safe-doc)
  (match-define (list uri old-version doc-text token)
    (with-read-doc safe-doc
      (λ (doc)
        (list (Doc-uri doc)
              (Doc-version doc)
              (doc-copy-text-buffer doc)
              (SafeDoc-token safe-doc)))))

  (define (check-syntax-task)
    (define result (doc-expand uri doc-text))
    (with-write-doc safe-doc
      (λ (doc)
        (define cur-version (Doc-version doc))
        (define trace (CSResult-trace result))
        (define diags (set->list (send trace get-warn-diags)))
        (send-diagnostics notify-client uri diags)

        (when (and (CSResult-succeed? result)
                   (equal? old-version cur-version))
          (doc-update-trace! doc trace cur-version)

          (define (walk-text-task)
            (doc-walk-text trace (CSResult-text result))
            (define new-diags (set->list (send trace get-warn-diags)))
            (when (not (set=? diags new-diags))
              (send-diagnostics notify-client uri new-diags)))

          (scheduler-push-task! token 'walk-text walk-text-task))))
    (clear-old-queries/new-trace token))

  (scheduler-stop-all-tasks! token)
  (scheduler-push-task! token 'check-syntax check-syntax-task))

(provide SafeDoc-token
         SafeDoc?
         new-safedoc
         safedoc-run-check-syntax!
         with-read-doc
         with-write-doc)

