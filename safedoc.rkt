#lang racket/base

;; This module provides `SafeDoc`, a thread-safe document representation for the
;; LSP server. It wraps a `Doc` object and uses a read-write lock to allow for
;; safe concurrent access from multiple threads, making it suitable for being
;; managed by the language server.

(require "base/rwlock.rkt"
         "doc.rkt"
         "check-syntax.rkt"
         "scheduler.rkt"
         "msg-io.rkt"
         "responses.rkt"
         racket/set
         racket/match
         racket/class)

;; SafeDoc has two eliminator:
;; with-read-doc: access Doc within a reader lock.
;; with-write-doc: access Doc within a writer lock.
;; Access its fields without protection should not be allowed.
(struct SafeDoc
  (doc rwlock)
  #:transparent)

(define (new-safedoc uri text version)
  (define doc (new-doc uri text version))
  (SafeDoc doc (make-rwlock)))

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
;; In this case, it send them with wrong uri.
(define (send-diagnostics uri diag-lst)
  (display-message/flush (diagnostics-message uri (set->list diag-lst))))

;; the only place where really run check-syntax
(define (safedoc-run-check-syntax! safe-doc)
  (match-define (list uri old-version doc-text)
                (with-read-doc safe-doc
                  (λ (doc)
                    (list (Doc-uri doc) (Doc-version doc) (send (Doc-text doc) copy)))))

  (define (check-syntax-task)
    ;; TODO: cache the namespace with some strategy
    (define ns (make-base-namespace))
    (define result (doc-expand uri doc-text ns))
    ;; make a new thread to write doc because this task will be executed by
    ;; the scheduler and can be executed synchronously if needed (e.g. from tests)
    ;; but updating the doc requires a write lock which might block.
    (thread
      (λ ()
        (with-write-doc safe-doc
          (λ (doc)
            (define cur-version (Doc-version doc))
            (define trace (CSResult-trace result))
            (define diags (set->list (send trace get-warn-diags)))
            (send-diagnostics uri diags)

            (when (and (CSResult-succeed? result) (equal? old-version cur-version))
              (doc-update-trace! doc trace cur-version)

              (define (walk-text-task)
                (doc-walk-text trace (CSResult-text result))
                (define new-diags (set->list (send trace get-warn-diags)))
                (when (not (set=? diags new-diags))
                  (send-diagnostics uri new-diags)))

              (scheduler-push-task! uri 'walk-text walk-text-task))))
        (clear-old-queries/new-trace uri))))

  (scheduler-push-task! uri 'check-syntax check-syntax-task))

(provide (struct-out SafeDoc)
         new-safedoc
         safedoc-run-check-syntax!
         with-read-doc
         with-write-doc)

