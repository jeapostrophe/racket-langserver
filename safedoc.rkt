#lang racket/base

;; This module provides `SafeDoc`, a thread-safe document representation for the
;; LSP server. It wraps a `Doc` object and uses a read-write lock to allow for
;; safe concurrent access from multiple threads, making it suitable for being
;; managed by the language server.

(require "base/rwlock.rkt"
         "doc.rkt"
         "check-syntax.rkt"
         "scheduler.rkt"
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

;; the only place where really run check-syntax
(define (safedoc-run-check-syntax! safe-doc)
  (match-define (list uri old-version text)
                (with-read-doc safe-doc
                               (λ (doc)
                                 (list (Doc-uri doc) (Doc-version doc) (send (Doc-text doc) copy)))))

  (define (task)
    (define new-trace (check-syntax uri text))
    ;; make a new thread to write doc because this task will be executed by
    ;; the scheduler and can be killed at any time.
    (thread
      (λ ()
        (with-write-doc safe-doc
                        (λ (doc)
                          (when (and (equal? old-version (Doc-version doc))
                                     new-trace)
                            (set-Doc-trace-version! doc old-version)
                            (set-Doc-trace! doc new-trace))))
        (clear-old-queries/new-trace uri))))

  (scheduler-push-task! (with-read-doc safe-doc (λ (doc) (Doc-uri doc))) 'check-syntax task))

(provide (struct-out SafeDoc)
         new-safedoc
         safedoc-run-check-syntax!
         with-read-doc
         with-write-doc)

