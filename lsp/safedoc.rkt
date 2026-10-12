#lang racket/base

;; This module provides `SafeDoc`, a thread-safe document representation for the
;; LSP server. It wraps a `Doc` object and uses a read-write lock to allow for
;; safe concurrent access from multiple threads, making it suitable for being
;; managed by the language server.

(require "../common/rwlock.rkt"
         "../workspace/source-store.rkt"
         "../doclib/doc.rkt"
         racket/contract)

;; The consumer updates source-view under the same lock as editor text.
;; analysis is a broadcast completion event; it contains no mutable trace.
;; Both projections are cleared on close. Jobs and terminal state belong to
;; source-controller, not to SafeDoc.
;; Access document and projection fields only inside a lock-acquiring eliminator.
(struct/contract SafeDoc
  ([doc Doc?]
   [rwlock RWLock?]
   [source-view (or/c #f Source-View?)]
   [analysis (or/c #f evt?)])
  #:mutable
  #:transparent)

(define (new-safedoc uri text version)
  (SafeDoc (make-doc uri text version) (make-rwlock) #f #f))

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

(provide SafeDoc?
         SafeDoc-doc
         SafeDoc-source-view
         set-SafeDoc-source-view!
         SafeDoc-analysis
         set-SafeDoc-analysis!
         new-safedoc
         with-read-doc
         with-write-doc
         with-read-safedoc
         with-write-safedoc)
