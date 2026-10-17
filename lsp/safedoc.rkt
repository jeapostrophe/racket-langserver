#lang racket/base

;; One open document's editor text and the fields that must stay consistent
;; with it under one lock.
;; Jobs, retention, and publication belong to source-controller.

(require "analysis.rkt"
         "../common/rwlock.rkt"
         "../workspace/source-store.rkt"
         "../doclib/doc.rkt"
         racket/contract)

;; source-view is updated in the same write-lock section as doc mutations.
;; analysis is a broadcast completion event, not the product.
;; available holds the completed product and analyzed snapshot even while analysis is pending.
;; Callers clear source-view, analysis, and available on close.
;; Access these fields only inside with-read-safedoc / with-write-safedoc
;; (or with-read-doc / with-write-doc for doc alone).
(struct/contract SafeDoc
  ([doc Doc?]
   [rwlock RWLock?]
   [source-view (or/c #f Source-View?)]
   [analysis (or/c #f evt?)]
   [available (or/c #f Available-Analysis?)])
  #:mutable
  #:transparent)

(define (new-safedoc uri text version)
  (SafeDoc (make-doc uri text version) (make-rwlock) #f #f #f))

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
         SafeDoc-available
         set-SafeDoc-available!
         SafeDoc-analysis
         set-SafeDoc-analysis!
         new-safedoc
         with-read-doc
         with-write-doc
         with-read-safedoc
         with-write-safedoc)
