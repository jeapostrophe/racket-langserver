#lang racket/base

(require "../../common/interfaces.rkt"
         "../../doclib/internal-types.rkt"
         "../../workspace/state.rkt"
         racket/contract
         racket/list
         racket/path
         racket/set)

(provide merge-reference-sources
         reference-sources->locations)

(define (same-document-path? left right)
  (equal? (simple-form-path left)
          (simple-form-path right)))

(define (reference-source<? left right)
  (path<? (simple-form-path (Reference-Source-path left))
          (simple-form-path (Reference-Source-path right))))

(define (pos<? left right)
  (or (< (Pos-line left) (Pos-line right))
      (and (= (Pos-line left) (Pos-line right))
           (< (Pos-char left) (Pos-char right)))))

(define (range<? left right)
  (define left-start (Range-start left))
  (define right-start (Range-start right))
  (or (pos<? left-start right-start)
      (and (equal? left-start right-start)
           (pos<? (Range-end left) (Range-end right)))))

(define (location<? left right)
  (define left-uri (Location-uri left))
  (define right-uri (Location-uri right))
  (or (string<? left-uri right-uri)
      (and (string=? left-uri right-uri)
           (range<? (Location-range left) (Location-range right)))))

(define (deduplicate-locations locations)
  (define seen (mutable-set))
  (for/list ([location (in-list locations)]
             #:unless (set-member? seen location))
    (set-add! seen location)
    location))

;; Merge the request document's live source with other workspace sources.
(define/contract (merge-reference-sources workspace document-result)
  (-> Workspace? Document-Reference-Result? (listof Reference-Source?))
  (define live-source (Document-Reference-Result-source document-result))
  (define module-binding (Document-Reference-Result-module-binding document-result))
  ;; Last successful workspace contributions for this binding (may include
  ;; a stale same-path snapshot, filtered out below).
  (define workspace-sources
    (if module-binding
        (workspace-reference-sources workspace module-binding)
        '()))
  (cons live-source
        (sort
          (filter (lambda (source)
                    (not (same-document-path? (Reference-Source-path source)
                                              (Reference-Source-path live-source))))
                  workspace-sources)
          reference-source<?)))

;; The protocol boundary needs stable, duplicate-free locations even when
;; several source snapshots report the same URI and range.
(define/contract (reference-sources->locations sources)
  (-> (listof Reference-Source?) (listof Location?))
  (sort
    (deduplicate-locations
      (append-map Reference-Source-locations sources))
    location<?))
