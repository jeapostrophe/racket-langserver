#lang racket/base

(require "../../common/interfaces.rkt"
         "../../doclib/internal-types.rkt"
         "../../workspace/state.rkt"
         racket/contract
         racket/list
         racket/path)

(provide merge-reference-sources
         reference-sources->locations)

(define (same-document-path? left right)
  (equal? (simple-form-path left)
          (simple-form-path right)))

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
        (filter (lambda (source)
                  (not (same-document-path? (Reference-Source-path source)
                                            (Reference-Source-path live-source))))
                workspace-sources)))

(define/contract (reference-sources->locations sources)
  (-> (listof Reference-Source?) (listof Location?))
  (append-map Reference-Source-locations sources))
