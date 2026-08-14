#lang racket/base

(require "../../common/interfaces.rkt"
         "../../doclib/internal-types.rkt"
         "../../workspace/state.rkt"
         racket/contract
         racket/list
         racket/path)

(provide merge-reference-sources
         reference-sources->locations)

;; Merge the request document's live source with other workspace sources.
(define/contract (merge-reference-sources workspace document-result include-declaration?)
  (-> Workspace? Document-Reference-Result? boolean? (listof Reference-Source?))
  (define live-source (Document-Reference-Result-source document-result))
  (define module-binding (Document-Reference-Result-module-binding document-result))
  ;; Last successful workspace contributions for this binding (may include
  ;; a stale same-path snapshot, filtered out below).
  (define workspace-sources
    (if module-binding
        (workspace-reference-sources workspace module-binding)
        '()))
  (define live-path (Reference-Source-path live-source))
  (define sources-by-path
    (for/hash ([source (in-list workspace-sources)]
               #:unless (equal? (Reference-Source-path source) live-path))
      (values (Reference-Source-path source) source)))
  (define definition
    (and module-binding
         include-declaration?
         (not (equal? (simple-form-path (Module-Binding-filepath module-binding)) live-path))
         (workspace-definition-location workspace module-binding)))
  (define merged-sources-by-path
    (if definition
        (hash-update
          sources-by-path
          (Module-Binding-filepath module-binding)
          (lambda (source)
            (Reference-Source
              (Reference-Source-path source)
              (cons definition (Reference-Source-locations source))))
          (lambda ()
            (Reference-Source (Module-Binding-filepath module-binding) '())))
        sources-by-path))
  (cons live-source (hash-values merged-sources-by-path)))

(define/contract (reference-sources->locations sources)
  (-> (listof Reference-Source?) (listof Location?))
  (append-map Reference-Source-locations sources))
