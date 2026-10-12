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
  ;; Virtual references are entirely local, even for collection-backed names.
  (define module-binding
    (and (path? (Reference-Source-source-id live-source))
         (Document-Reference-Result-module-binding document-result)))
  ;; Last successful workspace contributions for this binding (may include
  ;; a stale same-path snapshot, filtered out below).
  (define workspace-sources
    (if module-binding
        (workspace-reference-sources workspace module-binding)
        '()))
  (define live-id (Reference-Source-source-id live-source))
  (define sources-by-id
    (for/hash ([source (in-list workspace-sources)]
               #:unless (equal? (Reference-Source-source-id source) live-id))
      (values (Reference-Source-source-id source) source)))
  (define definition
    (and module-binding
         include-declaration?
         (not (equal? (simple-form-path (Module-Binding-filepath module-binding)) live-id))
         (workspace-definition-location workspace module-binding)))
  (define merged-sources-by-id
    (if definition
        (hash-update
          sources-by-id
          (Module-Binding-filepath module-binding)
          (lambda (source)
            (Reference-Source
              (Reference-Source-source-id source)
              (cons definition (Reference-Source-locations source))))
          (lambda ()
            (Reference-Source (Module-Binding-filepath module-binding) '())))
        sources-by-id))
  (cons live-source (hash-values merged-sources-by-id)))

(define/contract (reference-sources->locations sources)
  (-> (listof Reference-Source?) (listof Location?))
  (append-map Reference-Source-locations sources))
