#lang racket/base

(require "../../common/source.rkt"
         "../../common/interfaces.rkt"
         "../../doclib/internal-types.rkt"
         "../../workspace/state.rkt"
         racket/contract)

(provide reference-locations)

;; Locations for one references request.
;;
;; Order: the live document's locations, then the declaration when
;; `include-declaration?` is true and that file is a different source, then
;; locations from the other workspace sources.
;;
;; Keep each source's location order and duplicates. This function does not
;; sort sources against each other. Leave the live source out of the
;; workspace lists: its locations come from the request, and the index may
;; still hold older text for that document. The live source matches by exact
;; `Source-Id` equality.
(define/contract (reference-locations workspace document-result include-declaration?)
  (-> Workspace? Document-Reference-Result? boolean? (listof Location?))
  (define live-source (Document-Reference-Result-source document-result))
  (define module-binding (Document-Reference-Result-module-binding document-result))
  (define live-id (Reference-Source-id live-source))
  (define workspace-sources
    (if module-binding
        (workspace-reference-sources workspace module-binding)
        '()))
  (define definition
    (and include-declaration?
         module-binding
         (not (equal? (source-id (path->source (Module-Binding-filepath module-binding)))
                      live-id))
         (workspace-definition-location workspace module-binding)))
  (append (Reference-Source-locations live-source)
          (if definition (list definition) '())
          (for*/list ([source (in-list workspace-sources)]
                      #:unless (equal? (Reference-Source-id source) live-id)
                      [location (in-list (Reference-Source-locations source))])
            location)))
