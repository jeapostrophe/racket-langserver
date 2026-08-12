#lang racket/base

(require "../common/interfaces.rkt"
         "../doclib/internal-types.rkt"
         racket/contract)

(provide Contribution-Store?
         make-contribution-store
         contribution-store-source-paths
         contribution-store-add!
         contribution-store-remove-source!
         contribution-store-reference-sources)

;; Authoritative contributions plus a derived index for reference-sources lookup.
;; module-binding->path->locations maps Module-Binding -> citing-path -> locations in that path.
;; No lock; Workspace serializes every operation.
(struct/contract Contribution-Store
  ([path->contribution (hash/c path? Doc-Contribution? #:immutable #f)]
   [module-binding->path->locations
    (hash/c Module-Binding?
            (hash/c path? (listof Location?) #:immutable #f)
            #:immutable #f)]))

;; Time: O(1).
(define/contract (make-contribution-store)
  (-> Contribution-Store?)
  (Contribution-Store (make-hash) (make-hash)))

;; Time: O(m), m = number of stored contributions.
(define/contract (contribution-store-source-paths store)
  (-> Contribution-Store? (listof path?))
  (hash-keys (Contribution-Store-path->contribution store)))

;; Unhook one citing source-path from Module-Binding in the derived index.
;; Drop the Module-Binding entry when no citing sources remain.
;; Time: expected O(1).
(define (remove-source-from-module-binding! store module-binding source-path)
  (define module-binding->path->locations (Contribution-Store-module-binding->path->locations store))
  (define path->locations (hash-ref module-binding->path->locations module-binding #f))
  (when path->locations
    (hash-remove! path->locations source-path)
    (when (hash-empty? path->locations)
      (hash-remove! module-binding->path->locations module-binding))))

;; Drop this path as a citer: remove its Doc-Contribution and unhook only its
;; locations from the index. Other documents that cite bindings defined here
;; stay unchanged.
;; Time: expected O(n), n = |Doc-Contribution-references| of the source.
(define/contract (contribution-store-remove-source! store source-path)
  (-> Contribution-Store? path? void?)
  (define path->contribution (Contribution-Store-path->contribution store))
  (define contribution (hash-ref path->contribution source-path #f))
  (when contribution
    (hash-remove! path->contribution source-path)
    (for ([module-binding (in-hash-keys (Doc-Contribution-references contribution))])
      (remove-source-from-module-binding! store module-binding source-path))))

;; Time: expected O(n_old + n_new), each n = |references| of the old/new
;; contribution at the same path.
(define/contract (contribution-store-add! store contribution)
  (-> Contribution-Store? Doc-Contribution? void?)
  (define source-path (Doc-Contribution-path contribution))
  (contribution-store-remove-source! store source-path)
  (define module-binding->path->locations (Contribution-Store-module-binding->path->locations store))
  (hash-set! (Contribution-Store-path->contribution store) source-path contribution)
  (for ([(module-binding locations)
         (in-hash (Doc-Contribution-references contribution))])
    (define path->locations
      (hash-ref! module-binding->path->locations module-binding make-hash))
    (hash-set! path->locations source-path locations)))

;; Time: expected O(s), s = citing sources for the module-binding.
(define/contract (contribution-store-reference-sources store module-binding)
  (-> Contribution-Store? Module-Binding? (listof Reference-Source?))
  (define path->locations
    (hash-ref (Contribution-Store-module-binding->path->locations store) module-binding (hash)))
  (for/list ([(path locations) (in-hash path->locations)])
    (Reference-Source path locations)))
