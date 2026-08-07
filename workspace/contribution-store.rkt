#lang racket/base

(require "../common/interfaces.rkt"
         "../doclib/internal-types.rkt"
         racket/contract)

(provide Contribution-Store?
         make-contribution-store
         contribution-store-source-paths
         contribution-store-add!
         contribution-store-remove-source!
         contribution-store-find-references)

;; Authoritative contributions plus a derived index for fast find-references.
;; key->path->locations maps Binding-Key -> citing-path -> locations in that path.
;; No lock; Workspace serializes every operation.
(struct/contract Contribution-Store
  ([path->contribution (hash/c path-string? Doc-Contribution? #:immutable #f)]
   [key->path->locations
    (hash/c Binding-Key?
            (hash/c path-string? (listof Location?) #:immutable #f)
            #:immutable #f)]))

;; Time: O(1).
(define/contract (make-contribution-store)
  (-> Contribution-Store?)
  (Contribution-Store (make-hash) (make-hash)))

;; Time: O(m), m = number of stored contributions.
(define/contract (contribution-store-source-paths store)
  (-> Contribution-Store? (listof path-string?))
  (hash-keys (Contribution-Store-path->contribution store)))

;; Unhook one citing source-path from Binding-Key in the derived index.
;; Drop the Binding-Key entry when no citing sources remain.
;; Time: expected O(1).
(define (remove-source-from-key! store binding-key source-path)
  (define key->path->locations (Contribution-Store-key->path->locations store))
  (define path->locations (hash-ref key->path->locations binding-key #f))
  (when path->locations
    (hash-remove! path->locations source-path)
    (when (hash-empty? path->locations)
      (hash-remove! key->path->locations binding-key))))

;; Drop this path as a citer: remove its Doc-Contribution and unhook only its
;; locations from the index. Other documents that cite bindings defined here
;; stay unchanged.
;; Time: expected O(n), n = |Doc-Contribution-references| of the source.
(define/contract (contribution-store-remove-source! store source-path)
  (-> Contribution-Store? path-string? void?)
  (define path->contribution (Contribution-Store-path->contribution store))
  (define contribution (hash-ref path->contribution source-path #f))
  (when contribution
    (hash-remove! path->contribution source-path)
    (for ([binding-key (in-hash-keys (Doc-Contribution-references contribution))])
      (remove-source-from-key! store binding-key source-path))))

;; Time: expected O(n_old + n_new), each n = |references| of the old/new
;; contribution at the same path.
(define/contract (contribution-store-add! store contribution)
  (-> Contribution-Store? Doc-Contribution? void?)
  (define source-path (Doc-Contribution-path contribution))
  (contribution-store-remove-source! store source-path)
  (define key->path->locations (Contribution-Store-key->path->locations store))
  (hash-set! (Contribution-Store-path->contribution store) source-path contribution)
  (for ([(binding-key locations)
         (in-hash (Doc-Contribution-references contribution))])
    (define path->locations
      (hash-ref! key->path->locations binding-key make-hash))
    (hash-set! path->locations source-path locations)))

;; Time: expected O(s + L), s = citing sources for the key, L = |result|.
(define/contract (contribution-store-find-references store binding-key)
  (-> Contribution-Store? Binding-Key? (listof Location?))
  (define path->locations
    (hash-ref (Contribution-Store-key->path->locations store) binding-key (hash)))
  (for*/list ([locations (in-hash-values path->locations)]
              [location (in-list locations)])
    location))
