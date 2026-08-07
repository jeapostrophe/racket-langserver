#lang racket/base

(require "../../common/interfaces.rkt"
         "../../doclib/internal-types.rkt"
         "../../workspace/contribution-store.rkt"
         racket/set
         rackunit)

(require/expose "../../workspace/contribution-store.rkt"
                (Contribution-Store-path->contribution
                  Contribution-Store-key->path->locations))

(define range-0
  (Range (Pos 0 0) (Pos 0 1)))

(define (location name)
  (Location (format "file:///~a.rkt" name) range-0))

(define (binding-key filepath id)
  (Binding-Key filepath '() 0 id))

(define (contribution source entries)
  (Doc-Contribution
    source
    (for/hash ([entry (in-list entries)])
      (values (car entry) (cdr entry)))))

;; Rebuild the derived index from path->contribution using the same mutable
;; shape as Contribution-Store, so check-equal? can compare them directly.
(define (rebuild-key->path->locations store)
  (define key->path->locations (make-hash))
  (for* ([(source contribution)
          (in-hash (Contribution-Store-path->contribution store))]
         [(key locations)
          (in-hash (Doc-Contribution-references contribution))])
    (define path->locations
      (hash-ref! key->path->locations key make-hash))
    (hash-set! path->locations source locations))
  key->path->locations)

(define (check-store-consistent store)
  (check-equal? (Contribution-Store-key->path->locations store)
                (rebuild-key->path->locations store)))

(module+ test
  (test-case
    "replacement keeps derived indexes consistent"
    (define store (make-contribution-store))
    (define old-key (binding-key "defined.rkt" 'old))
    (define new-key (binding-key "defined.rkt" 'new))
    (contribution-store-add!
      store
      (contribution "source.rkt"
                    (list (cons old-key (list (location "old"))))))
    (check-store-consistent store)
    (contribution-store-add!
      store
      (contribution "source.rkt"
                    (list (cons new-key (list (location "new"))))))
    (check-store-consistent store))

  (test-case
    "shared sources and source removal keep derived indexes consistent"
    (define store (make-contribution-store))
    (define key (binding-key "defined.rkt" 'shared))
    (contribution-store-add!
      store
      (contribution "source-a.rkt"
                    (list (cons key (list (location "a"))))))
    (contribution-store-add!
      store
      (contribution "source-b.rkt"
                    (list (cons key (list (location "b"))))))
    (check-store-consistent store)
    (contribution-store-remove-source! store "source-a.rkt")
    (check-store-consistent store)
    (check-equal? (contribution-store-find-references store key)
                  (list (location "b"))))

  (test-case
    "removing a path drops only that path's contribution"
    (define store (make-contribution-store))
    (define removed-key (binding-key "removed.rkt" 'removed))
    (define preserved-key (binding-key "preserved.rkt" 'preserved))
    (contribution-store-add!
      store
      (contribution
        "removed.rkt"
        (list (cons preserved-key (list (location "removed-source"))))))
    (contribution-store-add!
      store
      (contribution
        "consumer-a.rkt"
        (list (cons removed-key (list (location "still-a")))
              (cons preserved-key (list (location "preserved"))))))
    (contribution-store-add!
      store
      (contribution
        "consumer-b.rkt"
        (list (cons removed-key (list (location "still-b"))))))
    (contribution-store-remove-source! store "removed.rkt")
    (check-store-consistent store)
    (check-false
      (member "removed.rkt" (contribution-store-source-paths store)))
    (check-equal?
      (list->set (contribution-store-find-references store removed-key))
      (set (location "still-a") (location "still-b")))
    (check-equal? (contribution-store-find-references store preserved-key)
                  (list (location "preserved")))))
