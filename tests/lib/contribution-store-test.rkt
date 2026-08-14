#lang racket/base

(require "../../common/interfaces.rkt"
         "../../doclib/internal-types.rkt"
         "../../workspace/contribution-store.rkt"
         racket/path
         racket/set
         rackunit)

(require/expose "../../workspace/contribution-store.rkt"
                (Contribution-Store-path->contribution
                  Contribution-Store-module-binding->path->locations
                  Contribution-Store-module-binding->path->definition))

(define range-0
  (Range (Pos 0 0) (Pos 0 1)))

(define (location name)
  (Location (format "file:///~a.rkt" name) range-0))

(define (module-binding filepath id)
  (Module-Binding filepath '() 0 id))

(define (contribution source entries [definition-entries '()])
  (Doc-Contribution
    source
    (for/hash ([entry (in-list entries)])
      (values (car entry) (cdr entry)))
    (for/hash ([entry (in-list definition-entries)])
      (values (car entry) (cdr entry)))))

;; Rebuild the derived index from path->contribution using the same mutable
;; shape as Contribution-Store, so check-equal? can compare them directly.
(define (rebuild-module-binding->path->locations store)
  (define module-binding->path->locations (make-hash))
  (for* ([(source contribution)
          (in-hash (Contribution-Store-path->contribution store))]
         [(module-binding locations)
          (in-hash (Doc-Contribution-references contribution))])
    (define path->locations
      (hash-ref! module-binding->path->locations module-binding make-hash))
    (hash-set! path->locations source locations))
  module-binding->path->locations)

(define (rebuild-module-binding->path->definition store)
  (define module-binding->path->definition (make-hash))
  (for* ([(source contribution)
          (in-hash (Contribution-Store-path->contribution store))]
         [(module-binding definition)
          (in-hash (Doc-Contribution-definitions contribution))])
    (define path->definition
      (hash-ref! module-binding->path->definition module-binding make-hash))
    (hash-set! path->definition source definition))
  module-binding->path->definition)

(define (check-store-consistent store)
  (check-equal? (Contribution-Store-module-binding->path->locations store)
                (rebuild-module-binding->path->locations store))
  (check-equal? (Contribution-Store-module-binding->path->definition store)
                (rebuild-module-binding->path->definition store)))

(module+ test
  (test-case
    "replacement keeps derived indexes consistent"
    (define store (make-contribution-store))
    (define source-path (string->path "source.rkt"))
    (define old-module-binding (module-binding source-path 'old))
    (define new-module-binding (module-binding source-path 'new))
    (contribution-store-add!
      store
      (contribution source-path
                    (list (cons old-module-binding (list (location "old"))))
                    (list (cons old-module-binding (location "old-definition")))))
    (check-store-consistent store)
    (check-equal? (contribution-store-definition-location store old-module-binding)
                  (location "old-definition"))
    (contribution-store-add!
      store
      (contribution source-path
                    (list (cons new-module-binding (list (location "new"))))
                    (list (cons new-module-binding (location "new-definition")))))
    (check-store-consistent store)
    (check-false (contribution-store-definition-location store old-module-binding))
    (check-equal? (contribution-store-definition-location store new-module-binding)
                  (location "new-definition")))

  (test-case
    "shared sources and source removal keep derived indexes consistent"
    (define store (make-contribution-store))
    (define shared (module-binding (string->path "defined.rkt") 'shared))
    (contribution-store-add!
      store
      (contribution (string->path "source-a.rkt")
                    (list (cons shared (list (location "a"))))))
    (contribution-store-add!
      store
      (contribution (string->path "source-b.rkt")
                    (list (cons shared (list (location "b"))))))
    (check-store-consistent store)
    (contribution-store-remove-source! store (string->path "source-a.rkt"))
    (check-store-consistent store)
    (check-equal? (contribution-store-reference-sources store shared)
                  (list (Reference-Source (string->path "source-b.rkt")
                                          (list (location "b"))))))

  (test-case
    "removing a path drops only that path's contribution"
    (define store (make-contribution-store))
    (define removed-module-binding (module-binding (string->path "removed.rkt") 'removed))
    (define preserved-module-binding (module-binding (string->path "preserved.rkt") 'preserved))
    (contribution-store-add!
      store
      (contribution
        (string->path "removed.rkt")
        (list (cons preserved-module-binding (list (location "removed-source"))))
        (list (cons removed-module-binding (location "removed-definition")))))
    (contribution-store-add!
      store
      (contribution
        (string->path "consumer-a.rkt")
        (list (cons removed-module-binding (list (location "still-a")))
              (cons preserved-module-binding (list (location "preserved"))))))
    (contribution-store-add!
      store
      (contribution
        (string->path "consumer-b.rkt")
        (list (cons removed-module-binding (list (location "still-b"))))))
    (contribution-store-remove-source! store (string->path "removed.rkt"))
    (check-store-consistent store)
    (check-false
      (member (string->path "removed.rkt") (contribution-store-source-paths store)))
    (check-false
      (contribution-store-definition-location store removed-module-binding))
    (check-equal?
      (list->set (contribution-store-reference-sources store removed-module-binding))
      (set (Reference-Source (string->path "consumer-a.rkt") (list (location "still-a")))
           (Reference-Source (string->path "consumer-b.rkt") (list (location "still-b")))))
    (check-equal? (contribution-store-reference-sources store preserved-module-binding)
                  (list
                    (Reference-Source
                      (string->path "consumer-a.rkt")
                      (list (location "preserved")))))))
