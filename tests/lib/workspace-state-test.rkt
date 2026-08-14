#lang racket/base

(require "../../common/interfaces.rkt"
         "../../doclib/internal-types.rkt"
         "../../workspace/current.rkt"
         "../../workspace/api.rkt"
         rackunit)

(define root
  (build-path (current-directory) ".workspace-state-test"))
(define nested-root
  (build-path root "nested"))
(define outside-root
  (build-path (current-directory) ".outside-workspace-state-test"))

(define range-0
  (Range (Pos 0 0) (Pos 0 1)))

(define (location name)
  (Location (format "file:///~a.rkt" name) range-0))

(define (contribution path entries)
  (Doc-Contribution path
                    (for/hash ([entry (in-list entries)])
                      (values (car entry) (cdr entry)))
                    (hash)))

(define (make-module-binding path submods phase+space id)
  (Module-Binding path submods phase+space id))

(module+ test
  (test-case
    "current workspace is a workspace"
    (check-true (Workspace? current-workspace)))

  (test-case
    "contributions replace by source path"
    (define workspace (make-workspace))
    (define source (build-path root "source.rkt"))
    (define module-binding-filepath (build-path root "defined.rkt"))
    (define old-module-binding (make-module-binding module-binding-filepath '(lib) 0 'old))
    (define new-module-binding (make-module-binding module-binding-filepath '(lib) 0 'new))
    (workspace-add-folder! workspace root)
    (workspace-set-contribution!
      workspace
      (contribution source
                    (list (cons old-module-binding (list (location "old"))))))
    (workspace-set-contribution!
      workspace
      (contribution source
                    (list (cons new-module-binding (list (location "new"))))))
    (check-equal? (workspace-reference-sources workspace old-module-binding) '())
    (check-equal? (workspace-reference-sources workspace new-module-binding)
                  (list (Reference-Source source (list (location "new"))))))

  (test-case
    "replacement removes only the replaced source from a shared binding"
    (define workspace (make-workspace))
    (define module-binding-filepath (build-path root "defined.rkt"))
    (define shared (make-module-binding module-binding-filepath '() 0 'shared))
    (define source-a (build-path root "source-a.rkt"))
    (define source-b (build-path root "source-b.rkt"))
    (workspace-add-folder! workspace root)
    (workspace-set-contribution!
      workspace
      (contribution source-a (list (cons shared (list (location "a"))))))
    (workspace-set-contribution!
      workspace
      (contribution source-b (list (cons shared (list (location "b"))))))
    (workspace-set-contribution! workspace (contribution source-a '()))
    (check-equal? (workspace-reference-sources workspace shared)
                  (list (Reference-Source source-b (list (location "b"))))))

  (test-case
    "overlapping roots retain contributions until all coverage is removed"
    (define workspace (make-workspace))
    (define source (build-path nested-root "source.rkt"))
    (define value-binding (make-module-binding (build-path root "defined.rkt") '() 0 'value))
    (workspace-add-folder! workspace root)
    (workspace-add-folder! workspace nested-root)
    (workspace-set-contribution!
      workspace
      (contribution source (list (cons value-binding (list (location "nested"))))))
    (workspace-remove-folder! workspace root)
    (check-equal? (workspace-reference-sources workspace value-binding)
                  (list (Reference-Source source (list (location "nested")))))
    (workspace-remove-folder! workspace nested-root)
    (check-equal? (workspace-reference-sources workspace value-binding) '()))

  (test-case
    "lookup uses every exact binding identity field"
    (define workspace (make-workspace))
    (define module-binding-filepath (build-path root "defined.rkt"))
    (define module-bindings
      (list (make-module-binding module-binding-filepath '(one) 0 'same)
            (make-module-binding module-binding-filepath '(two) 0 'same)
            (make-module-binding module-binding-filepath '(one) 1 'same)
            (make-module-binding module-binding-filepath '(one) 0 'other)))
    (workspace-add-folder! workspace root)
    (workspace-set-contribution!
      workspace
      (contribution
        (build-path root "source.rkt")
        (for/list ([mb (in-list module-bindings)]
                   [name (in-list '(one two phase identifier))])
          (cons mb (list (location name))))))
    (for ([mb (in-list module-bindings)]
          [name (in-list '(one two phase identifier))])
      (check-equal?
        (workspace-reference-sources workspace mb)
        (list (Reference-Source (build-path root "source.rkt")
                                (list (location name)))))))

  (test-case
    "set rejects uncovered sources but allows outside Module-Binding filepaths"
    (define workspace (make-workspace))
    (define outside-module-binding
      (make-module-binding (build-path outside-root "defined.rkt") '() 0 'outside))
    (workspace-add-folder! workspace root)
    (check-true (workspace-contains? workspace (build-path root "inside.rkt")))
    (check-false
      (workspace-contains? workspace (build-path outside-root "outside.rkt")))
    (workspace-set-contribution!
      workspace
      (contribution (build-path outside-root "source.rkt")
                    (list (cons outside-module-binding (list (location "rejected"))))))
    (check-equal? (workspace-reference-sources workspace outside-module-binding) '())
    (workspace-set-contribution!
      workspace
      (contribution (build-path root "source.rkt")
                    (list (cons outside-module-binding (list (location "accepted"))))))
    (check-equal?
      (workspace-reference-sources workspace outside-module-binding)
      (list (Reference-Source (build-path root "source.rkt")
                              (list (location "accepted"))))))

  (test-case
    "removing a path drops only that path's contribution"
    (define workspace (make-workspace))
    (define removed-path (build-path root "removed.rkt"))
    (define other-path (build-path root "other.rkt"))
    (define removed-module-binding (make-module-binding removed-path '() 0 'removed))
    (define other-module-binding (make-module-binding other-path '() 0 'other))
    (workspace-add-folder! workspace root)
    (workspace-set-contribution!
      workspace
      (contribution removed-path
                    (list (cons other-module-binding (list (location "removed-source"))))))
    (workspace-set-contribution!
      workspace
      (contribution (build-path root "consumer.rkt")
                    (list (cons removed-module-binding (list (location "still-present")))
                          (cons other-module-binding (list (location "preserved"))))))
    (workspace-remove-path! workspace removed-path)
    (check-equal?
      (workspace-reference-sources workspace removed-module-binding)
      (list (Reference-Source (build-path root "consumer.rkt")
                              (list (location "still-present")))))
    (check-equal?
      (workspace-reference-sources workspace other-module-binding)
      (list (Reference-Source (build-path root "consumer.rkt")
                              (list (location "preserved")))))))
