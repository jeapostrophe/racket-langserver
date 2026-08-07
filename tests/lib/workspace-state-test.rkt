#lang racket/base

(require "../../common/interfaces.rkt"
         "../../doclib/internal-types.rkt"
         "../../workspace/current.rkt"
         "../../workspace/state.rkt"
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
                      (values (car entry) (cdr entry)))))

(define (binding-key path submods phase+space id)
  (Binding-Key path submods phase+space id))

(module+ test
  (test-case
    "current workspace is a workspace"
    (check-true (Workspace? current-workspace)))

  (test-case
    "contributions replace by source path"
    (define workspace (make-workspace))
    (define source (build-path root "source.rkt"))
    (define key-filepath (build-path root "defined.rkt"))
    (define old-key (binding-key key-filepath '(lib) 0 'old))
    (define new-key (binding-key key-filepath '(lib) 0 'new))
    (workspace-add-folder! workspace root)
    (workspace-set-contribution!
      workspace
      (contribution source
                    (list (cons old-key (list (location "old"))))))
    (workspace-set-contribution!
      workspace
      (contribution source
                    (list (cons new-key (list (location "new"))))))
    (check-equal? (workspace-find-references workspace old-key) '())
    (check-equal? (workspace-find-references workspace new-key)
                  (list (location "new"))))

  (test-case
    "replacement removes only the replaced source from a shared binding"
    (define workspace (make-workspace))
    (define key-filepath (build-path root "defined.rkt"))
    (define key (binding-key key-filepath '() 0 'shared))
    (define source-a (build-path root "source-a.rkt"))
    (define source-b (build-path root "source-b.rkt"))
    (workspace-add-folder! workspace root)
    (workspace-set-contribution!
      workspace
      (contribution source-a (list (cons key (list (location "a"))))))
    (workspace-set-contribution!
      workspace
      (contribution source-b (list (cons key (list (location "b"))))))
    (workspace-set-contribution! workspace (contribution source-a '()))
    (check-equal? (workspace-find-references workspace key)
                  (list (location "b"))))

  (test-case
    "overlapping roots retain contributions until all coverage is removed"
    (define workspace (make-workspace))
    (define source (build-path nested-root "source.rkt"))
    (define key (binding-key (build-path root "defined.rkt") '() 0 'value))
    (workspace-add-folder! workspace root)
    (workspace-add-folder! workspace nested-root)
    (workspace-set-contribution!
      workspace
      (contribution source (list (cons key (list (location "nested"))))))
    (workspace-remove-folder! workspace root)
    (check-equal? (workspace-find-references workspace key)
                  (list (location "nested")))
    (workspace-remove-folder! workspace nested-root)
    (check-equal? (workspace-find-references workspace key) '()))

  (test-case
    "lookup uses every exact binding identity field"
    (define workspace (make-workspace))
    (define key-filepath (build-path root "defined.rkt"))
    (define keys
      (list (binding-key key-filepath '(one) 0 'same)
            (binding-key key-filepath '(two) 0 'same)
            (binding-key key-filepath '(one) 1 'same)
            (binding-key key-filepath '(one) 0 'other)))
    (workspace-add-folder! workspace root)
    (workspace-set-contribution!
      workspace
      (contribution
        (build-path root "source.rkt")
        (for/list ([key (in-list keys)]
                   [name (in-list '(one two phase identifier))])
          (cons key (list (location name))))))
    (for ([key (in-list keys)]
          [name (in-list '(one two phase identifier))])
      (check-equal? (workspace-find-references workspace key)
                    (list (location name)))))

  (test-case
    "set rejects uncovered sources but allows outside Binding-Key filepaths"
    (define workspace (make-workspace))
    (define outside-key
      (binding-key (build-path outside-root "defined.rkt") '() 0 'outside))
    (workspace-add-folder! workspace root)
    (check-true (workspace-contains? workspace (build-path root "inside.rkt")))
    (check-false
      (workspace-contains? workspace (build-path outside-root "outside.rkt")))
    (workspace-set-contribution!
      workspace
      (contribution (build-path outside-root "source.rkt")
                    (list (cons outside-key (list (location "rejected"))))))
    (check-equal? (workspace-find-references workspace outside-key) '())
    (workspace-set-contribution!
      workspace
      (contribution (build-path root "source.rkt")
                    (list (cons outside-key (list (location "accepted"))))))
    (check-equal? (workspace-find-references workspace outside-key)
                  (list (location "accepted"))))

  (test-case
    "removing a path drops only that path's contribution"
    (define workspace (make-workspace))
    (define removed-path (build-path root "removed.rkt"))
    (define other-path (build-path root "other.rkt"))
    (define removed-key (binding-key removed-path '() 0 'removed))
    (define other-key (binding-key other-path '() 0 'other))
    (workspace-add-folder! workspace root)
    (workspace-set-contribution!
      workspace
      (contribution removed-path
                    (list (cons other-key (list (location "removed-source"))))))
    (workspace-set-contribution!
      workspace
      (contribution (build-path root "consumer.rkt")
                    (list (cons removed-key (list (location "still-present")))
                          (cons other-key (list (location "preserved"))))))
    (workspace-remove-path! workspace removed-path)
    (check-equal? (workspace-find-references workspace removed-key)
                  (list (location "still-present")))
    (check-equal? (workspace-find-references workspace other-key)
                  (list (location "preserved")))))
