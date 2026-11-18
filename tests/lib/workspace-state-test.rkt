#lang racket/base

(require "../../common/source.rkt"
         "../../common/interfaces.rkt"
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

(module+ test
  (test-case
    "current workspace is a workspace"
    (check-true (Workspace? current-workspace)))

  (test-case
    "overlapping roots retain contributions until all coverage is removed"
    (define workspace (make-workspace))
    (define source (build-path nested-root "source.rkt"))
    (define value-binding (Module-Binding (build-path root "defined.rkt") '() 0 'value))
    (workspace-add-folder! workspace root)
    (workspace-add-folder! workspace nested-root)
    (workspace-set-contribution!
      workspace
      (contribution source (list (cons value-binding (list (location "nested"))))))
    (workspace-remove-folder! workspace root)
    (check-equal? (workspace-reference-sources workspace value-binding)
                  (list (Reference-Source (source-id (path->source source))
                                          (list (location "nested")))))
    (workspace-remove-folder! workspace nested-root)
    (check-equal? (workspace-reference-sources workspace value-binding) '()))

  (test-case
    "lookup uses every exact binding identity field"
    (define workspace (make-workspace))
    (define module-binding-filepath (build-path root "defined.rkt"))
    (define module-bindings
      (list (Module-Binding module-binding-filepath '(one) 0 'same)
            (Module-Binding module-binding-filepath '(two) 0 'same)
            (Module-Binding module-binding-filepath '(one) 1 'same)
            (Module-Binding module-binding-filepath '(one) 0 'other)))
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
        (list (Reference-Source (source-id (path->source (build-path root "source.rkt")))
                                (list (location name)))))))

  (test-case
    "set rejects uncovered sources but allows outside Module-Binding filepaths"
    (define workspace (make-workspace))
    (define outside-module-binding
      (Module-Binding (build-path outside-root "defined.rkt") '() 0 'outside))
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
      (list (Reference-Source (source-id (path->source (build-path root "source.rkt")))
                              (list (location "accepted"))))))

  (test-case "a trailing slash does not split a folder key"
    (define root (build-path (current-directory) "folder-key"))
    (define workspace (make-workspace))
    (workspace-add-folder! workspace root)
    (check-true (workspace-contains? workspace (build-path root "source.rkt")))
    (workspace-remove-folder!
      workspace
      (string->path (string-append (path->string root) "/")))
    (check-false (workspace-contains? workspace (build-path root "source.rkt")))))
