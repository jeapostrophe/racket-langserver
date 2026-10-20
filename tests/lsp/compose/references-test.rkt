#lang racket/base

(require "../../../common/source.rkt"
         "../../../common/interfaces.rkt"
         "../../../doclib/internal-types.rkt"
         "../../../lsp/compose/references.rkt"
         "../../../workspace/api.rkt"
         racket/list
         rackunit)

(define root
  (build-path (current-directory) ".reference-compose-test"))

(define module-binding
  (Module-Binding (build-path root "definition.rkt") '() 0 'value))

(define (location uri start-line start-char end-line end-char)
  (Location uri
            (Range (Pos start-line start-char)
                   (Pos end-line end-char))))

(define (contribution path locations)
  (Doc-Contribution path (hash module-binding locations) (hash)))

(module+ test
  (test-case
    "reference locations replace stale request uses with live locations"
    (define workspace (make-workspace))
    (define live-path (build-path root "live.rkt"))
    (define source-a (build-path root "a.rkt"))
    (define source-b (build-path root "b.rkt"))
    (workspace-add-folder! workspace root)
    (workspace-set-contribution!
      workspace
      (contribution source-b (list (location "file:///b.rkt" 0 0 0 1))))
    (workspace-set-contribution!
      workspace
      (contribution live-path (list (location "file:///live.rkt" 1 0 1 1))))
    (workspace-set-contribution!
      workspace
      (contribution source-a (list (location "file:///a.rkt" 0 0 0 1))))

    (define live-location (location "file:///live.rkt" 2 0 2 1))
    (define document-result
      (Document-Reference-Result
        (Reference-Source (source-id (path->source live-path)) (list live-location))
        module-binding))
    (define locations (reference-locations workspace document-result #f))

    (check-equal? (first locations) live-location)
    (check-equal? (sort (rest locations) string<? #:key Location-uri)
                  (list (location "file:///a.rkt" 0 0 0 1)
                        (location "file:///b.rkt" 0 0 0 1))))

  (test-case
    "reference locations for a local binding contain only live locations"
    (define workspace (make-workspace))
    (define live-source
      (Reference-Source (source-id (path->source (build-path root "local.rkt")))
                        (list (location "file:///local.rkt" 1 0 1 1))))

    (check-equal?
      (reference-locations
        workspace
        (Document-Reference-Result live-source #f)
        #f)
      (Reference-Source-locations live-source)))

  (test-case
    "reference locations prepend an accepted external definition to workspace uses"
    (define workspace (make-workspace))
    (define client-path (build-path root "client.rkt"))
    (define definition-path (Module-Binding-filepath module-binding))
    (define client-location (location "file:///client.rkt" 2 0 2 1))
    (define definition-location (location "file:///definition.rkt" 1 8 1 9))
    (define definition-use (location "file:///definition.rkt" 2 0 2 1))
    (workspace-add-folder! workspace root)
    (workspace-set-contribution!
      workspace
      (Doc-Contribution definition-path
                        (hash module-binding (list definition-use))
                        (hash module-binding definition-location)))

    (define locations
      (reference-locations
        workspace
        (Document-Reference-Result
          (Reference-Source (source-id (path->source client-path)) (list client-location))
          module-binding)
        #t))

    (check-equal?
      locations
      (list client-location definition-location definition-use)))

  (test-case
    "reference locations include an external definition without workspace uses"
    (define workspace (make-workspace))
    (define client-path (build-path root "client.rkt"))
    (define definition-path (Module-Binding-filepath module-binding))
    (define client-location (location "file:///client.rkt" 2 0 2 1))
    (define definition-location (location "file:///definition.rkt" 1 8 1 9))
    (workspace-add-folder! workspace root)
    (workspace-set-contribution!
      workspace
      (Doc-Contribution definition-path
                        (hash)
                        (hash module-binding definition-location)))

    (define locations
      (reference-locations
        workspace
        (Document-Reference-Result
          (Reference-Source (source-id (path->source client-path)) (list client-location))
          module-binding)
        #t))

    (check-equal?
      locations
      (list client-location definition-location)))

  (test-case
    "reference locations keep only live declarations for an exact path with dot segments"
    (define workspace (make-workspace))
    (define path (build-path root 'same "definition.rkt"))
    (define binding (Module-Binding path '() 0 'value))
    (define stale-definition (location "file:///definition.rkt" 1 8 1 9))
    (define live-definition (location "file:///definition.rkt" 2 8 2 9))
    (define live-use (location "file:///definition.rkt" 3 0 3 1))
    (workspace-add-folder! workspace root)
    (workspace-set-contribution!
      workspace
      (Doc-Contribution path (hash binding (list stale-definition))
                        (hash binding stale-definition)))
    (define live-source
      (Reference-Source (source-id (path->source path)) (list live-definition live-use)))

    (check-equal?
      (reference-locations workspace (Document-Reference-Result live-source binding) #t)
      (Reference-Source-locations live-source)))

  (test-case
    "reference locations preserve source lists without sorting or deduplicating"
    (define workspace (make-workspace))
    (define live-path (build-path root "b.rkt"))
    (define other-path (build-path root "a.rkt"))
    (define a-short (location "file:///a.rkt" 0 0 0 1))
    (define a-long (location "file:///a.rkt" 0 0 0 2))
    (define a-later (location "file:///a.rkt" 1 3 1 4))
    (define b-location (location "file:///b.rkt" 0 0 0 1))
    (workspace-add-folder! workspace root)
    (workspace-set-contribution!
      workspace (contribution other-path (list a-long a-short a-later a-short)))
    (check-equal?
      (reference-locations
        workspace
        (Document-Reference-Result
          (Reference-Source (source-id (path->source live-path))
                            (list b-location a-later b-location))
          module-binding)
        #f)
      (list b-location a-later b-location a-long a-short a-later a-short))))
