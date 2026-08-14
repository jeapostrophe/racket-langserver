#lang racket/base

(require "../../../common/interfaces.rkt"
         "../../../doclib/internal-types.rkt"
         "../../../lsp/compose/references.rkt"
         "../../../workspace/api.rkt"
         racket/list
         racket/set
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
  (Doc-Contribution path (hash module-binding locations)))

(module+ test
  (test-case
    "merge replaces the accepted request source and keeps the live locations"
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
        (Reference-Source live-path (list live-location))
        module-binding))
    (define sources (merge-reference-sources workspace document-result))

    (check-equal? (Reference-Source-path (first sources)) live-path)
    (check-equal? (Reference-Source-locations (first sources))
                  (list live-location))
    (check-equal? (list->set (map Reference-Source-path (rest sources)))
                  (set source-a source-b)))

  (test-case
    "merge for a local binding returns only the live source"
    (define workspace (make-workspace))
    (define live-source
      (Reference-Source (build-path root "local.rkt")
                        (list (location "file:///local.rkt" 1 0 1 1))))

    (check-equal?
      (merge-reference-sources
        workspace
        (Document-Reference-Result live-source #f))
      (list live-source)))

  (test-case
    "aggregation flattens locations in source order"
    (define a-short (location "file:///a.rkt" 0 0 0 1))
    (define a-long (location "file:///a.rkt" 0 0 0 2))
    (define a-later (location "file:///a.rkt" 1 3 1 4))
    (define b-location (location "file:///b.rkt" 0 0 0 1))
    (define sources
      (list (Reference-Source (build-path root "b.rkt")
                              (list b-location a-later))
            (Reference-Source (build-path root "a.rkt")
                              (list a-long a-short a-later a-short))))

    (check-equal? (reference-sources->locations sources)
                  (list b-location a-later a-long a-short a-later a-short))))
