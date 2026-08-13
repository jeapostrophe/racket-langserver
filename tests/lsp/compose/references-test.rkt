#lang racket/base

(require "../../../common/interfaces.rkt"
         "../../../doclib/internal-types.rkt"
         "../../../lsp/compose/references.rkt"
         "../../../workspace/api.rkt"
         racket/list
         racket/path
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
    "merge replaces the accepted request source and orders other sources"
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

    (check-equal? (map Reference-Source-path sources)
                  (list live-path source-a source-b))
    (check-equal? (Reference-Source-locations (first sources))
                  (list live-location)))

  (test-case
    "aggregation deduplicates and orders locations by URI and range"
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
                  (list a-short a-long a-later b-location))))
