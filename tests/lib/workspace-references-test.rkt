#lang racket/base

(module+ test
  (require "../silent-logger.rkt"
           rackunit
           racket/file
           racket/list
           "../../common/interfaces.rkt"
           "../../common/path-util.rkt"
           "../../doclib/doc.rkt"
           "../../doclib/internal-types.rkt"
           "../../lsp/compose/references.rkt"
           "../../workspace/api.rkt")

  (test-case "reference query combines live document and accepted workspace sources"
    (define tmp-dir (make-temporary-file "workspace-references~a" 'directory))
    (define lib-path (build-path tmp-dir "lib.rkt"))
    (define lib-text "#lang racket/base\n(provide foo)\n(define (foo) 42)\n")
    (define client-path (build-path tmp-dir "client.rkt"))
    (define client-text "#lang racket/base\n(require \"lib.rkt\")\n(foo)\n")

    (dynamic-wind
      (lambda ()
        (display-to-file lib-text lib-path #:exists 'replace)
        (display-to-file client-text client-path #:exists 'replace))
      (lambda ()
        (define lib-uri (path->uri lib-path))
        (define client-uri (path->uri client-path))
        (define lib-doc (make-doc lib-uri lib-text))
        (define client-doc (make-doc client-uri client-text))
        (check-true (doc-expand! lib-doc) "lib.rkt should expand successfully")
        (check-true (doc-expand! client-doc) "client.rkt should expand successfully")

        (define workspace (make-workspace))
        (workspace-add-folder! workspace tmp-dir)
        (workspace-set-contribution! workspace (Doc-contribution lib-doc))
        (workspace-set-contribution! workspace (Doc-contribution client-doc))

        (define document-result (doc-references lib-doc lib-uri (Pos 2 9) #t))
        (check-true (Document-Reference-Result? document-result))
        (check-true (Module-Binding? (Document-Reference-Result-module-binding document-result)))
        (check-not-false
          (member (Document-Reference-Result-module-binding document-result)
                  (hash-keys (Doc-Contribution-references (Doc-contribution client-doc)))))

        (check-equal?
          (reference-locations workspace document-result #t)
          (list (Location lib-uri (Range (Pos 2 9) (Pos 2 12)))
                (Location lib-uri (Range (Pos 1 9) (Pos 1 12)))
                (Location client-uri (Range (Pos 2 1) (Pos 2 4)))))

        (define client-result (doc-references client-doc client-uri (Pos 2 1) #t))
        (check-equal?
          (reference-locations workspace client-result #t)
          (list (Location client-uri (Range (Pos 2 1) (Pos 2 4)))
                (Location lib-uri (Range (Pos 2 9) (Pos 2 12)))
                (Location lib-uri (Range (Pos 1 9) (Pos 1 12)))))

        (define client-uses-only-result (doc-references client-doc client-uri (Pos 2 1) #f))
        (check-equal?
          (reference-locations workspace client-uses-only-result #f)
          (list (Location client-uri (Range (Pos 2 1) (Pos 2 4)))
                (Location lib-uri (Range (Pos 1 9) (Pos 1 12)))))

        (define accepted-lib-contribution (Doc-contribution lib-doc))
        (doc-apply-edit! lib-doc (Range (Pos 1 0) (Pos 1 0)) ";; shift\n")
        (define shifted-result (doc-references lib-doc lib-uri (Pos 3 9) #t))
        (check-eq? (Doc-contribution lib-doc) accepted-lib-contribution)
        (define shifted-locations (reference-locations workspace shifted-result #t))
        (check-equal?
          (take shifted-locations 2)
          (list (Location lib-uri (Range (Pos 3 9) (Pos 3 12)))
                (Location lib-uri (Range (Pos 2 9) (Pos 2 12)))))
        (define shifted-uses-only-result (doc-references lib-doc lib-uri (Pos 3 9) #f))
        (check-equal?
          (reference-locations workspace shifted-uses-only-result #f)
          (list (Location lib-uri (Range (Pos 2 9) (Pos 2 12)))
                (Location client-uri (Range (Pos 2 1) (Pos 2 4)))))
        (check-equal?
          shifted-locations
          (list (Location lib-uri (Range (Pos 3 9) (Pos 3 12)))
                (Location lib-uri (Range (Pos 2 9) (Pos 2 12)))
                (Location client-uri (Range (Pos 2 1) (Pos 2 4))))))
      (lambda () (delete-directory/files tmp-dir)))))
