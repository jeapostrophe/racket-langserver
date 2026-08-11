#lang racket/base

(require "../../common/interfaces.rkt"
         "../../common/path-util.rkt"
         "../../doclib/doc.rkt"
         "../../doclib/internal-types.rkt"
         "../../lsp/compose/references.rkt"
         "../../workspace/state.rkt"
         racket/file
         racket/path
         rackunit)

(module+ test
  (test-case
    "reference query combines live document and accepted workspace sources"
    (define tmp-dir (normalize-path "./.tmp"))
    (make-directory* tmp-dir)

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

        (define document-result
          (doc-references lib-doc lib-uri (Pos 2 9) #t))
        (check-true (Document-Reference-Result? document-result))
        (check-true (Binding-Key? (Document-Reference-Result-binding-key document-result)))
        (check-not-false
          (member (Document-Reference-Result-binding-key document-result)
                  (hash-keys
                    (Doc-Contribution-references (Doc-contribution client-doc)))))

        (define sources
          (merge-reference-sources workspace document-result))
        (check-equal? (map Reference-Source-path sources)
                      (list (Doc-Contribution-path (Doc-contribution lib-doc))
                            (Doc-Contribution-path (Doc-contribution client-doc))))
        (check-equal?
          (reference-sources->locations sources)
          (list
            (Location lib-uri (Range (Pos 1 9) (Pos 1 12)))
            (Location client-uri (Range (Pos 2 1) (Pos 2 4))))))
      (lambda ()
        (delete-file lib-path)
        (delete-file client-path)
        (delete-directory tmp-dir)))))
