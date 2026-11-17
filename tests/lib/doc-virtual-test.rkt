#lang racket/base

(module+ test
  (require "doc-test-support.rkt"
           rackunit
           racket/class
           racket/file
           racket/string
           "../../common/interfaces.rkt"
           "../../common/path-util.rkt"
           "../../common/source.rkt"
           "../../doclib/check-syntax.rkt"
           "../../doclib/doc.rkt"
           "../../doclib/doc-trace.rkt"
           "../../doclib/editor.rkt"
           "../../doclib/internal-types.rkt"
           "../../doclib/lexer.rkt")

  (test-case "virtual expansion retains URI syntax sources and excludes workspace contributions"
    (define text "#lang racket/base\n(require racket/list)\n(define value (first '(1)))\nvalue\n")
    (for ([uri (in-list '("untitled:Untitled-1"
                          "git:/project/main.rkt?rev=one#cell"
                          "git:/project/main.rkt?rev=two#cell"
                          "git:/project/main.rkt?rev=one#other"))])
      (define doc (make-doc uri text))
      (define editor (new lsp-editor%))
      (send editor insert text 0)
      (define collector
        (new build-trace%
          [source (uri->source uri)]
          [doc-text editor]
          [lexer-state (build-lexer-state text uri)]))
      (define expanded (expand-source (uri->source uri) (open-input-string text) collector))
      (check-true (ExpandResult-all-succeed? expanded))
      (check-equal? (syntax-source (ExpandResult-pre-syntax expanded)) uri)
      (check-equal? (syntax-source (ExpandResult-post-syntax expanded)) uri)
      (check-true (doc-expand! doc))
      (check-equal? (Doc-uri doc) uri)
      (check-false (Doc-contribution doc))
      (check-false (send collector get-contribution))))

  (test-case "virtual expansion resolves neighbors from existing URI parent directories"
    (define root (make-temporary-file "virtual-import~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([current-directory root])
          (make-directory "deps space")
          (define main-path (build-path root "deps space" "missing-main.rkt"))
          (display-to-file "#lang racket/base\n(provide value)\n(define value 42)\n"
                           (build-path root "deps space" "neighbor.rkt")
                           #:exists 'truncate)
          (define text "#lang racket/base\n(require \"neighbor.rkt\")\nvalue\n")
          (define file-uri (path->uri main-path))
          (define uris
            (append
              (for/list ([scheme (in-list '("git:" "notebook:" "custom:"))])
                (string-append (regexp-replace #rx"^file:" file-uri scheme) "?rev=one#cell"))
              '("custom:deps%20space/missing-main.rkt?rev=two#cell")))
          (check-false (file-exists? main-path))
          (for ([uri (in-list uris)])
            (define doc (make-doc uri text))
            (check-true (doc-expand! doc) uri)
            (check-equal? (Doc-uri doc) uri)
            (check-false (source-file-path (Doc-source doc)))
            (check-equal? (source-syntax-source (Doc-source doc)) uri)
            (check-false (Doc-contribution doc)))))
      (lambda () (delete-directory/files root))))

  (test-case "virtual Typed Racket syntax sources retain inferred hover and type diagnostics"
    (when (typed-racket-available?)
      (define uri "git:/project/typed.rkt?rev=one#cell")
      (define doc
        (make-doc uri
                  (string-append "#lang typed/racket\n"
                                 "(: count Integer)\n"
                                 "(define count 1)\n"
                                 "(+ count 2)\n")))
      (check-true (doc-expand! doc))
      (define hover (doc-hover doc (Pos 3 4)))
      (check-not-false hover)
      (check-equal? (Hover-range hover) (Range (Pos 3 3) (Pos 3 8)))
      (check-true (string-contains? (Hover-contents hover) "```racket\nInteger\n```"))
      (check-false (Doc-contribution doc))
      (define typed-diag
        (for/first ([diag (in-list
                            (check-syntax-diagnostics (uri->source uri)
                                                      "#lang typed/racket\n(+ 1 \"x\")\n"))]
                    #:when (equal? (Diagnostic-source diag) "Typed Racket"))
          diag))
      (check-not-false typed-diag)
      (check-equal? (Diagnostic-range typed-diag) (Range (Pos 1 5) (Pos 1 8)))
      (check-true (string-contains? (Diagnostic-message typed-diag) "expected: Number"))))

  (test-case "direct source documents retain the supplied normalized descriptor"
    (define path (build-path 'same "descriptor-preservation.rkt"))
    (define source (path->source path))
    (define normalized (normalize-source-path path))
    (define doc (make-source-doc source "#lang racket/base\n(define value 1)\nvalue\n"))
    (check-eq? (Doc-source doc) source)
    (check-equal? (source-file-path (Doc-source doc)) normalized)
    (check-equal? (source-syntax-source (Doc-source doc)) normalized)
    (check-equal? (Doc-uri doc) (source-uri source))
    (check-true (doc-expand! doc))
    (check-eq? (Doc-source doc) source)
    (check-equal? (Doc-Contribution-path (Doc-contribution doc)) normalized)))
