#lang racket/base

(module+ test
  (require "doc-test-support.rkt"
           drracket/check-syntax
           rackunit
           racket/class
           racket/file
           racket/match
           "../../common/interfaces.rkt"
           "../../common/path-util.rkt"
           "../../common/source.rkt"
           "../../doclib/check-syntax-compat.rkt"
           "../../doclib/doc.rkt"
           "../../doclib/internal-types.rkt")

  (test-case "doc-definition captures a for-syntax import without resolving the target"
    (define temp-dir (make-temporary-file "import-shift~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (define lib-path (build-path temp-dir "lib.rkt"))
        (display-to-file "#lang racket/base\n(provide foo)\n(define foo 1)\n"
                         lib-path
                         #:exists 'truncate)
        (define use-path (build-path temp-dir "use.rkt"))
        (define use-text
          (string-append
            "#lang racket/base\n"
            "(require (for-syntax racket/base \"lib.rkt\"))\n"
            "(begin-for-syntax\n"
            "  (define y foo))\n"))
        (display-to-file use-text use-path #:exists 'truncate)
        (define use-uri (path->uri use-path))
        (define d (make-doc use-uri use-text))
        (check-true (doc-expand! d))
        (check-equal? (doc-definition d use-uri (Pos 3 12))
                      (Module-Binding lib-path '() (if phase+space-callbacks? 1 0) 'foo)))
      (lambda () (delete-directory/files temp-dir))))

  (test-case "binding identity distinguishes equal symbols across submodules and phases"
    (define path (string->path "/tmp/binding-identity-test.rkt"))
    (define uri "file:///tmp/binding-identity-test.rkt")
    (define trace (make-trace path (make-string 40 #\space)))

    (send trace syncheck:add-jump-to-definition/phase-level+space path 1 2 'same path '(first) 0)
    (send trace syncheck:add-jump-to-definition/phase-level+space path 3 4 'same path '(second) 1)
    (send trace syncheck:add-definition-target/phase-level+space path 10 11 'same '(first) 0)
    (send trace syncheck:add-definition-target/phase-level+space path 20 21 'same '(second) 1)

    (define declaration-service (send trace get-declaration))
    (define first-binding (Module-Binding path '(first) 0 'same))
    (define second-binding (Module-Binding path '(second) 1 'same))
    (check-equal? (send declaration-service module-binding-at 1) first-binding)
    (check-equal? (send declaration-service module-binding-at 3) second-binding)
    (check-equal? (send declaration-service definition-at 1) (CharRange 10 11))
    (check-equal? (send declaration-service definition-at 3) (CharRange 20 21))

    (define definitions (Doc-Contribution-definitions (send trace get-contribution)))
    (check-equal? (hash-ref definitions first-binding)
                  (Location uri (Range (Pos 0 10) (Pos 0 11))))
    (check-equal? (hash-ref definitions second-binding)
                  (Location uri (Range (Pos 0 20) (Pos 0 21))))

    (send trace expand 0 2)
    (check-equal? (send declaration-service module-binding-at 3) first-binding)
    (check-equal? (send declaration-service definition-at 3) (CharRange 12 13))
    (send trace contract 0 2)
    (check-equal? (send declaration-service module-binding-at 1) first-binding)
    (check-equal? (send declaration-service definition-at 1) (CharRange 10 11)))

  (test-case "trace contribution groups module uses by exact binding identity"
    (define path (string->path "/tmp/contribution-test.rkt"))
    (define uri "file:///tmp/contribution-test.rkt")
    (define text "first use\nsecond use\n")
    (define trace (make-trace path text))

    (send trace syncheck:add-jump-to-definition/phase-level+space path 0 5 'same path '(first) 0)
    (send trace syncheck:add-jump-to-definition/phase-level+space path 10 16 'same path '(first) 0)
    (send trace syncheck:add-jump-to-definition/phase-level+space path 17 20 'same path '(second) 1)
    (send trace syncheck:add-arrow/name-dup path 6 9 path 21 21 #t 0 #f #f)

    (define contribution (send trace get-contribution))
    (define references (Doc-Contribution-references contribution))
    (define declaration-service (send trace get-declaration))
    (define first-module-binding (Module-Binding path '(first) 0 'same))
    (define second-module-binding (Module-Binding path '(second) 1 'same))
    (check-equal? (Doc-Contribution-path contribution) path)
    (check-true (immutable? references))
    (check-equal? (hash-count references) 2)
    (check-equal?
      (sort (hash-ref references first-module-binding)
            <
            #:key (lambda (location) (Pos-line (Range-start (Location-range location)))))
      (list (Location uri (Range (Pos 0 0) (Pos 0 5)))
            (Location uri (Range (Pos 1 0) (Pos 1 6)))))
    (check-equal? (hash-ref references second-module-binding)
                  (list (Location uri (Range (Pos 1 7) (Pos 1 10)))))
    (check-equal? (send declaration-service uses-at 0)
                  (list (CharRange 0 5) (CharRange 10 16)))
    (check-false (send declaration-service definition-at 0))

    (define d (make-doc uri text 7))
    (doc-update-trace! d trace contribution 7)
    (check-eq? (Doc-contribution d) contribution)
    (check-true (doc-trace-latest? d))
    (check-equal? (doc-module-binding-at d (Pos 0 0)) first-module-binding))

  (test-case "named binding queries resolve local and same-file module uses after edits"
    (define path (string->path "/tmp/binding-query-test.rkt"))
    (define trace (make-trace path (make-string 50 #\space)))
    (send trace syncheck:add-definition-target/phase-level+space path 5 6 'module-name '() 0)
    (send trace syncheck:add-arrow/name-dup path 5 6 path 10 11 #t 0 #f #f)
    (send trace syncheck:add-arrow/name-dup path 5 6 path 20 21 #t 0 #f #f)
    (send trace syncheck:add-arrow/name-dup path 30 31 path 35 36 #t 0 #f #f)
    (send trace syncheck:add-arrow/name-dup path 30 31 path 40 41 #t 0 #f #f)
    (send trace syncheck:add-arrow/name-dup path 30 31 path 45 46 #t 0 #t #f)

    (define declaration-service (send trace get-declaration))
    (define module-binding (Module-Binding path '() 0 'module-name))
    (check-equal? (send declaration-service module-binding-at 10) module-binding)
    (check-false (send declaration-service module-binding-at 35))
    (check-false (send declaration-service occurrence-at 45))
    (check-equal? (send declaration-service occurrence-at 30) (CharRange 30 31))
    (check-equal? (send declaration-service uses-at 10)
                  (list (CharRange 10 11) (CharRange 20 21)))
    (check-equal? (send declaration-service uses-at 35)
                  (list (CharRange 35 36) (CharRange 40 41)))
    (check-equal? (send declaration-service definition-at 35) (CharRange 30 31))

    (send trace expand 0 2)
    (check-equal? (send declaration-service occurrence-at 37) (CharRange 37 38))
    (check-equal? (send declaration-service uses-at 12)
                  (list (CharRange 12 13) (CharRange 22 23)))
    (check-equal? (send declaration-service definition-at 12) (CharRange 7 8))
    (check-equal? (send declaration-service definition-at 37) (CharRange 32 33))
    (check-equal? (send declaration-service uses-at 37)
                  (list (CharRange 37 38) (CharRange 42 43))))

  (test-case "declaration collection is independent of callback order"
    (define path (string->path "/tmp/binding-callback-order-test.rkt"))
    (define trace (make-trace path (make-string 40 #\space)))
    (send trace syncheck:add-jump-to-definition/phase-level+space
          path 10 11 'arrow-first path '() 0)
    (send trace syncheck:add-definition-target/phase-level+space path 5 6 'arrow-first '() 0)
    (send trace syncheck:add-definition-target/phase-level+space path 20 21 'definition-first '() 0)
    (send trace syncheck:add-jump-to-definition/phase-level+space
          path 25 26 'definition-first path '() 0)
    (send trace syncheck:add-definition-target/phase-level+space path 30 31 'unused '() 0)

    (define declaration-service (send trace get-declaration))
    (define arrow-first-binding (Module-Binding path '() 0 'arrow-first))
    (define definition-first-binding (Module-Binding path '() 0 'definition-first))
    (for ([position (in-list '(5 10))])
      (check-equal? (send declaration-service module-binding-at position) arrow-first-binding))
    (for ([position (in-list '(20 25))])
      (check-equal? (send declaration-service module-binding-at position) definition-first-binding))
    (check-equal? (send declaration-service definition-at 10) (CharRange 5 6))
    (check-equal? (send declaration-service definition-at 25) (CharRange 20 21))
    (check-equal? (send declaration-service uses-at 5) (list (CharRange 10 11)))
    (check-equal? (send declaration-service uses-at 20) (list (CharRange 25 26)))
    (check-equal? (send declaration-service occurrence-at 30) (CharRange 30 31))
    (check-equal? (send declaration-service definition-at 30) (CharRange 30 31))
    (check-equal? (send declaration-service module-binding-at 30)
                  (Module-Binding path '() 0 'unused))
    (check-equal? (send declaration-service uses-at 30) '())
    (check-equal?
      (for/hash ([entry (in-list (send declaration-service module-binding-uses))])
        (values (car entry) (cdr entry)))
      (hash (CharRange 10 11) arrow-first-binding
            (CharRange 25 26) definition-first-binding)))

  (test-case "a declaration moves once after an edit inside its range"
    (define path (string->path "/tmp/binding-declaration-edit-test.rkt"))
    (define trace (make-trace path (make-string 50 #\space)))
    (send trace syncheck:add-arrow/name-dup path 30 33 path 35 38 #t 0 #f #f)
    (define declaration-service (send trace get-declaration))

    (send trace expand 31 32)
    (check-equal? (send declaration-service occurrence-at 30) (CharRange 30 34))
    (check-equal? (send declaration-service definition-at 33) (CharRange 30 34))
    (check-equal? (send declaration-service occurrence-at 31) (CharRange 30 34))

    (send trace contract 31 32)
    (check-equal? (send declaration-service occurrence-at 30) (CharRange 30 33))
    (check-equal? (send declaration-service definition-at 30) (CharRange 30 33))

    (send trace expand 0 2)
    (check-equal? (send declaration-service occurrence-at 32) (CharRange 32 35))
    (check-equal? (send declaration-service definition-at 32) (CharRange 32 35))
    (check-equal? (send declaration-service uses-at 32) (list (CharRange 37 40))))

  (test-case "a declaration replays several edits when first read"
    (define path (string->path "/tmp/binding-declaration-replay-test.rkt"))
    (define trace (make-trace path (make-string 60 #\space)))
    (send trace syncheck:add-arrow/name-dup path 30 33 path 40 43 #t 0 #f #f)
    (define declaration-service (send trace get-declaration))

    (send trace expand 0 2)
    (send trace expand 33 35)
    (send trace contract 34 35)

    (check-equal? (send declaration-service occurrence-at 32) (CharRange 32 36))
    (check-equal? (send declaration-service definition-at 32) (CharRange 32 36))
    (check-equal? (send declaration-service uses-at 32) (list (CharRange 43 46))))

  (test-case "deleting declaration text clips and then removes its local binding"
    (define path (string->path "/tmp/binding-declaration-delete-test.rkt"))
    (define trace (make-trace path (make-string 60 #\space)))
    (send trace syncheck:add-arrow/name-dup path 30 35 path 40 43 #t 0 #f #f)
    (define declaration-service (send trace get-declaration))

    (send trace contract 32 37)
    (check-equal? (send declaration-service definition-at 35) (CharRange 30 32))

    (send trace contract 30 32)
    (check-equal? (send declaration-service occurrence-at 33) (CharRange 33 36))
    (check-false (send declaration-service definition-at 33)))

  (test-case "deleting a module declaration preserves its position-free binding"
    (define path (string->path "/tmp/module-declaration-delete-test.rkt"))
    (define trace (make-trace path (make-string 40 #\space)))
    (send trace syncheck:add-definition-target/phase-level+space path 10 11 'module-name '() 0)
    (send trace syncheck:add-arrow/name-dup path 10 11 path 20 21 #t 0 #f #f)
    (define declaration-service (send trace get-declaration))
    (define module-binding (Module-Binding path '() 0 'module-name))

    (send trace contract 10 11)
    (check-equal? (send declaration-service module-binding-at 19) module-binding)
    (check-equal? (send declaration-service uses-at 19) (list (CharRange 19 20)))
    (check-false (send declaration-service definition-at 19)))

  (test-case "failed expansion preserves the accepted contribution"
    (define d
      (make-doc "file:///tmp/contribution-lifecycle-test.rkt"
                "#lang racket/base\n(require racket/list)\nfirst\n"))
    (check-true (doc-expand! d))
    (define accepted-contribution (Doc-contribution d))
    (check-equal? (Doc-Contribution-path accepted-contribution)
                  (string->path "/tmp/contribution-lifecycle-test.rkt"))
    (check-true (positive? (hash-count (Doc-Contribution-references accepted-contribution))))
    (define accepted-pos (Pos 2 0))
    (define accepted-target (doc-module-binding-at d accepted-pos))

    (doc-apply-edit! d (Range (Pos 3 0) (Pos 3 0)) "(")
    (check-false (doc-expand! d))
    (check-eq? (Doc-contribution d) accepted-contribution)
    (check-equal? (doc-module-binding-at d accepted-pos) accepted-target)

    (doc-reset! d "#lang racket/base\n42\n")
    (check-true (doc-expand! d))
    (check-not-eq? (Doc-contribution d) accepted-contribution))

  (for ([names (in-list '((a) (struct:a a? a)))])
    (test-case (format "definition targets ~s retain their shared range" names)
      (define path (build-path (current-directory) "m.rkt"))
      (define uri (path->uri path))
      (define trace (make-trace path "#lang racket/base\n(provide a?)\n(struct a ())\n"))
      ;; Check Syntax reports all three struct bindings at the written a.
      (for ([name (in-list names)])
        (send trace syncheck:add-definition-target/phase-level+space path 39 40 name '() 0))
      (check-equal?
        (for/hash ([entry (in-list
                            (send (send trace get-declaration) module-binding-definitions))])
          (values (cdr entry) (car entry)))
        (for/hash ([name (in-list names)])
          (values (Module-Binding path '() 0 name) (CharRange 39 40))))
      (check-equal?
        (Doc-Contribution-definitions (send trace get-contribution))
        (for/hash ([name (in-list names)])
          (values (Module-Binding path '() 0 name)
                  (Location uri (Range (Pos 2 8) (Pos 2 9))))))))

  (define default-doc (make-expanded-doc))
  (define default-uri (Doc-uri default-doc))

  (for ([spec (in-list
                (list
                  (list "a binding usage" (Pos 2 0) (Range (Pos 2 0) (Pos 2 1)) 'x '())
                  (list "the definition site" (Pos 1 8) (Range (Pos 1 8) (Pos 1 9)) 'x '())
                  (list "imported 'define'" (Pos 1 1) (Range (Pos 1 1) (Pos 1 7)) 'new-define '())))])
    (match-define (list label pos expected-range expected-id expected-submods) spec)
    (test-case (format "position queries on ~a" label)
      (check-equal? (doc-occurrence-at default-doc pos) expected-range)
      (define target (doc-module-binding-at default-doc pos))
      (check-true (Module-Binding? target))
      (check-equal? (Module-Binding-submods target) expected-submods)
      (check-equal? (Module-Binding-phase+space target) 0)
      (check-equal? (Module-Binding-id target) expected-id)))

  (test-case "position queries on literal return no occurrence"
    (check-false (doc-occurrence-at default-doc (Pos 1 10)))
    (check-false (doc-module-binding-at default-doc (Pos 1 10))))

  (test-case "uses-at returns usage ranges for local x"
    (check-equal? (doc-uses-at default-doc (Pos 2 0)) (list (Range (Pos 2 0) (Pos 2 1)))))

  (test-case "binding-ranges-at includes definition then uses for local x"
    (check-equal? (doc-binding-ranges-at default-doc (Pos 2 0))
                  (list (Range (Pos 1 8) (Pos 1 9))
                        (Range (Pos 2 0) (Pos 2 1)))))

  (test-case "binding-ranges-at for imported define is uses only"
    (check-false (doc-definition-at default-doc (Pos 1 1)))
    (check-equal? (doc-binding-ranges-at default-doc (Pos 1 1))
                  (doc-uses-at default-doc (Pos 1 1))))

  (test-case "doc-definition for local x usage, imported define, and literal"
    (define local-def (doc-definition default-doc default-uri (Pos 2 0)))
    (check-equal? (Location-uri local-def) default-uri)
    (check-equal? (Location-range local-def) (Range (Pos 1 8) (Pos 1 9)))

    (define imported-def (doc-definition default-doc default-uri (Pos 1 1)))
    (check-true (Module-Binding? imported-def))
    (check-not-equal? (Module-Binding-filepath imported-def) (uri->path default-uri))

    (check-false (doc-definition default-doc default-uri (Pos 1 10))))

  (test-case "Document positions count UTF-16 code units after astral characters"
    (define uri "file:///tmp/doc-utf16-test.rkt")
    (define d (make-doc uri "#lang racket\n\"\U0001F600\" (define x 1) x\n"))
    (check-equal? (doc-pos->abs-pos d (Pos 1 13)) 25)
    (check-equal? (doc-abs-pos->pos d 25) (Pos 1 13))
    (doc-expand! d)
    (check-equal? (Location-range (doc-definition d uri (Pos 1 18)))
                  (Range (Pos 1 13) (Pos 1 14)))
    (doc-apply-edit! d (Range (Pos 1 18) (Pos 1 19)) "y")
    (check-equal? (doc-get-text d) "#lang racket\n\"\U0001F600\" (define x 1) y\n"))

  (define crlf-text "#lang racket\r\n(define x 1)\r\n#; x (+ x x)\r\n")

  (test-case "Definitions and references count each CRLF as two characters"
    (define def-uri "file:///tmp/doc-crlf-definition-test.rkt")
    (define def-doc (make-expanded-doc def-uri crlf-text))
    (check-equal? (Location-range (doc-definition def-doc def-uri (Pos 2 8)))
                  (Range (Pos 1 8) (Pos 1 9)))

    (define ref-uri "file:///tmp/doc-crlf-references-test.rkt")
    (define ref-doc (make-expanded-doc ref-uri crlf-text))
    (check-equal? (map Location-range
                       (Reference-Source-locations
                         (Document-Reference-Result-source (doc-references ref-doc ref-uri (Pos 2 8) #t))))
                  (list (Range (Pos 1 8) (Pos 1 9))
                        (Range (Pos 2 8) (Pos 2 9))
                        (Range (Pos 2 10) (Pos 2 11)))))

  (test-case "doc-references returns live sources for module and local bindings and shifts on edit"
    (define result (doc-references default-doc default-uri (Pos 2 0) #t))
    (check-true (Module-Binding? (Document-Reference-Result-module-binding result)))
    (define src (Document-Reference-Result-source result))
    (check-equal? (Reference-Source-id src) (source-id (uri->source default-uri)))
    (check-equal? (map Location-range (Reference-Source-locations src))
                  (list (Range (Pos 1 8) (Pos 1 9))
                        (Range (Pos 2 0) (Pos 2 1))))
    (check-equal?
      (map Location-range
           (Reference-Source-locations
             (Document-Reference-Result-source (doc-references default-doc default-uri (Pos 2 0) #f))))
      (list (Range (Pos 2 0) (Pos 2 1))))

    (define local-uri "file:///tmp/doc-local-reference-test.rkt")
    (define local-doc
      (make-expanded-doc local-uri "#lang racket\n(let ([x 1])\n  x\n  x)\n"))
    (define with-decl (doc-references local-doc local-uri (Pos 2 2) #t))
    (check-false (Document-Reference-Result-module-binding with-decl))
    (check-equal? (map Location-range (Reference-Source-locations (Document-Reference-Result-source with-decl)))
                  (list (Range (Pos 1 7) (Pos 1 8))
                        (Range (Pos 2 2) (Pos 2 3))
                        (Range (Pos 3 2) (Pos 3 3))))
    (define uses-only (doc-references local-doc local-uri (Pos 2 2) #f))
    (check-equal? (map Location-range (Reference-Source-locations (Document-Reference-Result-source uses-only)))
                  (list (Range (Pos 2 2) (Pos 2 3))
                        (Range (Pos 3 2) (Pos 3 3))))

    (define shift-uri "file:///tmp/doc-reference-edit-test.rkt")
    (define shift-doc
      (make-expanded-doc shift-uri "#lang racket\n(define x 1)\nx\nx\n"))
    (define accepted-contribution (Doc-contribution shift-doc))
    (define before-edit (doc-references shift-doc shift-uri (Pos 2 0) #t))
    (define module-binding (Document-Reference-Result-module-binding before-edit))
    (check-equal?
      (sort (map Location-range
                 (hash-ref (Doc-Contribution-references accepted-contribution) module-binding))
            <
            #:key (lambda (range) (Pos-line (Range-start range))))
      (list (Range (Pos 2 0) (Pos 2 1))
            (Range (Pos 3 0) (Pos 3 1))))
    (doc-apply-edit! shift-doc (Range (Pos 1 0) (Pos 1 0)) ";; shift\n")
    (define after-edit (doc-references shift-doc shift-uri (Pos 3 0) #t))
    (check-eq? (Doc-contribution shift-doc) accepted-contribution)
    (check-equal? (Document-Reference-Result-module-binding after-edit) module-binding)
    (check-equal?
      (map Location-range (Reference-Source-locations (Document-Reference-Result-source after-edit)))
      (list (Range (Pos 2 8) (Pos 2 9))
            (Range (Pos 3 0) (Pos 3 1))
            (Range (Pos 4 0) (Pos 4 1)))))

  (test-case "doc-highlights for local x, quoted data, unused binders, and quote arrows"
    (check-equal? (map DocumentHighlight-range (doc-highlights default-doc (Pos 2 0)))
                  (list (Range (Pos 1 8) (Pos 1 9))
                        (Range (Pos 2 0) (Pos 2 1))))

    (define quoted-doc
      (make-expanded-doc "file:///tmp/doc-quoted-data-highlight-test.rkt"
                         "#lang racket\n(define x 1)\nx\n(quote x)\n'x\n"))
    (check-equal? (doc-highlights quoted-doc (Pos 3 7)) '())
    (check-equal? (doc-highlights quoted-doc (Pos 4 1)) '())

    (define shadowed-doc
      (make-expanded-doc "file:///tmp/doc-unused-binder-highlight-test.rkt"
                         "#lang racket\n(define x 1)\n(let ([x 0])\n  1)\n(lambda (x) 1)\n"))
    (check-equal? (map DocumentHighlight-range (doc-highlights shadowed-doc (Pos 1 8)))
                  (list (Range (Pos 1 8) (Pos 1 9))))
    (check-equal? (map DocumentHighlight-range (doc-highlights shadowed-doc (Pos 2 7)))
                  (if unused-binder-callbacks? (list (Range (Pos 2 7) (Pos 2 8))) '()))
    (check-equal? (map DocumentHighlight-range (doc-highlights shadowed-doc (Pos 4 9)))
                  (if unused-binder-callbacks? (list (Range (Pos 4 9) (Pos 4 10))) '()))
    (check-true (Module-Binding? (doc-module-binding-at shadowed-doc (Pos 1 8))))
    (check-false (doc-module-binding-at shadowed-doc (Pos 2 7)))
    (check-false (doc-module-binding-at shadowed-doc (Pos 4 9)))

    (define quote-doc
      (make-expanded-doc "file:///tmp/doc-module-language-quote-test.rkt"
                         "#lang racket\n'(1 2)\n'(3 4)\n"))
    (check-equal? (map DocumentHighlight-range (doc-highlights quote-doc (Pos 1 0)))
                  (list (Range (Pos 1 0) (Pos 1 1))
                        (Range (Pos 2 0) (Pos 2 1)))))

  (test-case "module-language uses stay local, non-defining, and binding-aware"
    (define uri "file:///tmp/doc-module-language-use-test.rkt")
    (define d
      (make-expanded-doc
        uri
        (string-append "#lang racket\n"
                       "(define i+1-th 0)\n"
                       "(+ 1 2)\n"
                       "(* 3 4)\n"
                       "(let ([+ -])\n"
                       "  (+ 5 2))\n"
                       "(+ i+1-th 3)\n")))
    (define imported-plus-ranges
      (list (Range (Pos 2 1) (Pos 2 2))
            (Range (Pos 6 1) (Pos 6 2))))
    (check-equal? (map DocumentHighlight-range (doc-highlights d (Pos 2 1))) imported-plus-ranges)
    (check-equal? (map DocumentHighlight-range (doc-highlights d (Pos 3 1)))
                  (list (Range (Pos 3 1) (Pos 3 2))))
    (check-equal? (map DocumentHighlight-range (doc-highlights d (Pos 5 3)))
                  (list (Range (Pos 4 7) (Pos 4 8))
                        (Range (Pos 5 3) (Pos 5 4))))

    (define references (doc-references d uri (Pos 2 1) #t))
    (check-false (Document-Reference-Result-module-binding references))
    (check-equal? (map Location-range (Reference-Source-locations (Document-Reference-Result-source references)))
                  imported-plus-ranges)
    (check-false (doc-definition d uri (Pos 2 1)))
    (check-false (doc-prepare-rename d (Pos 2 1)))
    (check-false (doc-rename d uri (Pos 2 1) "plus"))

    (define contribution-ranges
      (for*/list ([locations (in-hash-values (Doc-Contribution-references (Doc-contribution d)))]
                  [location (in-list locations)])
        (Location-range location)))
    (for ([range (in-list imported-plus-ranges)])
      (check-false (member range contribution-ranges)))

    (doc-apply-edit! d (Range (Pos 2 0) (Pos 2 0)) ";; shift\n")
    (check-equal? (map DocumentHighlight-range (doc-highlights d (Pos 3 1)))
                  (list (Range (Pos 3 1) (Pos 3 2))
                        (Range (Pos 7 1) (Pos 7 2)))))

  (test-case "module-language use collection filters and defers endpoints"
    (define path (string->path "/tmp/module-language-use-service-test.rkt"))
    (define trace (make-trace path "racket + * ' word"))
    (define declaration-service (send trace get-declaration))

    (send trace syncheck:add-arrow/name-dup path 0 6 path 7 8 #t 0 'module-lang #f)
    (send trace syncheck:add-arrow/name-dup path 0 6 path 9 10 #t 0 'module-lang #f)
    (send trace syncheck:add-arrow/name-dup path 0 6 path 11 12 #t 0 'module-lang #f)
    (send trace syncheck:add-arrow/name-dup path 0 6 path 13 13 #t 0 'module-lang #f)
    (send trace syncheck:add-arrow/name-dup path 0 6 path 13 17 #t 0 #t #f)
    (send declaration-service walk-stx #f)

    (check-equal? (send declaration-service occurrence-at 7) (CharRange 7 8))
    (check-equal? (send declaration-service occurrence-at 9) (CharRange 9 10))
    (check-equal? (send declaration-service occurrence-at 11) (CharRange 11 12))
    (check-false (send declaration-service occurrence-at 13))
    (check-equal? (send declaration-service uses-at 7) (list (CharRange 7 8)))
    (check-false (send declaration-service definition-at 7))
    (check-false (send declaration-service module-binding-at 7)))

  (test-case "exact jumps win over pending module-language uses"
    (define path (string->path "/tmp/module-language-use-jump-test.rkt"))
    (define trace (make-trace path "racket exact"))
    (send trace syncheck:add-arrow/name-dup path 0 6 path 7 12 #t 0 'module-lang #f)
    (send trace syncheck:add-jump-to-definition/phase-level+space
          path 7 12 'exact path '(provider) 0)
    (define declaration-service (send trace get-declaration))
    (send declaration-service walk-stx #f)
    (check-equal? (send declaration-service module-binding-at 7)
                  (Module-Binding path '(provider) 0 'exact))
    (check-equal? (send declaration-service uses-at 7) (list (CharRange 7 12))))

  (test-case "explicit import variants keep exact full-token binding ranges"
    (define cases
      (list
        (list 'plain
              "#lang racket/base\n(require racket/list)\n(add-between '(1 2) 0)\n"
              (Pos 2 1) (Pos 2 12) (Pos 1 9))
        (list 'only
              (string-append "#lang racket/base\n"
                             "(require (only-in racket/list add-between))\n"
                             "(add-between '(1 2) 0)\n")
              (Pos 2 1) (Pos 2 12) (Pos 1 18))
        (list 'renamed
              (string-append "#lang racket/base\n"
                             "(require (rename-in racket/list "
                             "[add-between list-add-between]))\n"
                             "(list-add-between '(1 2) 0)\n")
              (Pos 2 1) (Pos 2 17) (Pos 1 39))
        (list 'prefixed
              (string-append "#lang racket/base\n"
                             "(require (prefix-in l: racket/list))\n"
                             "(l:add-between '(1 2) 0)\n")
              (Pos 2 1) (Pos 2 14) (Pos 1 20))))
    (for ([entry (in-list cases)])
      (match-define (list name text use-start use-end import-position) entry)
      (define uri (format "file:///tmp/~a-import-regression-test.rkt" name))
      (define d (make-doc uri text))
      (check-true (doc-expand! d))
      (check-equal? (doc-occurrence-at d use-start) (Range use-start use-end))
      (check-true (Module-Binding? (doc-module-binding-at d use-start)))
      (check-false (doc-occurrence-at d import-position))
      (check-false (doc-definition-at d import-position))))

  (test-case "doc-rename and doc-prepare-rename handle local and imported bindings"
    (define result (doc-rename default-doc default-uri (Pos 1 8) "y"))
    (define edits (hash-ref (WorkspaceEdit-changes result) (string->symbol default-uri)))
    (check-equal? edits
                  (list (TextEdit (Range (Pos 1 8) (Pos 1 9)) "y")
                        (TextEdit (Range (Pos 2 0) (Pos 2 1)) "y")))
    (check-false (doc-rename default-doc default-uri (Pos 1 1) "my-define"))
    (check-equal? (doc-prepare-rename default-doc (Pos 1 8)) (Range (Pos 1 8) (Pos 1 9)))
    (check-false (doc-prepare-rename default-doc (Pos 1 1)))))
