#lang racket/base

(module+ test
  (require "doc-test-support.rkt"
           rackunit
           racket/file
           racket/list
           racket/match
           racket/string
           "../../common/interfaces.rkt"
           "../../doclib/doc.rkt"
           "../../doclib/hover.rkt")

  (test-case "Hover card labels a mouse-over status without definition"
    (check-equal?
      (render-hover-card
        (Hover-Card #f #f #f (Hover-Annotation 'mouse-over-status "bound occurrence of count") #f))
      "**Mouse-over status**\n\nbound occurrence of count"))

  (test-case "Hover card renders the fixed slot stack"
    (check-equal?
      (render-hover-card
        (Hover-Card
          #f #f
          (Hover-Definition 'source (Hover-Code-Summary "(parse-config raw)" "racket"))
          #f
          (Hover-Documentation "Parse a configuration value." "https://docs.example.test/parse-config")))
#<<END
**Source**
```racket
(parse-config raw)
```

**Documentation** | [Online docs](https://docs.example.test/parse-config)

Parse a configuration value.
END
))

  (test-case "Hover card omits empty sections and docs separator"
    (check-equal?
      (render-hover-card
        (Hover-Card #f #f #f #f (Hover-Documentation "" "https://docs.example.test/parse-config")))
      "**Documentation** | [Online docs](https://docs.example.test/parse-config)"))

  (test-case "Hover card separates documentation body without an online link"
    (check-equal?
      (render-hover-card
        (Hover-Card #f #f #f #f (Hover-Documentation "Parse a configuration value." #f)))
      "**Documentation**\n\nParse a configuration value."))

  (test-case "Hover card labels a lone Rhombus source fence"
    (check-equal?
      (render-hover-card
        (Hover-Card #f #f (Hover-Definition 'source (Hover-Code-Summary "fun parse_config(raw): raw" "rhombus")) #f #f))
      "**Source**\n```rhombus\nfun parse_config(raw): raw\n```"))

  (test-case "Hover card labels definition when a type precedes it"
    (check-equal?
      (render-hover-card
        (Hover-Card (Hover-Code-Summary "Integer" "racket")
                    #t
                    (Hover-Definition 'source (Hover-Code-Summary "(define count 1)" "racket"))
                    #f #f))
      "**Type (stale)**\n```racket\nInteger\n```\n\n**Source**\n```racket\n(define count 1)\n```"))

  (test-case "build-hover-card keeps non-empty annotations and omits empty ones"
    (define kept
      (build-hover-card #:type-text "Integer"
                        #:type-stale? #f
                        #:annotation (Hover-Annotation 'mouse-over-status " \n Integer \t")
                        #:link #f #:signature #f #:source-summary #f #:documentation-text #f))
    (check-equal? (Hover-Card-annotation kept)
                  (Hover-Annotation 'mouse-over-status " \n Integer \t"))
    (check-equal? (render-hover-card kept)
                  "**Type**\n```racket\nInteger\n```\n\n**Mouse-over status**\n\n \n Integer \t")

    (define omitted
      (build-hover-card #:type-text #f #:type-stale? #f
                        #:annotation (Hover-Annotation 'log-tooltip "")
                        #:link #f #:signature #f #:source-summary #f #:documentation-text #f))
    (check-false (Hover-Card-annotation omitted)))

  (test-case "build-hover-card keeps annotations beside richer slots and standalone"
    (define with-type
      (build-hover-card #:type-text "Integer" #:type-stale? #f
                        #:annotation (Hover-Annotation 'mouse-over-status "bound occurrence")
                        #:link #f #:signature #f #:source-summary #f #:documentation-text #f))
    (check-equal? (render-hover-card with-type)
                  "**Type**\n```racket\nInteger\n```\n\n**Mouse-over status**\n\nbound occurrence")

    (define with-source
      (build-hover-card #:type-text #f #:type-stale? #f
                        #:annotation (Hover-Annotation 'mouse-over-status "1 bound occurrence")
                        #:link #f #:signature #f
                        #:source-summary (Hover-Code-Summary "(define count 1)" "racket")
                        #:documentation-text #f))
    (check-equal? (render-hover-card with-source)
                  "**Source**\n```racket\n(define count 1)\n```\n\n**Mouse-over status**\n\n1 bound occurrence")

    (define with-docs
      (build-hover-card #:type-text #f #:type-stale? #f
                        #:annotation (Hover-Annotation 'log-tooltip "imported from racket")
                        #:link "https://docs.example.test/map"
                        #:signature #f #:source-summary #f
                        #:documentation-text "Applies proc."))
    (check-equal? (render-hover-card with-docs)
                  "**Log tooltip**\n\nimported from racket\n\n**Documentation** | [Online docs](https://docs.example.test/map)\n\nApplies proc.")

    (define annotation-only
      (build-hover-card #:type-text #f #:type-stale? #f
                        #:annotation (Hover-Annotation 'mouse-over-status "bound occurrence")
                        #:link #f #:signature #f #:source-summary #f #:documentation-text #f))
    (check-equal? (render-hover-card annotation-only)
                  "**Mouse-over status**\n\nbound occurrence"))

  (test-case "build-hover-card definition precedence and stale type flag"
    (define typed-signature
      (build-hover-card #:type-text "Integer" #:type-stale? #f
                        #:annotation #f #:link #f
                        #:signature "(add1 z) -> number?"
                        #:source-summary #f #:documentation-text #f))
    (check-equal? (render-hover-card typed-signature)
                  "**Type**\n```racket\nInteger\n```\n\n**Signature**\n```racket\n(add1 z) -> number?\n```")

    (define source-over-signature
      (build-hover-card #:type-text #f #:type-stale? #f
                        #:annotation #f #:link #f
                        #:signature "(add1 z) -> number?"
                        #:source-summary (Hover-Code-Summary "(define count 1)" "racket")
                        #:documentation-text #f))
    (check-equal? (Hover-Definition-kind (Hover-Card-definition source-over-signature)) 'source)
    (check-equal? (render-hover-card source-over-signature)
                  "**Source**\n```racket\n(define count 1)\n```")

    (for ([signature (in-list '(#f "(count) -> integer?"))])
      (define card
        (build-hover-card #:type-text "Integer" #:type-stale? #f
                          #:annotation #f
                          #:link "https://example.com/count"
                          #:signature signature
                          #:source-summary (Hover-Code-Summary "(define count 1)" "racket")
                          #:documentation-text "Returns the count."))
      (check-not-false (Hover-Card-documentation card))
      (check-equal? (and (Hover-Card-definition card)
                         (Hover-Definition-kind (Hover-Card-definition card)))
                    (and signature 'signature))
      (check-false (string-contains? (render-hover-card card) "**Source**")))

    (define lone-signature
      (build-hover-card #:type-text #f #:type-stale? #f
                        #:annotation #f #:link #f
                        #:signature "(add1 z) -> number?"
                        #:source-summary #f #:documentation-text #f))
    (check-equal? (render-hover-card lone-signature)
                  "**Signature**\n```racket\n(add1 z) -> number?\n```")

    (check-false
      (Hover-Card-type-stale?
        (build-hover-card #:type-text #f #:type-stale? #t
                          #:annotation (Hover-Annotation 'mouse-over-status "bound occurrence")
                          #:link #f #:signature #f #:source-summary #f #:documentation-text #f)))
    (check-true
      (Hover-Card-type-stale?
        (build-hover-card #:type-text "Integer" #:type-stale? #t
                          #:annotation #f #:link #f #:signature #f
                          #:source-summary #f #:documentation-text #f))))

  (test-case "Document hover renders a docs-backed card"
    (define d (make-doc "file:///tmp/hover-test.rkt" "#lang racket\n(list)"))
    (check-true (doc-expand! d))
    (define h (doc-hover d (Pos 1 1)))
    (check-not-false h)
    (define result (Hover-contents h))
    (check-false (string-contains? result "**Source**"))
    (check-true (string-contains? result "```racket"))
    (check-true (string-contains? result "Returns a newly allocated list"))
    (check-true (string-contains? result "[Online docs]"))
    (check-true (string-contains? result "imported from"))
    (define imported-from-start (caar (regexp-match-positions #rx"imported from" result)))
    (define online-docs-start (caar (regexp-match-positions #rx"\\[Online docs\\]" result)))
    (check-true (< imported-from-start online-docs-start)))

  (test-case "Typed Racket hover renders inferred types, source, ranges, and stale state"
    (when (typed-racket-available?)
      (define d
        (make-doc
          "file:///tmp/typed-racket-hover-test.rkt"
          (string-append "#lang typed/racket\n"
                         "(: count Integer)\n"
                         "(define count 1)\n"
                         "(+ count 2)\n")))
      (check-true (doc-expand! d))
      (define use-hover (doc-hover d (Pos 3 4)))
      (check-not-false use-hover)
      (check-equal? (Hover-range use-hover) (Range (Pos 3 3) (Pos 3 8)))
      (check-equal?
        (Hover-contents use-hover)
        "**Type**\n```racket\nInteger\n```\n\n**Source**\n```racket\n(define count 1)\n```")

      (define open-paren-hover (doc-hover d (Pos 3 0)))
      (check-equal? (Hover-range open-paren-hover) (Range (Pos 3 0) (Pos 3 1)))
      (check-true (string-contains? (Hover-contents open-paren-hover) "**Type**"))
      (define literal-hover (doc-hover d (Pos 3 9)))
      (check-equal? (Hover-range literal-hover) (Range (Pos 3 9) (Pos 3 10)))
      (check-true (string-contains? (Hover-contents literal-hover) "**Type**"))
      (check-equal? (Hover-range (doc-hover d (Pos 3 10))) (Range (Pos 3 10) (Pos 3 11)))

      (doc-apply-edit! d (Range (Pos 3 9) (Pos 3 10)) "3")
      (doc-update-version! d 1)
      (define stale-hover (doc-hover d (Pos 3 4)))
      (check-not-false stale-hover)
      (check-true (string-prefix? (Hover-contents stale-hover) "**Type (stale)**"))))

  (test-case "Typed Racket hover keeps shifted type ranges while a trace is stale"
    (when (typed-racket-available?)
      (define d
        (make-doc
          "file:///tmp/typed-racket-stale-shift-test.rkt"
          "#lang typed/racket\n(: count Integer)\n(define count 1)\n(+ count 2)\n"))
      (check-true (doc-expand! d))
      (doc-apply-edit! d (Range (Pos 3 0) (Pos 3 0)) "    ")
      (doc-update-version! d 1)
      (define shifted-hover (doc-hover d (Pos 3 8)))
      (check-not-false shifted-hover)
      (check-equal? (Hover-range shifted-hover) (Range (Pos 3 7) (Pos 3 12)))
      (check-true (string-prefix? (Hover-contents shifted-hover) "**Type (stale)**"))
      (check-true (string-contains? (Hover-contents shifted-hover) "(define count 1)"))))

  (test-case "Typed Racket import hover shows type before signature"
    (when (typed-racket-available?)
      (define d (make-doc "file:///tmp/typed-racket-import-hover-test.rkt"
                          "#lang typed/racket\n(add1 1)\n"))
      (check-true (doc-expand! d))
      (define hover (doc-hover d (Pos 1 1)))
      (check-not-false hover)
      (define contents (Hover-contents hover))
      (define type-pos (car (regexp-match-positions #rx"\\*\\*Type\\*\\*" contents)))
      (define signature-pos (car (regexp-match-positions #rx"\\*\\*Signature\\*\\*" contents)))
      (check-true (< (car type-pos) (car signature-pos)))
      (check-true (string-contains? contents "```racket"))
      (check-false (string-contains? contents "**Source**"))
      (check-true (string-contains? contents "imported from"))))

  (test-case "Untyped documents do not gain Typed Racket type cards"
    (define d (make-doc "file:///tmp/untyped-no-type-card-test.rkt"
                        "#lang racket\n(define count 1)\n(+ count 2)\n"))
    (check-true (doc-expand! d))
    (define hover (doc-hover d (Pos 2 4)))
    (check-not-false hover)
    (check-false (string-contains? (Hover-contents hover) "**Type**"))
    (check-true (string-contains? (Hover-contents hover) "```racket")))

  (test-case "Document hover renders collapsed same-file headers for declarations and uses"
    (define text
#<<END
#lang racket
(let ([count (length (list 1))])
  (+ count 1))
(define (parse-config raw) raw)
(for/list ([element (list 1)]) element)
(parse-config "input")
END
      )
    (define d (make-doc "file:///tmp/hover-detail.rkt" text))
    (check-true (doc-expand! d))

    (define use-hover (doc-hover d (Pos 2 5)))
    (check-not-false use-hover)
    (check-equal? (Hover-range use-hover) (Range (Pos 2 5) (Pos 2 10)))
    (check-equal? (Hover-contents use-hover)
                  "**Source**\n```racket\n(let ([count (length (list 1))])\n  ...\n```")
    (check-equal? (Hover-contents (doc-hover d (Pos 3 9)))
                  "**Source**\n```racket\n(define (parse-config raw) raw)\n```\n\n**Mouse-over status**\n\n1 bound occurrence")
    (check-equal? (Hover-contents (doc-hover d (Pos 5 2)))
                  "**Source**\n```racket\n(define (parse-config raw) raw)\n```")
    (check-equal? (Hover-contents (doc-hover d (Pos 4 12)))
                  "**Source**\n```racket\n(for/list ([element (list 1)]) element)\n```\n\n**Mouse-over status**\n\n1 bound occurrence"))

  (test-case "Document hover balances compact clauses with collapsed headers"
    (define d
      (make-doc
        "file:///tmp/hover-detail-header.rkt"
        "#lang racket\n(let ([count (length (list 1))]\n      [limit 10])\n  (+ count limit))\n(define (fib\n         n)\n  n)\n(define answer (string-length \"input\"))\n(define (parse raw) ; accepts overrides\n  raw)\n"))
    (check-true (doc-expand! d))
    (check-equal? (Hover-contents (doc-hover d (Pos 2 7)))
                  "**Source**\n```racket\n[limit 10]\n```\n\n**Mouse-over status**\n\n1 bound occurrence")
    (check-equal? (Hover-contents (doc-hover d (Pos 5 9)))
                  "**Source**\n```racket\n(fib\n n)\n```\n\n**Mouse-over status**\n\n1 bound occurrence")
    (check-equal? (Hover-contents (doc-hover d (Pos 7 8)))
                  "**Source**\n```racket\n(define answer (string-length \"input\"))\n```\n\n**Mouse-over status**\n\nno bound occurrences")
    (check-equal? (Hover-contents (doc-hover d (Pos 8 9)))
                  "**Source**\n```racket\n(define (parse raw) ; accepts overrides\n  ...\n```\n\n**Mouse-over status**\n\nno bound occurrences"))

  (test-case "Document hover renders leading comments and respects comment boundaries"
    (define d
      (make-doc
        "file:///tmp/hover-detail-comments.rkt"
        "#lang racket\n;; Produces the next Fibonacci value.\n;; Kept separate from callers for reuse.\n(define (fib value)\n  value)\n(let (\n      ;; Counts visits in this branch.\n      [count 1])\n  count)\n"))
    (check-true (doc-expand! d))
    (check-equal?
      (Hover-contents (doc-hover d (Pos 3 9)))
      "**Source**\n```racket\n;; Produces the next Fibonacci value.\n;; Kept separate from callers for reuse.\n(define (fib value)\n  ...\n```\n\n**Mouse-over status**\n\nno bound occurrences")
    (check-equal?
      (Hover-contents (doc-hover d (Pos 7 7)))
      "**Source**\n```racket\n;; Counts visits in this branch.\n[count 1]\n```\n\n**Mouse-over status**\n\n1 bound occurrence")

    (define boundary-doc
      (make-doc
        "file:///tmp/hover-detail-comment-boundaries.rkt"
        "#lang racket\n;; Separated from the declaration.\n\n(define (separated value) value)\n(displayln 1) ; Explains the display call.\n(define (trailing value) value)\n"))
    (check-true (doc-expand! boundary-doc))
    (check-false (string-contains? (Hover-contents (doc-hover boundary-doc (Pos 3 9)))
                                   "Separated from"))
    (check-false (string-contains? (Hover-contents (doc-hover boundary-doc (Pos 5 9)))
                                   "Explains the display")))

  (test-case "Document hover bounds leading comment blocks independently from code"
    (define long-comment-line (string-append ";; " (make-string 205 #\x)))
    (define comment-lines
      (append (list long-comment-line)
              (for/list ([index (in-range 10)])
                (format ";; Extra comment line ~a" index))))
    (define d
      (make-doc
        "file:///tmp/hover-detail-long-comments.rkt"
        (string-append "#lang racket\n"
                       (string-join comment-lines "\n")
                       "\n(define (documented value) value)\n")))
    (check-true (doc-expand! d))
    (define contents (Hover-contents (doc-hover d (Pos 12 9))))
    (check-true (string-contains? contents (string-append ";; " (make-string 197 #\x) "...")))
    (check-true (string-contains? contents ";; Extra comment line 8"))
    (check-false (string-contains? contents ";; Extra comment line 9"))
    (check-true (string-contains? contents "...\n(define (documented value) value)")))

  (test-case "Document hover renders collapsed binding header and complete struct forms"
    (define d
      (make-doc
        "file:///tmp/hover-detail-context.rkt"
        "#lang racket\n(define (format-lines original-lines formatted-lines start-ln end-ln)\n  (for/list ([original-line (in-list original-lines)]\n             [formatted-line (in-list formatted-lines)]\n             [ln (in-naturals)]\n             #:break (> ln end-ln)\n             #:when (and (<= start-ln ln end-ln)\n                         (not (string=? original-line formatted-line))))\n    (list ln formatted-line)))\n"))
    (check-true (doc-expand! d))
    (check-equal?
      (Hover-contents (doc-hover d (Pos 4 14)))
      "**Source**\n```racket\n[ln (in-naturals)]\n```\n\n**Mouse-over status**\n\n3 bound occurrences")

    (define struct-doc
      (make-doc
        "file:///tmp/hover-detail-struct.rkt"
        "#lang racket\n(struct RopeNode\n  (left\n   right\n   chars\n   newlines\n   height)\n  #:transparent)\n"))
    (check-true (doc-expand! struct-doc))
    (check-equal?
      (Hover-contents (doc-hover struct-doc (Pos 1 8)))
      "**Source**\n```racket\n(struct RopeNode\n  (left\n   right\n   chars\n   newlines\n   height)\n  #:transparent)\n```\n\n**Mouse-over status**\n\nno bound occurrences"))

  (test-case "Document hover dedents nested source, comments, tabs, and literals"
    (for ([spec (in-list
                  (list
                    (list "file:///tmp/hover-detail-indent.rkt"
                          (string-append "#lang racket\n(let ()\n"
                                         "    ;; A nested structure.\n"
                                         "  (struct Nested\n    (left\n     right)\n    #:transparent)\n"
                                         "  Nested)\n")
                          (Pos 3 11)
                          "```racket\n  ;; A nested structure.\n(struct Nested\n  (left\n   right)\n  #:transparent)\n```")
                    (list "file:///tmp/hover-detail-indent-minimum.rkt"
                          "#lang racket\n(define (combine\n \n    first\n  second)\n  (list first second))\n"
                          (Pos 3 5)
                          "```racket\n      (combine\n\n  first\nsecond)\n```")
                    (list "file:///tmp/hover-detail-indent-marker.rkt"
                          "#lang racket\n(let ()\n    (define (identity value)\n      value)\n    identity)\n"
                          (Pos 4 5)
                          "```racket\n(define (identity value)\n  ...\n```")
                    (list "file:///tmp/hover-detail-indent-tabs.rkt"
                          "#lang racket\n(define (tabbed\n\t  arg)\n  arg)\n"
                          (Pos 2 3)
                          "```racket\n     (tabbed\narg)\n```")
                    (list "file:///tmp/hover-detail-indent-literal.rkt"
                          (string-append "#lang racket\n(let ()\n"
                                         "  (define\n    message\n    \"first\n      second\")\n"
                                         "  message)\n")
                          (Pos 6 3)
                          "```racket\n(define\n  message\n  \"first\n    second\")\n```")))])
      (match-define (list uri text pos expected) spec)
      (define d (make-doc uri text))
      (check-true (doc-expand! d))
      (check-true (string-contains? (Hover-contents (doc-hover d pos)) expected)))

    (define d6
      (make-doc
        "file:///tmp/hover-detail-indent-stale-comment.rkt"
        "#lang racket\n  ;; alpha beta\n  (define value 1)\n  value\n"))
    (check-true (doc-expand! d6))
    (doc-apply-edit! d6 (Range (Pos 1 10) (Pos 1 10)) "\n  ;;")
    (doc-update-version! d6 1)
    (check-true
      (string-contains?
        (Hover-contents (doc-hover d6 (Pos 4 3)))
        "```racket\n;; alpha\n;; beta\n(define value 1)\n```")))

  (test-case "Document hover keeps cross-file workspace bindings out of source detail"
    (define temp-dir (make-temporary-file "hover-detail~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (define helper-path (build-path temp-dir "helper.rkt"))
        (display-to-file "#lang racket\n(provide shared-value)\n(define shared-value 1)\n"
                         helper-path
                         #:exists 'truncate)
        (define main-path (build-path temp-dir "main.rkt"))
        (define d
          (make-doc (string-append "file://" (path->string main-path))
                    "#lang racket\n(require \"helper.rkt\")\nshared-value\n"))
        (check-true (doc-expand! d))
        (define imported-hover (doc-hover d (Pos 2 1)))
        (check-not-false imported-hover)
        (check-true (string-contains? (Hover-contents imported-hover) "imported from")))
      (lambda () (delete-directory/files temp-dir))))

  (test-case "Document hover renders stale and shifted retained source detail"
    (define d1
      (make-doc "file:///tmp/hover-detail-stale.rkt"
                "#lang racket\n(let ([count 1]) (+ count 1))\n"))
    (check-true (doc-expand! d1))
    (doc-apply-edit! d1 (Range (Pos 1 13) (Pos 1 14)) "2")
    (doc-update-version! d1 1)
    (check-equal? (Hover-contents (doc-hover d1 (Pos 1 22)))
                  "**Source**\n```racket\n(let ([count 2]) (+ count 1))\n```")
    (define declaration-hover (doc-hover d1 (Pos 1 7)))
    (check-not-false declaration-hover)
    (check-true (string-contains? (Hover-contents declaration-hover) "(let ([count 2])"))

    (define d2
      (make-doc "file:///tmp/hover-detail-shifted.rkt"
                "#lang racket\n(let ([count 1]) (+ count 1))\n"))
    (check-true (doc-expand! d2))
    (doc-apply-edit! d2 (Range (Pos 1 0) (Pos 1 0)) "  ")
    (doc-update-version! d2 1)
    (check-equal? (Hover-contents (doc-hover d2 (Pos 1 24)))
                  "**Source**\n```racket\n(let ([count 1]) (+ count 1))\n```")
    (check-true (string-contains? (Hover-contents (doc-hover d2 (Pos 1 9))) "(let ([count 1])")))

  (test-case "Document hover truncates source forms by characters and lines"
    (define long-line (string-append "\"" (make-string 600 #\x) "\""))
    (define short-lines (string-join (make-list 12 "  1") "\n"))
    (define text
      (string-append "#lang racket\n"
                     "(let ([value (list " long-line "\n"
                     short-lines
                     "\n)]) value)\n"))
    (define d (make-doc "file:///tmp/hover-detail-truncate.rkt" text))
    (check-true (doc-expand! d))
    (define result (Hover-contents (doc-hover d (Pos 1 7))))
    (check-true (string-prefix? result "**Source**\n```racket\n"))
    (define match (regexp-match #px"```racket\n((?:.|\n)*)\n```" result))
    (check-not-false match)
    (define code (second match))
    (check-true (<= (string-length code) 1004))
    (check-true (<= (add1 (length (regexp-match-positions* #px"\n" code))) 11))
    (check-true (string-suffix? code "\n  ...")))

  (test-case "Document hover renders Rhombus structural forms when Rhombus is available"
    (define rhombus-available?
      (with-handlers ([exn:fail? (lambda (_exn) #f)])
        (dynamic-require 'rhombus/main #f)
        #t))
    (when rhombus-available?
      (define d
        (make-doc "file:///tmp/hover-detail.rhm"
                  "#lang rhombus\ndef value = 1\nfun parse_value(raw): raw\n"))
      (check-true (doc-expand! d))
      (check-equal? (Hover-contents (doc-hover d (Pos 1 4)))
                    "**Source**\n```rhombus\ndef value = 1\n```\n\n**Mouse-over status**\n\nno bound occurrences")
      (check-equal? (Hover-contents (doc-hover d (Pos 2 4)))
                    "**Source**\n```rhombus\nfun parse_value(raw): raw\n```\n\n**Mouse-over status**\n\nno bound occurrences")

      (define comment-d
        (make-doc "file:///tmp/hover-detail-comments.rhm"
                  "#lang rhombus\n// Converts the value.\nfun documented(value): value\n"))
      (check-true (doc-expand! comment-d))
      (check-equal? (Hover-contents (doc-hover comment-d (Pos 2 4)))
                    "**Source**\n```rhombus\n// Converts the value.\nfun documented(value): value\n```\n\n**Mouse-over status**\n\nno bound occurrences")

      (define use-d
        (make-doc "file:///tmp/hover-detail-use.rhm"
                  "#lang rhombus\nfun parse_value(raw): raw\nparse_value(1)\n"))
      (check-true (doc-expand! use-d))
      (check-equal? (Hover-contents (doc-hover use-d (Pos 2 0)))
                    "**Source**\n```rhombus\nfun parse_value(raw): raw\n```"))))
