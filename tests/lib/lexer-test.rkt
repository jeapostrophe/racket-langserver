#lang racket

(module+ test
  (require rackunit
           "../../doclib/doc.rkt"
           "../../doclib/lexer.rkt"
           (only-in "../../doclib/lexer/snapshot.rkt"
                    make-lexer-span
                    find-token-index-at
                    find-token-index-at-or-before
                    find-token-index-at-or-after)
           (only-in "../../doclib/lexer/state.rkt"
                    build-snapshot-token-forest
                    lexer-state-body-forest)
           "../../doclib/lexer/token-tree.rkt"
           "../../doclib/lexer/tree-query.rkt"
           "../../common/interfaces.rkt")

  (define (entry-summary entry)
    (list (LexerEntry-start entry)
          (LexerEntry-end entry)
          (LexerEntry-text entry)
          (LexerEntry-type entry)))

  (define (snapshot-summaries text)
    (for/list ([entry (in-lexer-snapshot (build-lexer-snapshot text))])
      (entry-summary entry)))

  (define (snapshot-entries text)
    (for/list ([entry (in-lexer-snapshot (build-lexer-snapshot text))])
      entry))

  (define (match-range rx text)
    (match (regexp-match-positions rx text)
      [(list (cons start end)) (CharRange start end)]))

  (test-case
    "find-token-index-at-or-before returns false before the first token"
    (define tokens
      (vector (make-lexer-span 2 4 'symbol)
              (make-lexer-span 6 8 'symbol)))
    (check-false (find-token-index-at-or-before tokens 1))
    (check-equal? (find-token-index-at-or-before tokens 2) 0)
    (check-equal? (find-token-index-at-or-before tokens 5) 0)
    (check-equal? (find-token-index-at-or-before tokens 9) 1))

  (test-case
    "find-token-index-at-or-after returns false after the last token"
    (define tokens
      (vector (make-lexer-span 2 4 'symbol)
              (make-lexer-span 6 8 'symbol)))
    (check-equal? (find-token-index-at-or-after tokens 1) 0)
    (check-equal? (find-token-index-at-or-after tokens 2) 0)
    (check-equal? (find-token-index-at-or-after tokens 5) 1)
    (check-false (find-token-index-at-or-after tokens 8)))

  (test-case
    "find-token-index-at returns false outside token spans"
    (define tokens
      (vector (make-lexer-span 2 4 'symbol)
              (make-lexer-span 6 8 'symbol)))
    (check-false (find-token-index-at tokens 1))
    (check-equal? (find-token-index-at tokens 2) 0)
    (check-false (find-token-index-at tokens 4))
    (check-false (find-token-index-at tokens 5))
    (check-equal? (find-token-index-at tokens 7) 1)
    (check-false (find-token-index-at tokens 8)))

  (test-case
    "build-lexer-snapshot enumerates public lexer entries"
    (define snapshot (build-lexer-snapshot "#lang racket\n(define x 1)\n"))
    (define entries (for/list ([entry (in-lexer-snapshot snapshot)]) entry))
    ;; Cached lexer positions are normalized to the doc's 0-based offsets.
    (check-not-false (member (list 13 14 "(" 'open-paren)
                             (map entry-summary entries)))
    (check-not-false (member (list 14 20 "define" 'symbol)
                             (map entry-summary entries)))
    (check-not-false (member (list 20 21 " " 'white-space)
                             (map entry-summary entries)))
    (check-not-false (member (list 21 22 "x" 'symbol)
                             (map entry-summary entries)))
    (check-not-false (member (list 23 24 "1" 'constant)
                             (map entry-summary entries)))
    (check-not-false (member (list 24 25 ")" 'close-paren)
                             (map entry-summary entries))))

  (test-case
    "lexer snapshot normalizes a leading #lang directive"
    (check-equal?
      (take (snapshot-summaries "#lang racket/base\n(define x 1)\n") 2)
      (list (list 0 17 "#lang racket/base" 'lang-directive)
            (list 17 18 "\n" 'white-space))))

  (test-case
    "lexer snapshot preserves invalid #lang lines as errors"
    (define unknown-language-summary
      (first (snapshot-summaries "#lang not-a-real-language\n(define x 1)\n")))
    (check-equal? (list-ref unknown-language-summary 2)
                  "#lang not-a-real-language")
    (check-equal? (list-ref unknown-language-summary 3)
                  'error)
    (define malformed-reader-summary
      (first (snapshot-summaries "#lang reader (foo)\n1\n")))
    (check-equal? (list-ref malformed-reader-summary 2)
                  "#lang reader (foo)")
    (check-equal? (list-ref malformed-reader-summary 3)
                  'error))

  (test-case
    "lexer snapshot normalizes #lang reader without collapsing the payload"
    (check-equal?
      (take (snapshot-summaries "#lang reader syntax/module-reader\nracket/base\n") 2)
      (list (list 0 33 "#lang reader syntax/module-reader" 'lang-directive)
            (list 33 34 "\n" 'white-space))))

  (test-case
    "lexer snapshot keeps a leading #reader token as one big token"
    (check-equal?
      (take (snapshot-summaries "#reader scribble/reader\n@title{demo}\n") 2)
      (list (list 0 7 "#reader" 'reader-directive)
            (list 7 8 " " 'white-space))))

  (test-case
    "lexer snapshot keeps a block comment as a comment token"
    (check-equal?
      (take (snapshot-summaries "#| block |#\n") 2)
      (list (list 0 11 "#| block |#" 'comment)
            (list 11 12 "\n" 'white-space))))

  (test-case
    "lexer snapshot keeps shebang comments as comment tokens"
    (check-equal?
      (take (snapshot-summaries "#! /bin/sh\n") 2)
      (list (list 0 10 "#! /bin/sh" 'comment)
            (list 10 11 "\n" 'white-space)))
    (check-equal?
      (take (snapshot-summaries "#!/usr/bin/env racket\n") 2)
      (list (list 0 21 "#!/usr/bin/env racket" 'comment)
            (list 21 22 "\n" 'white-space))))

  (test-case
    "lexer snapshot normalizes quote-family prefixes"
    (define prefix-entries
      (filter (lambda (summary)
                (not (string=? (list-ref summary 2) " ")))
              (snapshot-summaries "' ` , ,@ #; x")))
    (check-equal?
      prefix-entries
      (list (list 0 1 "'" 'quote)
            (list 2 3 "`" 'quasiquote)
            (list 4 5 "," 'unquote)
            (list 6 8 ",@" 'unquote-splicing)
            (list 9 11 "#;" 'sexp-comment)
            (list 12 13 "x" 'symbol))))

  (test-case
    "lexer snapshot normalizes syntax quote-family prefixes"
    (check-equal?
      (filter (lambda (summary)
                (not (string=? (list-ref summary 2) " ")))
              (snapshot-summaries "#' #` #, #,@ x"))
      (list (list 0 2 "#'" 'syntax-quote)
            (list 3 5 "#`" 'syntax-quasiquote)
            (list 6 8 "#," 'syntax-unquote)
            (list 9 12 "#,@" 'syntax-unquote-splicing)
            (list 13 14 "x" 'symbol))))

  (test-case
    "token tree parses syntax quote-family prefixes as prefix nodes"
    (for ([prefix-type (in-list '(syntax-quote
                                   syntax-quasiquote
                                   syntax-unquote
                                   syntax-unquote-splicing))])
      (define spans
        (vector (make-lexer-span 0 2 prefix-type)
                (make-lexer-span 2 3 'white-space)
                (make-lexer-span 3 4 'symbol)))
      (define-values (node next-index)
        (parse-token-node spans 0))
      (check-true (Token-Prefix-Tree? node))
      (check-equal? (LexerTokenSpan-type (Token-Prefix-Tree-prefix-span node))
                    prefix-type)
      (check-equal?
        (LexerTokenSpan-type
          (Token-Leaf-span (first (Token-Prefix-Tree-skippable-nodes node))))
        'white-space)
      (check-true (Token-Leaf? (Token-Prefix-Tree-child node)))
      (check-equal? (Token-Prefix-Tree-end node) 4)
      (check-equal? next-index 3)))

  (test-case
    "token tree end is cached for unclosed lists"
    (define-values (non-empty-node non-empty-next-index)
      (parse-token-node
        (vector (make-lexer-span 0 1 'open-paren)
                (make-lexer-span 1 7 'symbol)
                (make-lexer-span 7 8 'white-space)
                (make-lexer-span 8 13 'symbol))
        0))
    (check-equal? non-empty-next-index 4)
    (check-equal? (token-node-end non-empty-node) 13)
    (check-equal? (Token-List-end non-empty-node) 13)

    (define-values (empty-node empty-next-index)
      (parse-token-node
        (vector (make-lexer-span 0 1 'open-paren))
        0))
    (check-equal? empty-next-index 1)
    (check-equal? (token-node-end empty-node) 1)
    (check-equal? (Token-List-end empty-node) 1))

  (test-case
    "token tree skips leading trivia and sexp comments"
    (define spans
      (vector (make-lexer-span 0 1 'white-space)
              (make-lexer-span 1 2 'comment)
              (make-lexer-span 2 4 'sexp-comment)
              (make-lexer-span 4 5 'white-space)
              (make-lexer-span 5 6 'symbol)))
    (define-values (nodes next-index)
      (parse-skippable-node spans 0))
    (check-equal? next-index 5)
    (check-equal? (length nodes) 3)
    (check-true (Token-Leaf? (first nodes)))
    (check-true (token-leaf-type? (first nodes) 'white-space))
    (check-true (Token-Leaf? (second nodes)))
    (check-true (token-leaf-type? (second nodes) 'comment))
    (check-true (Token-Prefix-Tree? (third nodes)))
    (check-equal? (LexerTokenSpan-type
                    (Token-Prefix-Tree-prefix-span (third nodes)))
                  'sexp-comment))

  (test-case
    "snapshot token forest is built separately from flat snapshot"
    (define snapshot (build-lexer-snapshot "(outer (inner x))"))
    (define forest (build-snapshot-token-forest (LexerSnapshot-text snapshot)
                                                #f
                                                (LexerSnapshot-tokens snapshot)))
    (define inner-list (token-forest-deepest-enclosing-list forest 9))
    (check-true (Token-List? inner-list))
    (check-equal? (LexerTokenSpan-start (Token-List-open-span inner-list)) 7)
    (let-values ([(parent path) (token-node-parent/path forest inner-list)])
      (check-true (Token-List? parent))
      (check-equal? (LexerTokenSpan-start (Token-List-open-span parent)) 0)
      (check-equal? (length path) 2)))

  (test-case
    "token forest keeps sexp comment ranges"
    (define text "#lang racket\n#; (define x 1)\n(+ 1 2)\n")
    (define snapshot (build-lexer-snapshot text))
    (define forest (build-snapshot-token-forest (LexerSnapshot-text snapshot)
                                                #f
                                                (LexerSnapshot-tokens snapshot)))
    (check-equal?
      (token-forest-sexp-comment-spans forest)
      (list (match-range #px"#; \\(define x 1\\)" text))))

  (test-case
    "token forest does not parse non-sexp body"
    (define text "#lang scribble/manual\n@section{Hi}\n")
    (define snapshot (build-lexer-snapshot text "file:///tmp/demo.scrbl"))
    (define forest (build-snapshot-token-forest (LexerSnapshot-text snapshot)
                                                "file:///tmp/demo.scrbl"
                                                (LexerSnapshot-tokens snapshot)))
    (define header-end (CharRange-end (match-range #px"#lang scribble/manual" text)))
    (check-false
      (for/or ([node (in-list (token-forest-flattened-nodes forest))])
        (> (token-node-end node) header-end)))
    ;; The truncated forest contains no lists, so tree queries return #f.
    (check-false (token-forest-deepest-enclosing-list forest 28)))

  (test-case
    "parse-token-forest respects start-idx and excludes earlier tokens"
    (define spans (vector (make-lexer-span 0 1 'open-paren)
                          (make-lexer-span 1 2 'symbol)
                          (make-lexer-span 2 3 'close-paren)
                          (make-lexer-span 3 4 'symbol)))
    (define forest0 (parse-token-forest spans))
    (check-equal? (length (Token-Forest-nodes forest0)) 2)
    (define forest1 (parse-token-forest spans 3))
    (check-equal? (length (Token-Forest-nodes forest1)) 1)
    (define leaf (first (Token-Forest-nodes forest1)))
    (check-true (Token-Leaf? leaf))
    (check-true (token-leaf-type? leaf 'symbol)))

  (test-case
    "build-snapshot-token-forest for sexp docs starts after language header"
    (define text "#lang racket\n(define x 1)\n")
    (define snapshot (build-lexer-snapshot text))
    (define forest (build-snapshot-token-forest (LexerSnapshot-text snapshot)
                                                #f
                                                (LexerSnapshot-tokens snapshot)))
    ;; The #lang token should not appear in the body forest.
    (check-false
      (for/or ([node (in-list (token-forest-flattened-nodes forest))])
        (<= (token-node-start node) 0 (token-node-end node))))
    ;; The body form (define x 1) should be present.
    (check-not-false
      (token-forest-deepest-enclosing-list forest 15)))

  (test-case
    "build-snapshot-token-forest keeps first form without a language header"
    (define text "(first x)\n(second y)\n")
    (define snapshot (build-lexer-snapshot text))
    (define forest (build-snapshot-token-forest (LexerSnapshot-text snapshot)
                                                #f
                                                (LexerSnapshot-tokens snapshot)))
    (check-equal?
      (for/list ([node (in-list (Token-Forest-nodes forest))]
                 #:when (Token-List? node))
        (token-node-span node))
      (list (match-range #px"\\(first x\\)" text)
            (match-range #px"\\(second y\\)" text))))

  (test-case
    "build-snapshot-token-forest keeps raw module body in sexp docs"
    (define text "(module demo racket/base (define x 1))\n")
    (define snapshot (build-lexer-snapshot text))
    (define forest (build-snapshot-token-forest (LexerSnapshot-text snapshot)
                                                #f
                                                (LexerSnapshot-tokens snapshot)))
    (define body-range (match-range #px"\\(define x 1\\)" text))
    (check-not-false
      (token-forest-deepest-enclosing-list forest
                                           (CharRange-start body-range))))

  (test-case
    "lexer-state-body-forest caches result"
    (define text "#lang racket\n(define x 1)\n")
    (define state (build-lexer-state text #f))
    (define first (lexer-state-body-forest state))
    (define second (lexer-state-body-forest state))
    (check-eq? first second))

  (test-case
    "flat token queries work on non-sexp documents without a meaningful body forest"
    (define text "#lang scribble/manual\n@section{Hi}\n")
    (define snapshot (build-lexer-snapshot text "file:///tmp/demo.scrbl"))
    ;; token-at and symbol-at are flat queries: they should work even when
    ;; the document body is not sexp.
    (check-equal?
      (LexerEntry-text (lexer-snapshot-token-at snapshot 6))
      "#lang scribble/manual")
    (check-equal?
      (LexerEntry-type (lexer-snapshot-token-at snapshot 6))
      'lang-directive)
    (check-false (lexer-snapshot-symbol-at snapshot 6))
    ;; `section` is a symbol token in the body.
    (check-equal?
      (LexerEntry-text (lexer-snapshot-symbol-at snapshot 23))
      "section")
    (check-equal?
      (LexerEntry-type (lexer-snapshot-symbol-at snapshot 23))
      'symbol))

  (test-case
    "unknown-language body mode is unknown"
    (define text "#lang not-a-real-language\n(define x 1)\n")
    (define snapshot (build-lexer-snapshot text))
    (define policy (lexer-language-policy (LexerSnapshot-text snapshot)
                                          (LexerSnapshot-tokens snapshot)))
    (check-equal? (Language-Policy-body-mode policy) 'unknown))

  (test-case
    "unknown-language still builds a forest for editor affordances"
    (define text "#lang not-a-real-language\n(define x 1)\n")
    (define state (build-lexer-state text #f))
    (check-not-false (lexer-state-body-forest state))
    (check-equal?
      (token-forest-sexp-comment-spans
        (lexer-state-body-forest state))
      '()))

  (test-case
    "unknown #reader and raw modules also produce unknown language policy"
    (define reader-text "#reader does/not/exist\nbody\n")
    (define reader-snapshot (build-lexer-snapshot reader-text))
    (define reader-policy (lexer-language-policy (LexerSnapshot-text reader-snapshot)
                                                 (LexerSnapshot-tokens reader-snapshot)))
    (check-equal? (Language-Policy-body-mode reader-policy) 'unknown)

    (define module-text "(module demo does/not/exist (define x 1))\n")
    (define module-snapshot (build-lexer-snapshot module-text))
    (define module-policy (lexer-language-policy (LexerSnapshot-text module-snapshot)
                                                 (LexerSnapshot-tokens module-snapshot)))
    (check-equal? (Language-Policy-body-mode module-policy) 'unknown))

  (test-case
    "lexer snapshot position queries return token and symbol entries"
    (define snapshot
      (build-lexer-snapshot "#lang racket/base\n(define greeting \"hello\")\n"))
    (define symbol (lexer-snapshot-symbol-at snapshot 21))
    (check-true (LexerEntry? symbol))
    (check-equal? (list (LexerEntry-start symbol)
                        (LexerEntry-end symbol)
                        (LexerEntry-text symbol)
                        (LexerEntry-type symbol))
                  (list 19 25 "define" 'symbol))
    (check-false (lexer-snapshot-symbol-at snapshot 36))
    (check-false (lexer-snapshot-symbol-at snapshot 18))

    (define paren-token (lexer-snapshot-token-at snapshot 18))
    (check-true (LexerEntry? paren-token))
    (check-equal? (list (LexerEntry-start paren-token)
                        (LexerEntry-end paren-token)
                        (LexerEntry-text paren-token)
                        (LexerEntry-type paren-token))
                  (list 18 19 "(" 'open-paren))

    (define token (lexer-snapshot-token-at snapshot 36))
    (check-true (LexerEntry? token))
    (check-equal? (list (LexerEntry-start token)
                        (LexerEntry-end token)
                        (LexerEntry-text token)
                        (LexerEntry-type token))
                  (list 35 42 "\"hello\"" 'string))
    (define space-token (lexer-snapshot-token-at snapshot 25))
    (check-true (LexerEntry? space-token))
    (check-equal? (list (LexerEntry-start space-token)
                        (LexerEntry-end space-token)
                        (LexerEntry-text space-token)
                        (LexerEntry-type space-token))
                  (list 25 26 " " 'white-space)))

  (test-case
    "tree query first meaningful symbol skips whitespace tokens"
    (define snapshot
      (build-lexer-snapshot "#lang racket/base\n(  list)\n(+ 1 2)\n"))
    (define forest
      (build-snapshot-token-forest (LexerSnapshot-text snapshot)
                                   #f
                                   (LexerSnapshot-tokens snapshot)))
    (check-equal?
      (LexerTokenSpan-start (token-forest-form-head forest 20))
      21)
    (check-equal?
      (LexerTokenSpan-start (token-forest-form-head forest 21))
      21)
    (check-false (token-forest-form-head forest 27)))

  (test-case
    "tree query first meaningful symbol skips comments"
    (define text "#lang racket/base\n( ; comment\n  list)\n")
    (define snapshot (build-lexer-snapshot text))
    (define forest
      (build-snapshot-token-forest (LexerSnapshot-text snapshot)
                                   #f
                                   (LexerSnapshot-tokens snapshot)))
    (define d (make-doc "file:///comment-test.rkt" text))
    (check-equal?
      (LexerTokenSpan-start (token-forest-form-head forest
                                                    (doc-pos->abs-pos d (Pos 1 1))))
      (doc-pos->abs-pos d (Pos 2 2))))

  (test-case
    "Document lexer snapshot invalidates on edits and resets"
    (define d (make-doc "file:///test.rkt" "foo"))
    (check-equal? (LexerEntry-text (doc-token-at d 0)) "foo")

    (doc-apply-edit! d
                     (Range (Pos 0 0) (Pos 0 3))
                     "bar")
    (check-equal? (LexerEntry-text (doc-token-at d 0)) "bar")

    (doc-reset! d "\"baz\"")
    (check-equal? (LexerEntry-text (doc-token-at d 0)) "\"baz\""))

  (test-case
    "doc-token-at works on a fresh document"
    (define d (make-doc "file:///test.rkt" "(define answer 42)"))
    (check-equal? (LexerEntry-text (doc-token-at d 1)) "define")
    (check-equal? (LexerEntry-text (doc-token-at d 8)) "answer"))

  (test-case
    "doc-token-prefix-at returns the prefix ending at a position"
    (define d (make-doc "file:///prefix-test.rkt" "foo"))
    (check-equal? (doc-token-prefix-at d 1) "fo")
    (check-equal? (doc-token-prefix-at d 10) ""))

  (test-case
    "doc-symbols returns lexer-derived symbol information"
    (define uri "file:///tmp/doc-test.rkt")
    (define d (make-doc uri "#lang racket\n(define x 1)\nx\n"))
    (define result (doc-symbols d uri))
    (check-equal? (length result) 4)
    (define define-sym (findf (λ (s) (equal? (SymbolInformation-name s) "define")) result))
    (check-not-false define-sym)
    (check-equal? (SymbolInformation-kind define-sym) SymbolKind-Variable)
    (define one-sym (findf (λ (s) (equal? (SymbolInformation-name s) "1")) result))
    (check-not-false one-sym)
    (check-equal? (SymbolInformation-kind one-sym) SymbolKind-Constant)))
