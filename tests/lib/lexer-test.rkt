#lang racket

(module+ test
  (require rackunit
           "../../doclib/doc.rkt"
           "../../doclib/lexer.rkt"
           (only-in "../../doclib/lexer/shared.rkt"
                    make-lexer-span)
           "../../doclib/lexer/token-tree.rkt"
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
      (check-equal? next-index 3)))

  (test-case
    "token tree end uses the last child for unclosed lists"
    (define-values (non-empty-node non-empty-next-index)
      (parse-token-node
        (vector (make-lexer-span 0 1 'open-paren)
                (make-lexer-span 1 7 'symbol)
                (make-lexer-span 7 8 'white-space)
                (make-lexer-span 8 13 'symbol))
        0))
    (check-equal? non-empty-next-index 4)
    (check-equal? (token-node-end non-empty-node) 13)

    (define-values (empty-node empty-next-index)
      (parse-token-node
        (vector (make-lexer-span 0 1 'open-paren))
        0))
    (check-equal? empty-next-index 1)
    (check-equal? (token-node-end empty-node) 1))

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
    (check-equal? (LexerTokenSpan-type (Token-Leaf-span (first nodes)))
                  'white-space)
    (check-true (Token-Leaf? (second nodes)))
    (check-equal? (LexerTokenSpan-type (Token-Leaf-span (second nodes)))
                  'comment)
    (check-true (Token-Prefix-Tree? (third nodes)))
    (check-equal? (LexerTokenSpan-type
                    (Token-Prefix-Tree-prefix-span (third nodes)))
                  'sexp-comment))

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
    "lexer snapshot next symbol start skips whitespace tokens"
    (define snapshot
      (build-lexer-snapshot "#lang racket/base\n(  list)\n(+ 1 2)\n"))
    (check-equal? (lexer-snapshot-next-symbol-start snapshot 20) 21)
    (check-equal? (lexer-snapshot-next-symbol-start snapshot 21) 21)
    (check-equal? (lexer-snapshot-next-symbol-start snapshot 27) #f))

  (test-case
    "lexer snapshot next symbol start skips comments"
    (define text "#lang racket/base\n( ; comment\n  list)\n")
    (define snapshot (build-lexer-snapshot text))
    (define d (make-doc "file:///comment-test.rkt" text))
    (check-equal?
      (lexer-snapshot-next-symbol-start snapshot
                                        (doc-pos->abs-pos d (Pos 1 1)))
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
