#lang racket

(module+ test
  (require rackunit
           "../../doclib/doc.rkt"
           "../../doclib/doc-trace.rkt"
           "../../doclib/check-syntax.rkt"
           "../../doclib/editor.rkt"
           "../../doclib/internal-types.rkt"
           "../../common/interfaces.rkt"
           racket/class
           racket/file
           data/interval-map)

  (test-case
    "Document creation and basic accessors"
    (define d (make-doc "file:///test.rkt" "hello world"))
    (check-equal? (Doc-version d) 0)
    (check-equal? (Doc-uri d) "file:///test.rkt")
    (check-equal? (doc-get-text d) "hello world"))

  (test-case
    "Document update"
    (define d (make-doc "file:///test.rkt" "hello world"))
    ;; Replace "world" with "racket"
    ;; "hello world"
    ;; 01234567890
    ;; world starts at 6, len 5.
    (doc-apply-edit! d
                     (Range (Pos 0 6) (Pos 0 11))
                     "racket")
    (check-equal? (doc-get-text d) "hello racket")

    ;; Insert "!" at end
    ;; "hello racket" length is 12
    (doc-apply-edit! d
                     (Range (Pos 0 12) (Pos 0 12))
                     "!")
    (check-equal? (doc-get-text d) "hello racket!"))

  (test-case
    "Document deletions and complex updates"
    (define d (make-doc "file:///test.rkt" "12345"))

    ;; Delete "234" (indices 1 to 4)
    ;; "12345"
    ;;  01234
    (doc-apply-edit! d
                     (Range (Pos 0 1) (Pos 0 4))
                     "")
    (check-equal? (doc-get-text d) "15")

    ;; Prepend "0"
    (doc-apply-edit! d
                     (Range (Pos 0 0) (Pos 0 0))
                     "0")
    (check-equal? (doc-get-text d) "015")

    ;; Replace everything
    (doc-apply-edit! d
                     (Range (Pos 0 0) (Pos 0 3))
                     "cleaned")
    (check-equal? (doc-get-text d) "cleaned"))

  (test-case
    "Document position conversion"
    (define text "line1\nline2\nline3")
    (define d (make-doc "file:///test.rkt" text))

    ;; check doc-pos->abs-pos
    ;; line1\n is 6 chars (0-5)
    ;; line2 starts at 6
    (check-equal? (doc-pos->abs-pos d (Pos 0 0)) 0)
    (check-equal? (doc-pos->abs-pos d (Pos 1 0)) 6)
    (check-equal? (doc-pos->abs-pos d (Pos 2 0)) 12)

    ;; check doc-abs-pos->pos
    (define p (doc-abs-pos->pos d 6))
    (check-equal? (Pos-line p) 1)
    (check-equal? (Pos-char p) 0))

  (test-case
    "Find containing paren"
    (define text "(list 1 2)")
    (define d (make-doc "file:///test.rkt" text))
    ;; (list 1 2)
    ;; 0123456789
    ;; inside `list` at 2
    (check-equal? (doc-find-containing-paren d 2) 0)
    ;; at 1 (just after open paren)
    (check-equal? (doc-find-containing-paren d 1) 0)
    ;; at last position (close-paren at buffer end, still inside the form)
    (check-equal? (doc-find-containing-paren d 9) 0)

    (define text2 "((a) b)")
    (define d2 (make-doc "file:///test.rkt" text2))
    ;; ((a) b)
    ;; 0123456
    ;; inside (a) at 2 ('a')
    (check-equal? (doc-find-containing-paren d2 2) 1)
    ;; inside outer at 5 ('b')
    (check-equal? (doc-find-containing-paren d2 5) 0)

    ;; Edge cases
    (define text3 "( [ { ] )")
    ;; 012345678
    (define d3 (make-doc "file:///test.rkt" text3))

    ;; Inside [ : pos 3 ' '. Previous is [.
    (check-equal? (doc-find-containing-paren d3 3) 2)

    ;; Inside { : pos 5. The lexer normalizes { as an opening paren, so it is
    ;; the enclosing delimiter here.
    (check-equal? (doc-find-containing-paren d3 5) 4)

    ;; Unmatched close
    (define d4 (make-doc "file:///test.rkt" " ) ("))
    ;; 0123
    (check-false (doc-find-containing-paren d4 1)))

  (test-case
    "Find containing paren ignores parens inside strings"
    (define d (make-doc "file:///test.rkt" "(foo \"(\")"))
    (check-equal? (doc-find-containing-paren d 6) 0))

  (test-case
    "Document meta updates"
    (define d (make-doc "file:///test.rkt" "v1"))
    (doc-update-version! d 2)
    (check-equal? (Doc-version d) 2)
    (doc-update-uri! d "file:///test2.rkt")
    (check-equal? (Doc-uri d) "file:///test2.rkt")
    (doc-reset! d "v2")
    (check-equal? (doc-get-text d) "v2"))

  (test-case
    "Document line/pos calc"
    (define text "line1\nline2")
    ;; line1\n is 6 chars, line2 is 5 chars. Total 11.
    (define d (make-doc "file:///test.rkt" text))
    ;; doc-end-abs-pos
    (check-equal? (doc-end-abs-pos d) 11)
    ;; doc-line-start-abs-pos
    (check-equal? (doc-line-start-abs-pos d 1) 6)
    ;; doc-line-end-abs-pos
    (check-equal? (doc-line-end-abs-pos d 0) 5) ;; excludes newline
    (check-equal? (doc-line-end-abs-pos d 1) 11))

  (test-case
    "doc-token-at returns the token at the given position"
    (define text "foo bar-baz \"str\"")
    (define d (make-doc "file:///test.rkt" text))

    (define (token-summary token)
      (list (LexerEntry-start token)
            (LexerEntry-end token)
            (LexerEntry-text token)
            (LexerEntry-type token)))

    (check-equal? (token-summary (doc-token-at d 2))
                  (list 0 3 "foo" 'symbol))
    (check-equal? (token-summary (doc-token-at d 10))
                  (list 4 11 "bar-baz" 'symbol))
    (check-equal? (token-summary (doc-token-at d 3))
                  (list 3 4 " " 'white-space))
    (check-equal? (token-summary (doc-token-at d 12))
                  (list 12 17 "\"str\"" 'string))
    (check-equal? (token-summary (doc-token-at d 15))
                  (list 12 17 "\"str\"" 'string))
    (check-equal? (token-summary (doc-token-at d 16))
                  (list 12 17 "\"str\"" 'string))
    (check-equal? (token-summary (doc-token-at d 0))
                  (list 0 3 "foo" 'symbol)))

  (test-case
    "doc-token-at works on a non-sexp document without depending on body forest"
    (define text "#lang scribble/manual\n@section{Hi}\n")
    (define d (make-doc "file:///test.scrbl" text))
    ;; Flat token queries should work even for non-sexp languages.
    (check-equal? (LexerEntry-text (doc-token-at d 6)) "#lang scribble/manual")
    (check-equal? (LexerEntry-type (doc-token-at d 6)) 'lang-directive)
    (check-equal? (doc-token-prefix-at d 6) "#lang s"))

  (test-case
    "doc-body-forest builds a forest for unknown languages"
    (define text "#lang not-a-real-language\n(define x 1)\n")
    (define d (make-doc "file:///test.unknown" text))
    (check-not-false (doc-body-forest d)))

  (test-case
    "doc-find-containing-paren works for unknown languages"
    (define text "#lang not-a-real-language\n(define x 1)\n")
    (define d (make-doc "file:///test.unknown" text))
    (check-equal? (doc-find-containing-paren d 28) 26))

  (test-case
    "doc-find-containing-paren fallback keeps the first form without a language header"
    (define d (make-doc "file:///test.rkt" "(first x)\n(second y)\n"))
    (check-equal? (doc-find-containing-paren d 2) 0)
    (check-equal? (doc-find-containing-paren d 12) 10))

  (test-case
    "Range tokens (Semantic Tokens)"
    (define text "#lang racket\n(define x 1)")
    (define d (make-doc "file:///test.rkt" text))

    (define before-expand (doc-range-tokens d (Range (Pos 0 0) (Pos 1 11))))
    (check-true (empty? before-expand) "tokens should be empty before doc-expand!")

    (check-true (doc-expand! d))
    (define after-expand (doc-range-tokens d (Range (Pos 0 0) (Pos 1 11))))
    (check-false (empty? after-expand) "tokens should exist after doc-expand!")

    (check-true (andmap SemanticToken? after-expand)))

  (test-case
    "Range tokens include sexp comment semantic tokens"
    (define text "#lang racket\n#; (define x 1)\n(+ 1 2)")
    (define d (make-doc "file:///test.rkt" text))
    (define comment-range (first (regexp-match-positions #px"#; \\(define x 1\\)" text)))
    (define tokens (doc-range-tokens d (Range (Pos 0 0) (Pos 2 7))))
    (define comment-token
      (findf (lambda (token)
               (eq? (SemanticToken-type token) SemanticTokenType-comment))
             tokens))
    (check-true (SemanticToken? comment-token))
    (check-equal? (SemanticToken-start comment-token) (car comment-range))
    (check-equal? (SemanticToken-end comment-token) (cdr comment-range)))

  (test-case
    "Range tokens split multi-line sexp comment semantic tokens"
    (define text "#lang racket\n#;\n(define x 1)\n(+ 1 2)")
    (define d (make-doc "file:///test.rkt" text))
    (define tokens (doc-range-tokens d (Range (Pos 0 0) (Pos 3 7))))
    (define comment-ranges
      (for/list ([token (in-list tokens)]
                 #:when (eq? (SemanticToken-type token) SemanticTokenType-comment))
        (cons (SemanticToken-start token) (SemanticToken-end token))))
    (check-equal? comment-ranges
                  (list (first (regexp-match-positions #px"#;" text))
                        (first (regexp-match-positions #px"\\(define x 1\\)" text)))))

  (test-case
    "Range tokens remove stale trace tokens inside current sexp comments"
    (define text "#lang racket\n(define x 1)\nx\n")
    (define d (make-doc "file:///test.rkt" text))
    (check-true (doc-expand! d))

    (doc-apply-edit! d (Range (Pos 1 0) (Pos 1 0)) "#; ")

    (define updated-text (doc-get-text d))
    (define comment-range
      (first (regexp-match-positions #px"#; \\(define x 1\\)" updated-text)))
    (define tokens (doc-range-tokens d (Range (Pos 0 0) (Pos 3 0))))
    (define-values (comment-start comment-end)
      (values (car comment-range) (cdr comment-range)))
    (define (token-intersects-comment? token)
      (char-range-intersect? (SemanticToken-start token)
                             (SemanticToken-end token)
                             comment-start
                             comment-end))
    (define (comment-token? token)
      (eq? (SemanticToken-type token) SemanticTokenType-comment))
    (define (token-starts-before? left right)
      (<= (SemanticToken-start left) (SemanticToken-start right)))

    (define comment-token
      (findf (λ (token)
               (and (comment-token? token)
                    (= (SemanticToken-start token) comment-start)
                    (= (SemanticToken-end token) comment-end)))
             tokens))
    (define non-comment-tokens
      (filter-not comment-token? tokens))

    (check-true (SemanticToken? comment-token))
    (check-false
      (ormap token-intersects-comment? non-comment-tokens)
      "current sexp-comment span should mask intersecting stale trace tokens")
    (check-not-false
      (findf (λ (token)
               (and (not (comment-token? token))
                    (not (token-intersects-comment? token))))
             tokens)
      "stale trace tokens outside the comment should be preserved")
    (check-true
      (for/and ([left (in-list tokens)]
                [right (in-list (rest tokens))])
        (token-starts-before? left right))
      "semantic tokens should remain monotonic for LSP relative encoding"))

  (test-case
    "Formatting"
    ;; doc.rkt `doc-format-edits` delegates to the external formatter.
    (define text "#lang racket/base\n(define x\n1)")
    (define d (make-doc "file:///test.rkt" text))
    (define opts
      (FormattingOptions #:tab-size 2
                         #:insert-spaces #t
                         #:trim-trailing-whitespace #t
                         #:insert-final-newline #f
                         #:trim-final-newlines #f
                         #:key #f)) ;; tab-size 2
    (define edits (doc-format-edits d (Range (Pos 0 0) (Pos 2 0)) #:formatting-options opts))
    (check-equal? (length edits) 1)
    (check-true (andmap TextEdit? edits))
    (check-equal? (map TextEdit-newText edits) (list "  1)"))

    ;; Test with tab size 4
    (define opts4
      (FormattingOptions #:tab-size 4
                         #:insert-spaces #t
                         #:trim-trailing-whitespace #t
                         #:insert-final-newline #f
                         #:trim-final-newlines #f
                         #:key #f))
    (define edits4 (doc-format-edits d (Range (Pos 0 0) (Pos 2 0)) #:formatting-options opts4))
    (check-equal? (length edits4) 1)
    (check-true (andmap TextEdit? edits4))
    (check-equal? (map TextEdit-newText edits4) (list "  1)")))

  (test-case
    "Formatting modes"
    (define opts
      (FormattingOptions #:tab-size 2
                         #:insert-spaces #t
                         #:trim-trailing-whitespace #t
                         #:insert-final-newline #f
                         #:trim-final-newlines #f
                         #:key #f))

    (define normal-doc
      (make-doc "file:///test.rkt"
                "#lang racket/base\n\n(define (bob)\n  \n  (+ 1 2))\n"))
    (check-equal?
      (doc-format-edits normal-doc
                        (Range (Pos 3 0) (Pos 4 0))
                        #:formatting-options opts)
      (list (TextEdit (Range (Pos 3 0) (Pos 3 2)) "")))

    (define interactive-doc
      (make-doc "file:///test.rkt"
                "#lang racket/base\n\n(define (bob)\n\n  (+ 1 2))\n"))
    (check-equal?
      (doc-format-edits interactive-doc
                        (Range (Pos 3 0) (Pos 3 0))
                        #:on-type? #t
                        #:formatting-options opts)
      (list (TextEdit (Range (Pos 3 0) (Pos 3 0)) "  "))))

  (test-case
    "Formatting language guard"
    (define opts
      (FormattingOptions #:tab-size 2
                         #:insert-spaces #t
                         #:trim-trailing-whitespace #t
                         #:insert-final-newline #f
                         #:trim-final-newlines #f
                         #:key #f))

    (define raw-doc
      (make-doc "file:///test.rkt" "(define x\n1)"))
    (check-equal?
      (doc-format-edits raw-doc
                        (Range (Pos 0 0) (Pos 2 0))
                        #:formatting-options opts)
      '())

    (define rhombus-doc
      (make-doc "file:///test.rhm"
                "#lang rhombus\n  fun f():\n    1\n"))
    (check-equal?
      (doc-format-edits rhombus-doc
                        (Range (Pos 0 0) (Pos 3 0))
                        #:formatting-options opts)
      '()))

  (define (find-diagnostic-by-message diags expected-message)
    (for/first ([diag (in-list diags)]
                #:when (string=? (Diagnostic-message diag) expected-message))
      diag))

  (define (language-declaration-diagnostics diags)
    (for/list ([diag (in-list diags)]
               #:when (string=? (Diagnostic-source diag) "Language Declaration Check"))
      diag))

  (define (check-syntax-diagnostics uri text)
    (define doc-text (new lsp-editor%))
    (send doc-text insert text 0)
    (set->list (send (CSResult-trace (check-syntax uri doc-text)) get-warn-diags)))

  (test-case
    "Document diagnostics report missing language declarations"
    (define text "(define x 1)\n")
    (define diags
      (check-syntax-diagnostics "file:///tmp/missing-language-test.rkt"
                                text))
    (define diag
      (find-diagnostic-by-message
        diags
        "Missing language header. Start the file with `#lang <language>`, `#reader <reader>`, or `(module <name> <language> ...)`."))
    (check-not-false diag)
    (check-equal? (Diagnostic-source diag) "Language Declaration Check")
    (define range (Diagnostic-range diag))
    (check-equal? (Pos-line (Range-start range)) 0)
    (check-equal? (Pos-char (Range-start range)) 0)
    (check-equal? (Pos-line (Range-end range)) 0)
    (check-equal? (Pos-char (Range-end range))
                  (string-length "(define x 1)")))

  (test-case
    "Document diagnostics accept unknown language headers"
    (define text "#lang not-a-real-language\n1\n")
    (define diags
      (check-syntax-diagnostics "file:///tmp/unknown-language-test.rkt"
                                text))
    (check-equal? (language-declaration-diagnostics diags) '()))

  (test-case
    "Document diagnostics accept wrapped #lang declarations"
    (define at-exp-diags
      (check-syntax-diagnostics "file:///tmp/at-exp-language-test.rkt"
                                "#lang at-exp racket\n@(+ 1 2)\n"))
    (check-equal? (language-declaration-diagnostics at-exp-diags) '())

    (define s-exp-diags
      (check-syntax-diagnostics "file:///tmp/s-exp-language-test.rkt"
                                "#lang s-exp racket/base\n(+ 1 2)\n"))
    (check-equal? (language-declaration-diagnostics s-exp-diags) '()))

  (test-case
    "Document diagnostics use first line range for empty language spans"
    (define text "#lang \n(define x 1)\n")
    (define diags
      (check-syntax-diagnostics "file:///tmp/empty-language-test.rkt"
                                text))
    (define diag
      (find-diagnostic-by-message
        diags
        "Incomplete language header. Provide the missing language or reader name."))
    (check-not-false diag)
    (check-equal? (Diagnostic-source diag) "Language Declaration Check")
    (define range (Diagnostic-range diag))
    (check-equal? (Pos-line (Range-start range)) 0)
    (check-equal? (Pos-char (Range-start range)) 0)
    (check-equal? (Pos-line (Range-end range)) 0)
    (check-equal? (Pos-char (Range-end range))
                  (string-length "#lang ")))

  (test-case
    "Apply TextEdits"
    (define text "#lang racket/base\n(define x\n1)")
    (define d (make-doc "file:///test.rkt" text))
    (define opts
      (FormattingOptions #:tab-size 2
                         #:insert-spaces #t
                         #:trim-trailing-whitespace #t
                         #:insert-final-newline #f
                         #:trim-final-newlines #f
                         #:key #f))
    (define edits (doc-format-edits d (Range (Pos 0 0) (Pos 2 0)) #:formatting-options opts))
    (check-equal?
      edits
      (list (TextEdit (Range (Pos 2 0) (Pos 2 2)) "  1)")))
    (check-equal? (LexerEntry-type (doc-token-at d 19)) 'symbol)
    (doc-apply-edits! d edits)
    (check-equal? (doc-get-text d) "#lang racket/base\n(define x\n  1)")
    (define updated-token
      (doc-token-at d (doc-pos->abs-pos d (Pos 2 2))))
    (check-true (LexerEntry? updated-token))
    (check-equal? (LexerEntry-type updated-token) 'constant)
    (check-equal? (LexerEntry-text updated-token) "1"))

  (test-case
    "Apply TextEdits invalidates lexer snapshot across spanning tokens"
    (define d (make-doc "file:///test.rkt" "#lang racket\n#| comment |#\nfoo\n"))
    (check-equal? (LexerEntry-type (doc-token-at d (doc-pos->abs-pos d (Pos 2 0))))
                  'symbol)
    (doc-apply-edits! d (list (TextEdit (Range (Pos 1 11) (Pos 1 13)) "")))
    (define comment-token
      (doc-token-at d (doc-pos->abs-pos d (Pos 2 0))))
    (check-true (LexerEntry? comment-token))
    (check-equal? (LexerEntry-type comment-token) 'error)
    (check-equal? (LexerEntry-text comment-token) "#| comment \nfoo\n"))

  (test-case
    "Get definition"
    (define tmp-file (make-temporary-file "test~a.rkt"))
    (define text "#lang racket\n(define x 1)\nx")
    (with-output-to-file tmp-file #:exists 'replace (lambda () (display text)))

    (define def-range (doc-get-definition-by-id tmp-file 'x))
    (check-pred Range? def-range)

    (delete-file tmp-file))

  ;; Tests for newly extracted doc-* functions
  ;; All use a common expanded document:
  ;;   #lang racket
  ;;   (define x 1)
  ;;   x
  ;;
  ;; Byte positions:
  ;;   Line 0: "#lang racket"   pos 0..12, newline at 12
  ;;   Line 1: "(define x 1)"   pos 13..25, newline at 25
  ;;   Line 2: "x"              pos 26..27
  ;;
  ;; "x" definition: line 1, char 8, abs pos 21..22
  ;; "x" usage:      line 2, char 0, abs pos 26..27
  ;; "define":       line 1, char 1..7, abs pos 14..20

  (test-case
    "rktd data file: no missing #lang or expansion diagnostics"
    (define d (make-doc "file:///tmp/doc-test-data.rktd" "((a . 1) (b . 2))"))
    (check-true (doc-expand! d))
    (check-equal? (doc-diagnostics d) '()))

  (test-case
    "rktd data file: read errors are still reported"
    (define d (make-doc "file:///tmp/doc-test-data.rktd" "((a . 1)"))
    (check-false (doc-expand! d)))

  (test-case
    "rkt file without #lang still reports missing #lang"
    (define d (make-doc "file:///tmp/doc-test-nolang.rkt" "1234"))
    (check-true (doc-expand! d))
    (check-true (for/or ([diag (in-list (doc-diagnostics d))])
                  (regexp-match? #rx"#lang" (Diagnostic-message diag)))))

  (define (make-expanded-doc)
    (define text
#<<END
#lang racket
(define x 1)
x
END
      )
    (define uri "file:///tmp/doc-test.rkt")
    (define d (make-doc uri text))
    (doc-expand! d)
    (values d uri))

  (define (make-doc-with-online-completions text prefix->completions)
    (define uri "file:///tmp/completion-prefix-test.rkt")
    (define d (make-doc uri text))
    (define test-trace%
      (class build-trace%
        (super-new [src (string->path "/tmp/completion-prefix-test.rkt")]
                   [doc-text (new lsp-editor%)])
        (define/override (get-completions) '())
        (define/override (get-online-completions str-before-cursor)
          (hash-ref prefix->completions str-before-cursor '()))))
    (doc-update-trace! d (new test-trace%) (Doc-version d))
    d)

  (test-case
    "doc-get-decl on a binding usage"
    (define-values (d _uri) (make-expanded-doc))
    ;; "x" at line 2, char 0 is a usage → resolves to local declaration
    (define-values (start end decl) (doc-get-decl d (Pos 2 0)))
    (check-equal? start 26 "usage start pos")
    (check-equal? end 27 "usage end pos")
    (check-false (Decl-filepath decl) "local binding has no filepath")
    (check-equal? (Decl-left decl) 21 "declaration left pos")
    (check-equal? (Decl-right decl) 22 "declaration right pos"))

  (test-case
    "doc-get-decl on the definition site"
    (define-values (d _uri) (make-expanded-doc))
    ;; "x" in (define x 1) at line 1, char 8
    (define-values (start end decl) (doc-get-decl d (Pos 1 8)))
    (check-equal? start 21 "definition start pos")
    (check-equal? end 22 "definition end pos")
    (check-false (Decl-filepath decl) "local binding has no filepath")
    (check-equal? (Decl-left decl) 21)
    (check-equal? (Decl-right decl) 22))

  (test-case
    "doc-get-decl on imported 'define'"
    (define-values (d _uri) (make-expanded-doc))
    ;; "define" at line 1, char 1 is an imported symbol
    (define-values (start end decl) (doc-get-decl d (Pos 1 1)))
    (check-equal? start 14 "imported define start pos")
    (check-equal? end 20 "imported define end pos")
    (check-not-false (Decl-filepath decl) "imported binding has a filepath")
    ;; Imported symbols have left=0, right=0
    (check-equal? (Decl-left decl) 0)
    (check-equal? (Decl-right decl) 0))

  (test-case
    "doc-get-decl on literal returns #f"
    (define-values (d _uri) (make-expanded-doc))
    ;; "1" at line 1, char 10 is a literal
    (define-values (_start _end decl) (doc-get-decl d (Pos 1 10)))
    (check-false decl "literal should not have a declaration"))

  (test-case
    "doc-get-bindings returns usage ranges for local x"
    (define-values (d _uri) (make-expanded-doc))
    (define-values (_s _e decl) (doc-get-decl d (Pos 2 0)))
    (define bindings (doc-get-bindings d decl))
    ;; Should contain exactly the usage of "x" at line 2, char 0..1
    (check-equal? (length bindings) 1)
    (check-equal? (first bindings)
                  (Range (Pos 2 0) (Pos 2 1))))

  (test-case
    "doc-completion returns x in items"
    (define-values (d _uri) (make-expanded-doc))
    (define result (doc-completion d (Pos 2 1)))
    (check-equal? (CompletionList-isIncomplete result) #t)
    (define items (CompletionList-items result))
    ;; "x" should be among the completions
    (check-not-false
      (findf (λ (i) (equal? (CompletionItem-label i) "x")) items)
      "x should be in completions"))

  (test-case
    "doc-completion at buffer start returns a list"
    (define-values (d _uri) (make-expanded-doc))
    (check-pred CompletionList?
                (doc-completion d (Pos 0 0))))

  (test-case
    "doc-completion at symbol buffer start does not consume the first character"
    (define d (make-doc-with-online-completions "foo"
                                                (hash "" '(alpha beta))))
    (define result (doc-completion d (Pos 0 0)))
    (define items (CompletionList-items result))
    (check-equal? (map CompletionItem-label items)
                  '("alpha" "beta")
                  "buffer-start completion should pass an empty prefix to online completion lookup"))

  (test-case
    "doc-completion passes a symbol module-path prefix to online completion lookup"
    (define d
      (make-doc-with-online-completions
        "#lang racket/base\n(require foo/bar)\n"
        (hash "foo/bar" '(symbol-prefix))))
    (define result (doc-completion d (Pos 1 16)))
    (check-equal? (map CompletionItem-label (CompletionList-items result))
                  '("symbol-prefix")))

  (test-case
    "doc-completion passes a string module-path prefix to online completion lookup"
    (define d
      (make-doc-with-online-completions
        "#lang racket/base\n(require \"foo/bar\")\n"
        (hash "foo/bar" '(string-prefix))))
    (define result (doc-completion d (Pos 1 17)))
    (check-equal? (map CompletionItem-label (CompletionList-items result))
                  '("string-prefix")))

  (test-case
    "doc-completion ignores a quote token before a quoted module path"
    (define d
      (make-doc-with-online-completions
        "#lang racket/base\n(require 'foo/bar)\n"
        (hash "" '(empty-prefix)
              "'" '(quote-prefix))))
    (define result (doc-completion d (Pos 1 10)))
    (check-equal? (map CompletionItem-label (CompletionList-items result))
                  '("empty-prefix")))

  (test-case
    "doc-completion ignores require keywords for module-name completion"
    (define d
      (make-doc-with-online-completions
        "#lang racket/base\n(require #:only-in racket/base)\n"
        (hash "" '(empty-prefix)
              "#:only-in" '(keyword-prefix))))
    (define result (doc-completion d (Pos 1 18)))
    (check-equal? (map CompletionItem-label (CompletionList-items result))
                  '("empty-prefix")))

  (test-case
    "doc-definition for local x usage"
    (define-values (d uri) (make-expanded-doc))
    ;; "x" at line 2 → definition at line 1 char 8..9
    (define result (doc-definition d uri (Pos 2 0)))
    (check-equal? (Location-uri result) uri)
    (check-equal? (Location-range result)
                  (Range (Pos 1 8) (Pos 1 9))))

  (test-case
    "doc-definition for imported define"
    (define-values (d uri) (make-expanded-doc))
    ;; "define" at line 1 → jumps to external file
    (define result (doc-definition d uri (Pos 1 1)))
    ;; URI should point to the racket source file, not our test file
    (check-not-equal? (Location-uri result) uri
                      "imported define should point to external file"))

  (test-case
    "doc-definition returns #f for literal"
    (define-values (d uri) (make-expanded-doc))
    (define result (doc-definition d uri (Pos 1 10)))
    (check-false result))

  (test-case
    "doc-references for local x"
    (define-values (d uri) (make-expanded-doc))
    ;; "x" usage at (2,0) is a local binding, so doc-references returns bindings
    (define result (doc-references d uri (Pos 2 0) #t))
    (check-equal? (length result) 1)
    (define ref (first result))
    (check-equal? (Location-uri ref) uri)
    (check-equal? (Location-range ref)
                  (Range (Pos 2 0) (Pos 2 1))))

  (test-case
    "doc-highlights for local x"
    (define-values (d _uri) (make-expanded-doc))
    ;; "x" usage at (2,0): highlights should include both usage and declaration
    (define result (doc-highlights d (Pos 2 0)))
    (check-equal? (length result) 2)
    ;; First highlight: the usage (binding) at line 2
    (check-equal? (DocumentHighlight-range (first result))
                  (Range (Pos 2 0) (Pos 2 1)))
    ;; Second highlight: the declaration at line 1
    (check-equal? (DocumentHighlight-range (second result))
                  (Range (Pos 1 8) (Pos 1 9))))

  (test-case
    "doc-rename local x to y"
    (define-values (d uri) (make-expanded-doc))
    ;; Rename "x" at definition site (1,8) to "y"
    (define result (doc-rename d uri (Pos 1 8) "y"))
    (define changes (WorkspaceEdit-changes result))
    (define edits (hash-ref changes (string->symbol uri)))
    (check-equal? (length edits) 2 "should produce 2 edits (defn + usage)")
    ;; First edit: declaration at line 1 char 8..9
    (check-equal? (TextEdit-newText (first edits)) "y")
    (check-equal? (TextEdit-range (first edits))
                  (Range (Pos 1 8) (Pos 1 9)))
    ;; Second edit: usage at line 2 char 0..1
    (check-equal? (TextEdit-newText (second edits)) "y")
    (check-equal? (TextEdit-range (second edits))
                  (Range (Pos 2 0) (Pos 2 1))))

  (test-case
    "doc-rename on imported define returns #f"
    (define-values (d uri) (make-expanded-doc))
    (define result (doc-rename d uri (Pos 1 1) "my-define"))
    (check-false result))

  (test-case
    "doc-prepare-rename for local x"
    (define-values (d _uri) (make-expanded-doc))
    ;; "x" at definition site (1,8) → renamable, returns its range
    (define result (doc-prepare-rename d (Pos 1 8)))
    (check-equal? result
                  (Range (Pos 1 8) (Pos 1 9))))

  (test-case
    "doc-prepare-rename on imported define returns #f"
    (define-values (d _uri) (make-expanded-doc))
    (define result (doc-prepare-rename d (Pos 1 1)))
    (check-false result))

  (test-case
    "Document hover"
    (define text
#<<END
#lang racket
(list)
END
      )
    (define uri "file:///tmp/hover-test.rkt")
    (define d (make-doc uri text))
    (doc-expand! d)

    (define h (doc-hover d (Pos 1 1)))
    (check-not-false h)
    (define result (Hover-contents h))
    (check-true (string-contains? result "Returns a newly allocated list")))

  (test-case
    "Document signature help"
    (define text
#<<END
#lang racket/base

(list )
END
      )
    (define uri "file:///tmp/signature-help-test.rkt")
    (define d (make-doc uri text))
    (doc-expand! d)

    (define help (doc-signature-help d (Pos 2 6)))
    (check-not-false help "help should not be #f")
    (define sigs (SignatureHelp-signatures help))
    (check-false (empty? sigs) "signatures should not be empty")
    (define first-sig (first sigs))
    (check-true (string-contains? (SignatureInformation-label first-sig) "list")
                "label should contain 'list'"))

  (test-case
    "Document signature help for single-character function"
    (define text
#<<END
#lang racket/base

(+ )
END
      )
    (define uri "file:///tmp/signature-help-single-char-test.rkt")
    (define d (make-doc uri text))
    (doc-expand! d)

    (define help (doc-signature-help d (Pos 2 3)))
    (check-not-false help "help should not be #f for single-character callees")
    (define sigs (SignatureHelp-signatures help))
    (check-false (empty? sigs) "signatures should not be empty")
    (define first-sig (first sigs))
    (check-true (string-contains? (SignatureInformation-label first-sig) "+")
                "label should contain '+'"))

  (test-case
    "Document signature help skips spaces after the opening paren"
    (define text
#<<END
#lang racket/base

(  list )
END
      )
    (define uri "file:///tmp/signature-help-space-test.rkt")
    (define d (make-doc uri text))
    (doc-expand! d)

    (define help (doc-signature-help d (Pos 2 8)))
    (check-not-false help "help should not be #f when whitespace separates the callee")
    (define sigs (SignatureHelp-signatures help))
    (check-false (empty? sigs) "signatures should not be empty")
    (define first-sig (first sigs))
    (check-true (string-contains? (SignatureInformation-label first-sig) "list")
                "label should contain 'list'"))

  (test-case
    "Document signature help skips a newline after the opening paren"
    (define text
#<<END
#lang racket/base

(
  list )
END
      )
    (define uri "file:///tmp/signature-help-newline-test.rkt")
    (define d (make-doc uri text))
    (doc-expand! d)

    (define help (doc-signature-help d (Pos 3 7)))
    (check-not-false help "help should not be #f when the callee starts on the next line")
    (define sigs (SignatureHelp-signatures help))
    (check-false (empty? sigs) "signatures should not be empty")
    (define first-sig (first sigs))
    (check-true (string-contains? (SignatureInformation-label first-sig) "list")
                "label should contain 'list'"))

  (test-case
    "Document signature help skips a comment after the opening paren"
    (define text
#<<END
#lang racket/base

( ; comment
  list )
END
      )
    (define uri "file:///tmp/signature-help-comment-test.rkt")
    (define d (make-doc uri text))
    (doc-expand! d)

    (define help (doc-signature-help d (Pos 3 7)))
    (check-not-false help "help should not be #f when a comment separates the callee")
    (define sigs (SignatureHelp-signatures help))
    (check-false (empty? sigs) "signatures should not be empty")
    (define first-sig (first sigs))
    (check-true (string-contains? (SignatureInformation-label first-sig) "list")
                "label should contain 'list'"))

  (test-case
    "Document signature help outside a closed top-level form returns #f"
    (define text
#<<END
#lang racket/base

(list)
END
      )
    (define uri "file:///tmp/signature-help-close-paren-test.rkt")
    (define d (make-doc uri text))
    (doc-expand! d)

    (check-false (doc-signature-help d (Pos 2 6))))

  (test-case
    "Document signature help after nested closing paren moves to the surrounding call"
    (define text
#<<END
#lang racket/base

(list (+ 1 2))
END
      )
    (define uri "file:///tmp/signature-help-nested-close-paren-test.rkt")
    (define d (make-doc uri text))
    (doc-expand! d)

    (define help (doc-signature-help d (Pos 2 13)))
    (check-not-false help "help should not be #f after the inner closing paren")
    (define sigs (SignatureHelp-signatures help))
    (check-false (empty? sigs) "signatures should not be empty")
    (define first-sig (first sigs))
    (check-true (string-contains? (SignatureInformation-label first-sig) "list")
                "label should contain 'list'"))

  (test-case
    "Document signature help ignores non-symbol callee expressions"
    (define text
#<<END
#lang racket/base

((lambda (x) x) )
END
      )
    (define uri "file:///tmp/signature-help-callee-expression-test.rkt")
    (define d (make-doc uri text))
    (doc-expand! d)

    (check-false (doc-signature-help d (Pos 2 16))))

  (test-case
    "Document signature help at buffer start returns #f"
    (define text
#<<END
#lang racket/base

(list )
END
      )
    (define uri "file:///tmp/signature-help-edge-test.rkt")
    (define d (make-doc uri text))
    (doc-expand! d)
    (check-false (doc-signature-help d (Pos 0 0))))

  (test-case
    "Document code action"
    (define text
#<<END
#lang racket/base

(define x 0)
END
      )
    (define uri "file:///tmp/code-action-test.rkt")
    (define d (make-doc uri text))
    (doc-expand! d)

    (define start (Pos 2 8))
    (define end (Pos 2 9))
    (define actions (doc-code-action d (Range start end)))
    (check-false (empty? actions) "actions should not be empty")
    (define act (first actions))
    (check-equal? (CodeAction-title act) "Add prefix `_` to ignore"))

  (test-case
    "Document code action for overlapping range"
    (define text
#<<END
#lang racket/base

(define x 0)
END
      )
    (define uri "file:///tmp/code-action-overlap-test.rkt")
    (define d (make-doc uri text))
    (doc-expand! d)

    (define start (Pos 2 7))
    (define end (Pos 2 9))
    (define actions (doc-code-action d (Range start end)))
    (check-false (empty? actions) "actions should not be empty for overlapping ranges")
    (define act (first actions))
    (check-equal? (CodeAction-title act) "Add prefix `_` to ignore"))

  )
