#lang racket

(module+ test
  (require rackunit
           "../../doclib/doc.rkt"
           "../../doclib/hover.rkt"
           "../../doclib/doc-trace.rkt"
           "../../doclib/check-syntax-compat.rkt"
           "../../doclib/check-syntax.rkt"
           "../../doclib/editor.rkt"
           "../../doclib/internal-types.rkt"
           "../../doclib/lexer.rkt"
           "../../common/interfaces.rkt"
           "../../common/path-util.rkt"
           racket/class
           racket/file
           drracket/check-syntax
           data/interval-map)

  (test-case
    "Document creation and basic accessors"
    (define d (make-doc "file:///test.rkt" "hello world"))
    (check-equal? (Doc-version d) 0)
    (check-equal? (Doc-uri d) "file:///test.rkt")
    (check-false (Doc-contribution d))
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

  (test-case
    "On-type formatting delegates language policy to doclib"
    (define opts
      (FormattingOptions #:tab-size 2
                         #:insert-spaces #t
                         #:trim-trailing-whitespace #t
                         #:insert-final-newline #f
                         #:trim-final-newlines #f
                         #:key #f))

    (define sexp-doc
      (make-doc "file:///test.rkt"
                "#lang racket/base\n(define x\n1)"))
    (check-equal?
      (doc-on-type-format-edits sexp-doc
                                (Pos 2 2)
                                ")"
                                #:formatting-options opts)
      (list (TextEdit (Range (Pos 2 0) (Pos 2 2)) "  1)")))

    (define rhombus-doc
      (make-doc "file:///test.rhm"
                "#lang rhombus\n  fun f():\n    1)\n"))
    (check-equal?
      (doc-on-type-format-edits rhombus-doc
                                (Pos 2 6)
                                ")"
                                #:formatting-options opts)
      '())

    (define unknown-doc
      (make-doc "file:///unknown.rkt"
                "#lang not-a-real-language\n(define x\n1)"))
    (check-equal?
      (doc-on-type-format-edits unknown-doc
                                (Pos 2 2)
                                ")"
                                #:formatting-options opts)
      '()))

  (define (find-diagnostic-by-message diags expected-message)
    (for/first ([diag (in-list diags)]
                #:when (string=? (Diagnostic-message diag) expected-message))
      diag))

  (define (language-header-diagnostics diags)
    (for/list ([diag (in-list diags)]
               #:when (string=? (Diagnostic-source diag) "Language Header Check"))
      diag))

  (define (check-syntax-diagnostics uri text)
    (define doc-text (new lsp-editor%))
    (send doc-text insert text 0)
    (define lexer-state (build-lexer-state text uri))
    (set->list (send (CSResult-trace (check-syntax uri doc-text lexer-state)) get-warn-diags)))

  (define (typed-racket-available?)
    (with-handlers ([exn:fail? (lambda (_exn) #f)])
      (dynamic-require 'typed/racket #f)
      #t))

  (test-case
    "Expansion logs preserve emission order"
    (define text
      (string-append
        "#lang racket/base\n"
        "(require (for-syntax racket/base))\n"
        "(begin-for-syntax\n"
        "  (log-message (current-logger) 'info 'order-test \"first\" 'first)\n"
        "  (log-message (current-logger) 'info 'order-test \"second\" 'second))\n"))
    (define path
      (string->path "/tmp/expansion-log-order-test.rkt"))
    (define uri
      "file:///tmp/expansion-log-order-test.rkt")
    (define doc-text
      (new lsp-editor%))
    (send doc-text insert text 0)
    (define trace
      (new build-trace%
        [src path]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text uri)]))
    (define result
      (expand-source path
                     (open-input-string text)
                     trace))
    (define order-test-data
      (for/list ([record (in-list (ExpandResult-logs result))]
                 #:when (eq? (vector-ref record 3) 'order-test))
        (vector-ref record 2)))
    (check-equal? order-test-data
                  '(first second)))

  (test-case
    "Document diagnostics report missing language headers"
    (define text "(define x 1)\n")
    (define diags
      (check-syntax-diagnostics "file:///tmp/missing-language-test.rkt"
                                text))
    (define diag
      (find-diagnostic-by-message
        diags
        "Missing language header. Start the file with `#lang <language>`, `#reader <reader>`, or `(module <name> <language> ...)`."))
    (check-not-false diag)
    (check-equal? (Diagnostic-source diag) "Language Header Check")
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
    (check-equal? (language-header-diagnostics diags) '()))

  (test-case
    "Document diagnostics simplify missing collection messages"
    (define text "#lang racke\n")
    (define diags
      (check-syntax-diagnostics "file:///tmp/missing-collection-test.rkt"
                                text))
    (define diag
      (find-diagnostic-by-message
        diags
        (string-append
          "Cannot find language \"racke\".\n"
          "  module path: racke/lang/reader\n"
          "Check that the language name is correct and the package is installed.")))
    (check-not-false diag)
    (check-equal? (Diagnostic-source diag) "Racket"))

  (test-case
    "Document diagnostics do not label requires as #lang failures"
    (define text "#lang racket/base\n(require racke/lang/reader)\n")
    (define diags
      (check-syntax-diagnostics "file:///tmp/missing-require-collection-test.rkt"
                                text))
    (define diag
      (find-diagnostic-by-message
        diags
        (string-append
          "Cannot find language \"racke\".\n"
          "  module path: racke/lang/reader\n"
          "Check that the language name is correct and the package is installed.")))
    (check-not-false diag)
    (check-equal? (Diagnostic-source diag) "Racket"))

  (test-case
    "Document diagnostics report missing require modules"
    (define text "#lang racket/base\n(require foo/bar)\n")
    (define diags
      (check-syntax-diagnostics "file:///tmp/missing-require-module-test.rkt"
                                text))
    (define diag
      (find-diagnostic-by-message
        diags
        (string-append
          "Cannot find module \"foo/bar\" in collection \"foo\".\n"
          "Check that the module name is correct and the package is installed.")))
    (check-not-false diag)
    (check-equal? (Diagnostic-source diag) "Racket"))

  (test-case
    "Document diagnostics accept wrapped #lang headers"
    (define at-exp-diags
      (check-syntax-diagnostics "file:///tmp/at-exp-language-test.rkt"
                                "#lang at-exp racket\n@(+ 1 2)\n"))
    (check-equal? (language-header-diagnostics at-exp-diags) '())

    (define s-exp-diags
      (check-syntax-diagnostics "file:///tmp/s-exp-language-test.rkt"
                                "#lang s-exp racket/base\n(+ 1 2)\n"))
    (check-equal? (language-header-diagnostics s-exp-diags) '()))

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
    (check-equal? (Diagnostic-source diag) "Language Header Check")
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

    (define def-range (doc-get-definition-by-id tmp-file '() 0 'x))
    (check-pred Range? def-range)

    (delete-file tmp-file))

  (test-case
    "Get definition uses exact submodule and phase identity"
    (define tmp-file (make-temporary-file "binding-identity~a.rkt"))
    (define text
      (string-append
        "#lang racket/base\n"
        "(module first racket/base\n"
        "  (define same 1))\n"
        "(module second racket/base\n"
        "  (require (for-syntax racket/base))\n"
        "  (begin-for-syntax\n"
        "    (define same 2)))\n"))
    (with-output-to-file tmp-file
      #:exists 'replace
      (lambda ()
        (display text)))

    (define first-range
      (doc-get-definition-by-id tmp-file '(first) 0 'same))
    (define second-range
      (doc-get-definition-by-id tmp-file '(second) 1 'same))
    (check-equal? first-range (Range (Pos 2 10) (Pos 2 14)))
    (check-equal? second-range (Range (Pos 6 12) (Pos 6 16)))

    (delete-file tmp-file))

  (test-case
    "binding identity distinguishes equal symbols across submodules and phases"
    (define path (string->path "/tmp/binding-identity-test.rkt"))
    (define uri "file:///tmp/binding-identity-test.rkt")
    (define text (make-string 40 #\space))
    (define doc-text (new lsp-editor%))
    (send doc-text insert text 0)
    (define trace
      (new build-trace%
        [src path]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text uri)]))

    (send trace
          syncheck:add-jump-to-definition/phase-level+space
          path 1 2 'same path '(first) 0)
    (send trace
          syncheck:add-jump-to-definition/phase-level+space
          path 3 4 'same path '(second) 1)
    (send trace
          syncheck:add-definition-target/phase-level+space
          path 10 11 'same '(first) 0)
    (send trace
          syncheck:add-definition-target/phase-level+space
          path 20 21 'same '(second) 1)

    (define declaration-service (send trace get-declaration))
    (define first-binding (Module-Binding path '(first) 0 'same))
    (define second-binding (Module-Binding path '(second) 1 'same))
    (define first-target (send declaration-service module-binding-at 1))
    (define second-target (send declaration-service module-binding-at 3))
    (check-equal? first-target first-binding)
    (check-equal? second-target second-binding)
    (check-equal? (send declaration-service definition-at 1)
                  (CharRange 10 11))
    (check-equal? (send declaration-service definition-at 3)
                  (CharRange 20 21))
    (define definitions
      (Doc-Contribution-definitions (send trace get-contribution)))
    (check-equal? (hash-ref definitions first-binding)
                  (Location uri (Range (Pos 0 10) (Pos 0 11))))
    (check-equal? (hash-ref definitions second-binding)
                  (Location uri (Range (Pos 0 20) (Pos 0 21))))

    (send trace expand 0 2)
    (define shifted-target (send declaration-service module-binding-at 3))
    (check-equal? shifted-target first-binding)
    (check-equal? (send declaration-service definition-at 3)
                  (CharRange 12 13))
    (send trace contract 0 2)
    (define restored-target (send declaration-service module-binding-at 1))
    (check-equal? restored-target first-binding)
    (check-equal? (send declaration-service definition-at 1)
                  (CharRange 10 11)))

  (test-case
    "trace contribution groups module uses by exact binding identity"
    (define path (string->path "/tmp/contribution-test.rkt"))
    (define uri "file:///tmp/contribution-test.rkt")
    (define text "first use\nsecond use\n")
    (define doc-text (new lsp-editor%))
    (send doc-text insert text 0)
    (define trace
      (new build-trace%
        [src path]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text uri)]))

    (send trace
          syncheck:add-jump-to-definition/phase-level+space
          path 0 5 'same path '(first) 0)
    (send trace
          syncheck:add-jump-to-definition/phase-level+space
          path 10 16 'same path '(first) 0)
    (send trace
          syncheck:add-jump-to-definition/phase-level+space
          path 17 20 'same path '(second) 1)
    ;; Local lexical bindings are not cross-file contribution entries.
    (send trace
          syncheck:add-arrow/name-dup
          path 6 9 path 21 21 #t 0 #f #f)

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
            #:key (lambda (location)
                    (Pos-line (Range-start (Location-range location)))))
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
    (define installed-target (doc-module-binding-at d (Pos 0 0)))
    (check-equal? installed-target first-module-binding))

  (test-case
    "named binding queries resolve local and same-file module uses after edits"
    (define path (string->path "/tmp/binding-query-test.rkt"))
    (define text (make-string 50 #\space))
    (define doc-text (new lsp-editor%))
    (send doc-text insert text 0)
    (define trace
      (new build-trace%
        [src path]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text (path->uri path))]))
    (send trace
          syncheck:add-definition-target/phase-level+space
          path 5 6 'module-name '() 0)
    (send trace
          syncheck:add-arrow/name-dup
          path 5 6 path 10 11 #t 0 #f #f)
    (send trace
          syncheck:add-arrow/name-dup
          path 5 6 path 20 21 #t 0 #f #f)
    (send trace
          syncheck:add-arrow/name-dup
          path 30 31 path 35 36 #t 0 #f #f)
    (send trace
          syncheck:add-arrow/name-dup
          path 30 31 path 40 41 #t 0 #f #f)
    (send trace
          syncheck:add-arrow/name-dup
          path 30 31 path 45 46 #t 0 #t #f)

    (define declaration-service (send trace get-declaration))
    (define module-binding (Module-Binding path '() 0 'module-name))
    (define module-target (send declaration-service module-binding-at 10))
    (check-equal? module-target module-binding)
    (check-false (send declaration-service module-binding-at 35))
    (check-false (send declaration-service occurrence-at 45))
    (check-equal? (send declaration-service occurrence-at 30)
                  (CharRange 30 31))
    (check-equal? (send declaration-service uses-at 10)
                  (list (CharRange 10 11) (CharRange 20 21)))
    (check-equal? (send declaration-service uses-at 35)
                  (list (CharRange 35 36) (CharRange 40 41)))
    (check-equal? (send declaration-service definition-at 35)
                  (CharRange 30 31))

    (send trace expand 0 2)
    (check-equal? (send declaration-service occurrence-at 37)
                  (CharRange 37 38))
    (check-equal? (send declaration-service uses-at 12)
                  (list (CharRange 12 13) (CharRange 22 23)))
    (check-equal? (send declaration-service definition-at 12)
                  (CharRange 7 8))
    (check-equal? (send declaration-service definition-at 37)
                  (CharRange 32 33))
    (check-equal? (send declaration-service uses-at 37)
                  (list (CharRange 37 38) (CharRange 42 43))))

  (test-case
    "declaration collection is independent of callback order"
    (define path (string->path "/tmp/binding-callback-order-test.rkt"))
    (define text (make-string 40 #\space))
    (define doc-text (new lsp-editor%))
    (send doc-text insert text 0)
    (define trace
      (new build-trace%
        [src path]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text (path->uri path))]))

    ;; Same-file jump first, then attach the definition's module identity.
    (send trace
          syncheck:add-jump-to-definition/phase-level+space
          path 10 11 'arrow-first path '() 0)
    (send trace
          syncheck:add-definition-target/phase-level+space
          path 5 6 'arrow-first '() 0)

    ;; Definition first, then collect its same-file jump.
    (send trace
          syncheck:add-definition-target/phase-level+space
          path 20 21 'definition-first '() 0)
    (send trace
          syncheck:add-jump-to-definition/phase-level+space
          path 25 26 'definition-first path '() 0)
    ;; A target with no arrows still has a queryable definition occurrence.
    (send trace
          syncheck:add-definition-target/phase-level+space
          path 30 31 'unused '() 0)

    (define declaration-service (send trace get-declaration))
    (define arrow-first-binding (Module-Binding path '() 0 'arrow-first))
    (define definition-first-binding (Module-Binding path '() 0 'definition-first))
    (for ([position (in-list (list 5 10))])
      (check-equal? (send declaration-service module-binding-at position)
                    arrow-first-binding))
    (for ([position (in-list (list 20 25))])
      (check-equal? (send declaration-service module-binding-at position)
                    definition-first-binding))
    (check-equal? (send declaration-service definition-at 10)
                  (CharRange 5 6))
    (check-equal? (send declaration-service definition-at 25)
                  (CharRange 20 21))
    (check-equal? (send declaration-service uses-at 5)
                  (list (CharRange 10 11)))
    (check-equal? (send declaration-service uses-at 20)
                  (list (CharRange 25 26)))
    (check-equal? (send declaration-service occurrence-at 30)
                  (CharRange 30 31))
    (check-equal? (send declaration-service definition-at 30)
                  (CharRange 30 31))
    (check-equal? (send declaration-service module-binding-at 30)
                  (Module-Binding path '() 0 'unused))
    (check-equal? (send declaration-service uses-at 30) '())
    (check-equal?
      (for/hash ([entry (in-list (send declaration-service module-binding-uses))])
        (values (car entry) (cdr entry)))
      (hash (CharRange 10 11) arrow-first-binding
            (CharRange 25 26) definition-first-binding)))

  (test-case
    "a declaration moves once after an edit inside its range"
    (define path (string->path "/tmp/binding-declaration-edit-test.rkt"))
    (define text (make-string 50 #\space))
    (define doc-text (new lsp-editor%))
    (send doc-text insert text 0)
    (define trace
      (new build-trace%
        [src path]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text (path->uri path))]))
    (send trace
          syncheck:add-arrow/name-dup
          path 30 33 path 35 38 #t 0 #f #f)
    (define declaration-service (send trace get-declaration))

    ;; Interior insertions extend the retained declaration range. Deleting the
    ;; same text restores it, and the following edit must still move it once.
    (send trace expand 31 32)
    (check-equal? (send declaration-service occurrence-at 30)
                  (CharRange 30 34))
    (check-equal? (send declaration-service definition-at 33)
                  (CharRange 30 34))
    (check-equal? (send declaration-service occurrence-at 31)
                  (CharRange 30 34))

    (send trace contract 31 32)
    (check-equal? (send declaration-service occurrence-at 30)
                  (CharRange 30 33))
    (check-equal? (send declaration-service definition-at 30)
                  (CharRange 30 33))

    (send trace expand 0 2)
    (check-equal? (send declaration-service occurrence-at 32)
                  (CharRange 32 35))
    (check-equal? (send declaration-service definition-at 32)
                  (CharRange 32 35))
    (check-equal? (send declaration-service uses-at 32)
                  (list (CharRange 37 40))))

  (test-case
    "a declaration replays several edits when first read"
    (define path (string->path "/tmp/binding-declaration-replay-test.rkt"))
    (define text (make-string 60 #\space))
    (define doc-text (new lsp-editor%))
    (send doc-text insert text 0)
    (define trace
      (new build-trace%
        [src path]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text (path->uri path))]))
    (send trace
          syncheck:add-arrow/name-dup
          path 30 33 path 40 43 #t 0 #f #f)
    (define declaration-service (send trace get-declaration))

    (send trace expand 0 2)
    (send trace expand 33 35)
    (send trace contract 34 35)

    (check-equal? (send declaration-service occurrence-at 32)
                  (CharRange 32 36))
    (check-equal? (send declaration-service definition-at 32)
                  (CharRange 32 36))
    (check-equal? (send declaration-service uses-at 32)
                  (list (CharRange 43 46))))

  (test-case
    "deleting declaration text clips and then removes its local binding"
    (define path (string->path "/tmp/binding-declaration-delete-test.rkt"))
    (define text (make-string 60 #\space))
    (define doc-text (new lsp-editor%))
    (send doc-text insert text 0)
    (define trace
      (new build-trace%
        [src path]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text (path->uri path))]))
    (send trace
          syncheck:add-arrow/name-dup
          path 30 35 path 40 43 #t 0 #f #f)
    (define declaration-service (send trace get-declaration))

    (send trace contract 32 37)
    (check-equal? (send declaration-service definition-at 35)
                  (CharRange 30 32))

    (send trace contract 30 32)
    (check-equal? (send declaration-service occurrence-at 33)
                  (CharRange 33 36))
    (check-false (send declaration-service definition-at 33)))

  (test-case
    "deleting a module declaration preserves its position-free binding"
    (define path (string->path "/tmp/module-declaration-delete-test.rkt"))
    (define text (make-string 40 #\space))
    (define doc-text (new lsp-editor%))
    (send doc-text insert text 0)
    (define trace
      (new build-trace%
        [src path]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text (path->uri path))]))
    (send trace
          syncheck:add-definition-target/phase-level+space
          path 10 11 'module-name '() 0)
    (send trace
          syncheck:add-arrow/name-dup
          path 10 11 path 20 21 #t 0 #f #f)
    (define declaration-service (send trace get-declaration))
    (define module-binding (Module-Binding path '() 0 'module-name))

    (send trace contract 10 11)

    (check-equal? (send declaration-service module-binding-at 19)
                  module-binding)
    (check-equal? (send declaration-service uses-at 19)
                  (list (CharRange 19 20)))
    (check-false (send declaration-service definition-at 19)))

  (test-case
    "failed expansion preserves the accepted contribution"
    (define d
      (make-doc "file:///tmp/contribution-lifecycle-test.rkt"
                "#lang racket/base\n(require racket/list)\nfirst\n"))
    (check-true (doc-expand! d))
    (define accepted-contribution (Doc-contribution d))
    (check-equal? (Doc-Contribution-path accepted-contribution)
                  (string->path "/tmp/contribution-lifecycle-test.rkt"))
    (check-true
      (positive? (hash-count (Doc-Contribution-references accepted-contribution))))
    (define accepted-pos (Pos 2 0))
    (define accepted-target (doc-module-binding-at d accepted-pos))

    (doc-apply-edit! d (Range (Pos 3 0) (Pos 3 0)) "(")
    (check-false (doc-expand! d))
    (check-eq? (Doc-contribution d) accepted-contribution)
    (define preserved-target (doc-module-binding-at d accepted-pos))
    (check-equal? preserved-target accepted-target)

    (doc-reset! d "#lang racket/base\n42\n")
    (check-true (doc-expand! d))
    (check-not-eq? (Doc-contribution d) accepted-contribution))

  ;; Tests for newly extracted doc-* functions
  ;; All use a common expanded document:
  ;;   #lang racket
  ;;   (define x 1)
  ;;   x

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
                   [doc-text (new lsp-editor%)]
                   [lexer-state (build-lexer-state text uri)])
        (define/override (get-completions) '())
        (define/override (get-online-completions str-before-cursor)
          (hash-ref prefix->completions str-before-cursor '()))))
    (define trace (new test-trace%))
    (doc-update-trace! d
                       trace
                       (send trace get-contribution)
                       (Doc-version d))
    d)

  (test-case
    "position queries on a binding usage"
    (define-values (d _uri) (make-expanded-doc))
    ;; "x" at line 2, char 0 resolves to its same-file module identity.
    (check-equal? (doc-occurrence-at d (Pos 2 0))
                  (Range (Pos 2 0) (Pos 2 1)))
    (define target (doc-module-binding-at d (Pos 2 0)))
    (check-true (Module-Binding? target))
    (check-equal? (Module-Binding-id target) 'x))

  (test-case
    "position queries on the definition site"
    (define-values (d _uri) (make-expanded-doc))
    ;; "x" in (define x 1) at line 1, char 8
    (check-equal? (doc-occurrence-at d (Pos 1 8))
                  (Range (Pos 1 8) (Pos 1 9)))
    (define target (doc-module-binding-at d (Pos 1 8)))
    (check-true (Module-Binding? target))
    (check-equal? (Module-Binding-id target) 'x))

  (test-case
    "position queries on imported 'define'"
    (define-values (d _uri) (make-expanded-doc))
    ;; "define" at line 1, char 1 is an imported symbol
    (check-equal? (doc-occurrence-at d (Pos 1 1))
                  (Range (Pos 1 1) (Pos 1 7)))
    (define target (doc-module-binding-at d (Pos 1 1)))
    (check-true (Module-Binding? target))
    (check-equal? (Module-Binding-submods target) '())
    (check-equal? (Module-Binding-phase+space target) 0)
    ;; Check Syntax reports the target identifier, which may differ from the
    ;; source spelling after a rename transformer.
    (check-equal? (Module-Binding-id target) 'new-define))

  (test-case
    "position queries on literal return no occurrence"
    (define-values (d _uri) (make-expanded-doc))
    ;; "1" at line 1, char 10 is a literal
    (check-false (doc-occurrence-at d (Pos 1 10)))
    (check-false (doc-module-binding-at d (Pos 1 10))))

  (test-case
    "uses-at returns usage ranges for local x"
    (define-values (d _uri) (make-expanded-doc))
    (define bindings (doc-uses-at d (Pos 2 0)))
    ;; Should contain exactly the usage of "x" at line 2, char 0..1
    (check-equal? (length bindings) 1)
    (check-equal? (first bindings)
                  (Range (Pos 2 0) (Pos 2 1))))

  (test-case
    "binding-ranges-at includes definition then uses for local x"
    (define-values (d _uri) (make-expanded-doc))
    (check-equal? (doc-binding-ranges-at d (Pos 2 0))
                  (list (Range (Pos 1 8) (Pos 1 9))
                        (Range (Pos 2 0) (Pos 2 1)))))

  (test-case
    "binding-ranges-at for imported define is uses only"
    (define-values (d _uri) (make-expanded-doc))
    (check-false (doc-definition-at d (Pos 1 1)))
    (check-equal? (doc-binding-ranges-at d (Pos 1 1))
                  (doc-uses-at d (Pos 1 1))))

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
    "doc-references returns a live source for same-document x"
    (define-values (d uri) (make-expanded-doc))
    ;; "x" at (2,0) resolves to a module binding and one live use in this document.
    (define result (doc-references d uri (Pos 2 0) #t))
    (check-true (Module-Binding? (Document-Reference-Result-module-binding result)))
    (define source (Document-Reference-Result-source result))
    (check-equal? (Reference-Source-path source) (uri->path uri))
    (check-equal? (map Location-range (Reference-Source-locations source))
                  (list (Range (Pos 1 8) (Pos 1 9))
                        (Range (Pos 2 0) (Pos 2 1))))

    (define uses-only (doc-references d uri (Pos 2 0) #f))
    (check-equal?
      (map Location-range
           (Reference-Source-locations
             (Document-Reference-Result-source uses-only)))
      (list (Range (Pos 2 0) (Pos 2 1)))))

  (test-case
    "doc-references preserves local binding behavior"
    (define uri "file:///tmp/doc-local-reference-test.rkt")
    (define d
      (make-doc uri
                "#lang racket\n(let ([x 1])\n  x\n  x)\n"))
    (check-true (doc-expand! d))

    (define with-declaration (doc-references d uri (Pos 2 2) #t))
    (check-false (Document-Reference-Result-module-binding with-declaration))
    (check-equal?
      (map Location-range
           (Reference-Source-locations
             (Document-Reference-Result-source with-declaration)))
      (list (Range (Pos 1 7) (Pos 1 8))
            (Range (Pos 2 2) (Pos 2 3))
            (Range (Pos 3 2) (Pos 3 3))))

    (define uses-only (doc-references d uri (Pos 2 2) #f))
    (check-equal?
      (map Location-range
           (Reference-Source-locations
             (Document-Reference-Result-source uses-only)))
      (list (Range (Pos 2 2) (Pos 2 3))
            (Range (Pos 3 2) (Pos 3 3)))))

  (test-case
    "doc-references shifts every live range while the accepted source stays stale"
    (define uri "file:///tmp/doc-reference-edit-test.rkt")
    (define d
      (make-doc uri
                "#lang racket\n(define x 1)\nx\nx\n"))
    (check-true (doc-expand! d))
    (define accepted-contribution (Doc-contribution d))
    (define before-edit (doc-references d uri (Pos 2 0) #t))
    (define module-binding
      (Document-Reference-Result-module-binding before-edit))
    (define accepted-ranges
      (sort
        (map Location-range
             (hash-ref (Doc-Contribution-references accepted-contribution)
                       module-binding))
        <
        #:key (lambda (range)
                (Pos-line (Range-start range)))))
    (check-equal?
      accepted-ranges
      (list (Range (Pos 2 0) (Pos 2 1))
            (Range (Pos 3 0) (Pos 3 1))))

    (doc-apply-edit! d (Range (Pos 1 0) (Pos 1 0)) ";; shift\n")

    (define after-edit (doc-references d uri (Pos 3 0) #t))
    (check-eq? (Doc-contribution d) accepted-contribution)
    (check-equal? (Document-Reference-Result-module-binding after-edit)
                  module-binding)
    (define live-ranges
      (map Location-range
           (Reference-Source-locations
             (Document-Reference-Result-source after-edit))))
    (check-equal?
      live-ranges
      (list (Range (Pos 2 8) (Pos 2 9))
            (Range (Pos 3 0) (Pos 3 1))
            (Range (Pos 4 0) (Pos 4 1)))))

  (test-case
    "doc-highlights for local x"
    (define-values (d _uri) (make-expanded-doc))
    ;; "x" usage at (2,0): highlights should include both declaration and usage
    (define result (doc-highlights d (Pos 2 0)))
    (check-equal? (length result) 2)
    ;; First highlight: the declaration at line 1
    (check-equal? (DocumentHighlight-range (first result))
                  (Range (Pos 1 8) (Pos 1 9)))
    ;; Second highlight: the usage at line 2
    (check-equal? (DocumentHighlight-range (second result))
                  (Range (Pos 2 0) (Pos 2 1))))

  (test-case
    "doc-highlights returns empty for quoted data"
    (define d
      (make-doc "file:///tmp/doc-quoted-data-highlight-test.rkt"
                (string-append "#lang racket\n"
                               "(define x 1)\n"
                               "x\n"
                               "(quote x)\n"
                               "'x\n")))
    (check-true (doc-expand! d))
    (check-equal? (doc-highlights d (Pos 3 7)) '())
    (check-equal? (doc-highlights d (Pos 4 1)) '()))

  (test-case
    "doc-highlights keeps unused shadowed binders separate"
    (define uri "file:///tmp/doc-unused-binder-highlight-test.rkt")
    (define d
      (make-doc uri
                (string-append "#lang racket\n"
                               "(define x 1)\n"
                               "(let ([x 0])\n"
                               "  1)\n"
                               "(lambda (x) 1)\n")))
    (check-true (doc-expand! d))

    (define module-range (Range (Pos 1 8) (Pos 1 9)))
    (define let-range (Range (Pos 2 7) (Pos 2 8)))
    (define lambda-range (Range (Pos 4 9) (Pos 4 10)))
    ;; Check Syntax before Racket 8.11 does not report unused lexical binders.
    (define expected-let-ranges
      (if unused-binder-callbacks? (list let-range) '()))
    (define expected-lambda-ranges
      (if unused-binder-callbacks? (list lambda-range) '()))
    (check-equal? (map DocumentHighlight-range (doc-highlights d (Pos 1 8)))
                  (list module-range))
    (check-equal? (map DocumentHighlight-range (doc-highlights d (Pos 2 7)))
                  expected-let-ranges)
    (check-equal? (map DocumentHighlight-range (doc-highlights d (Pos 4 9)))
                  expected-lambda-ranges)
    (check-true (Module-Binding? (doc-module-binding-at d (Pos 1 8))))
    (check-false (doc-module-binding-at d (Pos 2 7)))
    (check-false (doc-module-binding-at d (Pos 4 9))))

  (test-case
    "doc-highlights uses non-empty module-language arrow ranges"
    (define uri "file:///tmp/doc-module-language-highlight-test.rkt")
    (define d
      (make-doc uri
                (string-append "#lang racket\n"
                               "(define i+1-th 0)\n"
                               "(+ 1 2)\n"
                               "(+ i+1-th 3)\n")))
    (check-true (doc-expand! d))
    (define result (doc-highlights d (Pos 2 1)))
    (check-equal? (map DocumentHighlight-range result)
                  (list (Range (Pos 2 1) (Pos 2 2))
                        (Range (Pos 3 1) (Pos 3 2)))))

  (test-case
    "doc-highlights accepts quote ranges from module-language arrows"
    (define uri "file:///tmp/doc-module-language-quote-test.rkt")
    (define d
      (make-doc uri
                "#lang racket\n'(1 2)\n'(3 4)\n"))
    (check-true (doc-expand! d))
    (check-equal?
      (map DocumentHighlight-range (doc-highlights d (Pos 1 0)))
      (list (Range (Pos 1 0) (Pos 1 1))
            (Range (Pos 2 0) (Pos 2 1)))))

  (test-case
    "module-language uses stay local, non-defining, and binding-aware"
    (define uri "file:///tmp/doc-module-language-use-test.rkt")
    (define d
      (make-doc uri
                (string-append "#lang racket\n"
                               "(define i+1-th 0)\n"
                               "(+ 1 2)\n"
                               "(* 3 4)\n"
                               "(let ([+ -])\n"
                               "  (+ 5 2))\n"
                               "(+ i+1-th 3)\n")))
    (check-true (doc-expand! d))

    (define imported-plus-ranges
      (list (Range (Pos 2 1) (Pos 2 2))
            (Range (Pos 6 1) (Pos 6 2))))
    (check-equal?
      (map DocumentHighlight-range (doc-highlights d (Pos 2 1)))
      imported-plus-ranges)
    (check-equal?
      (map DocumentHighlight-range (doc-highlights d (Pos 3 1)))
      (list (Range (Pos 3 1) (Pos 3 2))))
    (check-equal?
      (map DocumentHighlight-range (doc-highlights d (Pos 5 3)))
      (list (Range (Pos 4 7) (Pos 4 8))
            (Range (Pos 5 3) (Pos 5 4))))

    (define references (doc-references d uri (Pos 2 1) #t))
    (check-false (Document-Reference-Result-module-binding references))
    (check-equal?
      (map Location-range
           (Reference-Source-locations
             (Document-Reference-Result-source references)))
      imported-plus-ranges)
    (check-false (doc-definition d uri (Pos 2 1)))
    (check-false (doc-prepare-rename d (Pos 2 1)))
    (check-false (doc-rename d uri (Pos 2 1) "plus"))

    (define contribution-ranges
      (for*/list ([locations (in-hash-values
                               (Doc-Contribution-references (Doc-contribution d)))]
                  [location (in-list locations)])
        (Location-range location)))
    (for ([range (in-list imported-plus-ranges)])
      (check-false (member range contribution-ranges)))

    (doc-apply-edit! d (Range (Pos 2 0) (Pos 2 0)) ";; shift\n")
    (check-equal?
      (map DocumentHighlight-range (doc-highlights d (Pos 3 1)))
      (list (Range (Pos 3 1) (Pos 3 2))
            (Range (Pos 7 1) (Pos 7 2)))))

  (test-case
    "module-language use collection filters and defers endpoints"
    (define path (string->path "/tmp/module-language-use-service-test.rkt"))
    (define uri (path->uri path))
    (define text "racket + * ' word")
    (define doc-text (new lsp-editor%))
    (send doc-text insert text 0)
    (define trace
      (new build-trace%
        [src path]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text uri)]))
    (define declaration-service (send trace get-declaration))

    (send trace syncheck:add-arrow/name-dup path 0 6 path 7 8 #t 0 'module-lang #f)
    (send trace syncheck:add-arrow/name-dup path 0 6 path 9 10 #t 0 'module-lang #f)
    (send trace syncheck:add-arrow/name-dup path 0 6 path 11 12 #t 0 'module-lang #f)
    (send trace syncheck:add-arrow/name-dup path 0 6 path 13 13 #t 0 'module-lang #f)
    (send trace syncheck:add-arrow/name-dup path 0 6 path 13 17 #t 0 #t #f)
    (send declaration-service walk-stx #f)

    (check-equal? (send declaration-service occurrence-at 7)
                  (CharRange 7 8))
    (check-equal? (send declaration-service occurrence-at 9)
                  (CharRange 9 10))
    (check-equal? (send declaration-service occurrence-at 11)
                  (CharRange 11 12))
    (check-false (send declaration-service occurrence-at 13))
    (check-equal? (send declaration-service uses-at 7)
                  (list (CharRange 7 8)))
    (check-false (send declaration-service definition-at 7))
    (check-false (send declaration-service module-binding-at 7)))

  (test-case
    "exact jumps win over pending module-language uses"
    (define path (string->path "/tmp/module-language-use-jump-test.rkt"))
    (define uri (path->uri path))
    (define text "racket exact")
    (define doc-text (new lsp-editor%))
    (send doc-text insert text 0)
    (define trace
      (new build-trace%
        [src path]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text uri)]))
    (send trace syncheck:add-arrow/name-dup path 0 6 path 7 12 #t 0 'module-lang #f)
    (send trace
          syncheck:add-jump-to-definition/phase-level+space
          path 7 12 'exact path '(provider) 0)
    (define declaration-service (send trace get-declaration))
    (send declaration-service walk-stx #f)
    (check-equal? (send declaration-service module-binding-at 7)
                  (Module-Binding path '(provider) 0 'exact))
    (check-equal? (send declaration-service uses-at 7)
                  (list (CharRange 7 12))))

  (test-case
    "explicit import variants keep exact full-token binding ranges"
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
      (check-equal? (doc-occurrence-at d use-start)
                    (Range use-start use-end))
      (check-true (Module-Binding? (doc-module-binding-at d use-start)))
      (check-false (doc-occurrence-at d import-position))
      (check-false (doc-definition-at d import-position))))

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
    "Hover card labels a mouse-over status without definition"
    (check-equal?
      (render-hover-card
        (Hover-Card #f
                    #f
                    #f
                    (Hover-Annotation 'mouse-over-status
                                      "bound occurrence of count")
                    #f))
      "**Mouse-over status**\n\nbound occurrence of count"))

  (test-case
    "Hover card renders the fixed slot stack"
    (check-equal?
      (render-hover-card
        (Hover-Card
          #f
          #f
          (Hover-Definition
            'source
            (Hover-Code-Summary "(parse-config raw)" "racket"))
          #f
          (Hover-Documentation "Parse a configuration value."
                               "https://docs.example.test/parse-config")))
#<<END
**Source**
```racket
(parse-config raw)
```

**Documentation** | [Online docs](https://docs.example.test/parse-config)

Parse a configuration value.
END
))

  (test-case
    "Hover card omits empty sections and docs separator"
    (check-equal?
      (render-hover-card
        (Hover-Card #f
                    #f
                    #f
                    #f
                    (Hover-Documentation ""
                                         "https://docs.example.test/parse-config")))
      "**Documentation** | [Online docs](https://docs.example.test/parse-config)"))

  (test-case
    "Hover card separates documentation body without an online link"
    (check-equal?
      (render-hover-card
        (Hover-Card #f
                    #f
                    #f
                    #f
                    (Hover-Documentation "Parse a configuration value."
                                         #f)))
      "**Documentation**\n\nParse a configuration value."))

  (test-case
    "Hover card labels a lone Rhombus source fence"
    (check-equal?
      (render-hover-card
        (Hover-Card #f
                    #f
                    (Hover-Definition
                      'source
                      (Hover-Code-Summary "fun parse_config(raw): raw" "rhombus"))
                    #f
                    #f))
      (string-append
        "**Source**\n"
        "```rhombus\nfun parse_config(raw): raw\n```")))

  (test-case
    "Hover card labels definition when a type precedes it"
    (check-equal?
      (render-hover-card
        (Hover-Card
          (Hover-Code-Summary "Integer" "racket")
          #t
          (Hover-Definition
            'source
            (Hover-Code-Summary "(define count 1)" "racket"))
          #f
          #f))
      (string-append
        "**Type (stale)**\n"
        "```racket\nInteger\n```\n\n"
        "**Source**\n"
        "```racket\n(define count 1)\n```")))

  (test-case
    "build-hover-card keeps a non-empty annotation beside type"
    (define card
      (build-hover-card #:type-text "Integer"
                        #:type-stale? #f
                        #:annotation
                        (Hover-Annotation 'mouse-over-status
                                          " \n Integer \t")
                        #:link #f
                        #:signature #f
                        #:source-summary #f
                        #:documentation-text #f))
    (check-equal? (Hover-Card-annotation card)
                  (Hover-Annotation 'mouse-over-status
                                    " \n Integer \t"))
    (check-equal? (render-hover-card card)
                  (string-append
                    "**Type**\n"
                    "```racket\nInteger\n```\n\n"
                    "**Mouse-over status**\n\n"
                    " \n Integer \t")))

  (test-case
    "build-hover-card omits an empty annotation"
    (define card
      (build-hover-card #:type-text #f
                        #:type-stale? #f
                        #:annotation (Hover-Annotation 'log-tooltip "")
                        #:link #f
                        #:signature #f
                        #:source-summary #f
                        #:documentation-text #f))
    (check-false (Hover-Card-annotation card)))

  (test-case
    "build-hover-card keeps annotations beside richer slots"
    (define with-type
      (build-hover-card #:type-text "Integer"
                        #:type-stale? #f
                        #:annotation
                        (Hover-Annotation 'mouse-over-status
                                          "bound occurrence")
                        #:link #f
                        #:signature #f
                        #:source-summary #f
                        #:documentation-text #f))
    (check-equal? (Hover-Card-annotation with-type)
                  (Hover-Annotation 'mouse-over-status
                                    "bound occurrence"))
    (check-equal?
      (render-hover-card with-type)
      (string-append
        "**Type**\n"
        "```racket\nInteger\n```\n\n"
        "**Mouse-over status**\n\n"
        "bound occurrence"))
    (define with-source
      (build-hover-card #:type-text #f
                        #:type-stale? #f
                        #:annotation
                        (Hover-Annotation 'mouse-over-status
                                          "1 bound occurrence")
                        #:link #f
                        #:signature #f
                        #:source-summary (Hover-Code-Summary "(define count 1)" "racket")
                        #:documentation-text #f))
    (check-equal? (Hover-Card-annotation with-source)
                  (Hover-Annotation 'mouse-over-status
                                    "1 bound occurrence"))
    (check-equal?
      (render-hover-card with-source)
      (string-append
        "**Source**\n"
        "```racket\n(define count 1)\n```\n\n"
        "**Mouse-over status**\n\n"
        "1 bound occurrence"))
    (define with-docs
      (build-hover-card #:type-text #f
                        #:type-stale? #f
                        #:annotation
                        (Hover-Annotation 'log-tooltip
                                          "imported from racket")
                        #:link "https://docs.example.test/map"
                        #:signature #f
                        #:source-summary #f
                        #:documentation-text "Applies proc."))
    (check-equal? (Hover-Card-annotation with-docs)
                  (Hover-Annotation 'log-tooltip
                                    "imported from racket"))
    (check-equal?
      (render-hover-card with-docs)
      (string-append
        "**Log tooltip**\n\n"
        "imported from racket\n\n"
        "**Documentation** | [Online docs](https://docs.example.test/map)\n\n"
        "Applies proc.")))

  (test-case
    "build-hover-card keeps annotation-only cards"
    (define card
      (build-hover-card #:type-text #f
                        #:type-stale? #f
                        #:annotation
                        (Hover-Annotation 'mouse-over-status
                                          "bound occurrence")
                        #:link #f
                        #:signature #f
                        #:source-summary #f
                        #:documentation-text #f))
    (check-equal? (Hover-Card-annotation card)
                  (Hover-Annotation 'mouse-over-status
                                    "bound occurrence"))
    (check-equal? (render-hover-card card)
                  "**Mouse-over status**\n\nbound occurrence"))

  (test-case
    "build-hover-card prefers source over signature"
    (define typed-signature
      (build-hover-card #:type-text "Integer"
                        #:type-stale? #f
                        #:annotation #f
                        #:link #f
                        #:signature "(add1 z) -> number?"
                        #:source-summary #f
                        #:documentation-text #f))
    (check-equal?
      (render-hover-card typed-signature)
      (string-append
        "**Type**\n"
        "```racket\nInteger\n```\n\n"
        "**Signature**\n"
        "```racket\n(add1 z) -> number?\n```"))
    (define source-over-signature
      (build-hover-card #:type-text #f
                        #:type-stale? #f
                        #:annotation #f
                        #:link #f
                        #:signature "(add1 z) -> number?"
                        #:source-summary (Hover-Code-Summary "(define count 1)" "racket")
                        #:documentation-text #f))
    (check-equal?
      (Hover-Definition-kind (Hover-Card-definition source-over-signature))
      'source)
    (check-equal?
      (render-hover-card source-over-signature)
      (string-append
        "**Source**\n"
        "```racket\n(define count 1)\n```")))

  (test-case
    "build-hover-card labels a lone signature"
    (define card
      (build-hover-card #:type-text #f
                        #:type-stale? #f
                        #:annotation #f
                        #:link #f
                        #:signature "(add1 z) -> number?"
                        #:source-summary #f
                        #:documentation-text #f))
    (check-equal?
      (render-hover-card card)
      (string-append
        "**Signature**\n"
        "```racket\n(add1 z) -> number?\n```")))

  (test-case
    "build-hover-card marks stale only when a type is present"
    (check-false
      (Hover-Card-type-stale?
        (build-hover-card #:type-text #f
                          #:type-stale? #t
                          #:annotation
                          (Hover-Annotation 'mouse-over-status
                                            "bound occurrence")
                          #:link #f
                          #:signature #f
                          #:source-summary #f
                          #:documentation-text #f)))
    (check-true
      (Hover-Card-type-stale?
        (build-hover-card #:type-text "Integer"
                          #:type-stale? #t
                          #:annotation #f
                          #:link #f
                          #:signature #f
                          #:source-summary #f
                          #:documentation-text #f))))

  (test-case
    "Document hover renders a docs-backed card"
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
    (check-true (string-contains? result "```racket"))
    (check-true (string-contains? result "Returns a newly allocated list"))
    (check-true (string-contains? result "[Online docs]"))
    (check-true (string-contains? result "imported from"))
    (define imported-from-start
      (caar (regexp-match-positions #rx"imported from" result)))
    (define online-docs-start
      (caar (regexp-match-positions #rx"\\[Online docs\\]" result)))
    (check-true (< imported-from-start online-docs-start)))

  (test-case
    "Typed Racket hover renders inferred types, source, ranges, and stale state"
    (when (typed-racket-available?)
      (define d
        (make-doc
          "file:///tmp/typed-racket-hover-test.rkt"
          (string-append
            "#lang typed/racket\n"
            "(: count Integer)\n"
            "(define count 1)\n"
            "(+ count 2)\n")))
      (check-true (doc-expand! d))

      (define use-hover
        (doc-hover d (Pos 3 4)))
      (check-not-false use-hover)
      (check-equal? (Hover-range use-hover)
                    (Range (Pos 3 3) (Pos 3 8)))
      (check-equal?
        (Hover-contents use-hover)
        (string-append
          "**Type**\n"
          "```racket\nInteger\n```\n\n"
          "**Source**\n"
          "```racket\n(define count 1)\n```"))

      ;; Literal and delimiter tooltips remain independently addressable.
      (define open-paren-hover
        (doc-hover d (Pos 3 0)))
      (check-equal? (Hover-range open-paren-hover)
                    (Range (Pos 3 0) (Pos 3 1)))
      (check-true
        (string-contains? (Hover-contents open-paren-hover) "**Type**"))
      (define literal-hover
        (doc-hover d (Pos 3 9)))
      (check-equal? (Hover-range literal-hover)
                    (Range (Pos 3 9) (Pos 3 10)))
      (check-true
        (string-contains? (Hover-contents literal-hover) "**Type**"))
      (check-equal? (Hover-range (doc-hover d (Pos 3 10)))
                    (Range (Pos 3 10) (Pos 3 11)))

      (doc-apply-edit! d
                       (Range (Pos 3 9) (Pos 3 10))
                       "3")
      (doc-update-version! d 1)
      (define stale-hover
        (doc-hover d (Pos 3 4)))
      (check-not-false stale-hover)
      (check-true
        (string-prefix? (Hover-contents stale-hover)
                        "**Type (stale)**"))))

  (test-case
    "Typed Racket hover keeps shifted type ranges while a trace is stale"
    (when (typed-racket-available?)
      (define d
        (make-doc
          "file:///tmp/typed-racket-stale-shift-test.rkt"
          (string-append
            "#lang typed/racket\n"
            "(: count Integer)\n"
            "(define count 1)\n"
            "(+ count 2)\n")))
      (check-true (doc-expand! d))
      (doc-apply-edit! d (Range (Pos 3 0) (Pos 3 0)) "    ")
      (doc-update-version! d 1)
      (define shifted-hover
        (doc-hover d (Pos 3 8)))
      (check-not-false shifted-hover)
      (check-equal? (Hover-range shifted-hover)
                    (Range (Pos 3 7) (Pos 3 12)))
      (check-true
        (string-prefix? (Hover-contents shifted-hover)
                        "**Type (stale)**"))
      (check-true
        (string-contains? (Hover-contents shifted-hover)
                          "(define count 1)"))))

  (test-case
    "Typed Racket import hover shows type before signature"
    (when (typed-racket-available?)
      (define d
        (make-doc
          "file:///tmp/typed-racket-import-hover-test.rkt"
          "#lang typed/racket\n(add1 1)\n"))
      (check-true (doc-expand! d))
      (define hover
        (doc-hover d (Pos 1 1)))
      (check-not-false hover)
      (define contents
        (Hover-contents hover))
      (define type-pos
        (car (regexp-match-positions #rx"\\*\\*Type\\*\\*" contents)))
      (define signature-pos
        (car (regexp-match-positions #rx"\\*\\*Signature\\*\\*" contents)))
      (check-not-false type-pos)
      (check-not-false signature-pos)
      (check-true (< (car type-pos) (car signature-pos)))
      (check-true (string-contains? contents "```racket"))
      (check-false (string-contains? contents "**Source**"))
      (check-true (string-contains? contents "imported from"))))

  (test-case
    "Untyped documents do not gain Typed Racket type cards"
    (define d
      (make-doc
        "file:///tmp/untyped-no-type-card-test.rkt"
        "#lang racket\n(define count 1)\n(+ count 2)\n"))
    (check-true (doc-expand! d))
    (define hover
      (doc-hover d (Pos 2 4)))
    (check-not-false hover)
    (check-false
      (string-contains? (Hover-contents hover) "**Type**"))
    (check-true
      (string-contains? (Hover-contents hover) "```racket")))

  (test-case
    ;; Type-error tooltips must arrive via typed-racket%, not diag%.
    "Typed Racket tooltip decoder routes type errors into diagnostics"
    (when (typed-racket-available?)
      (define diags
        (check-syntax-diagnostics
          "file:///tmp/typed-racket-error-test.rkt"
          "#lang typed/racket\n(+ 1 \"x\")\n"))
      (define typed-diag
        (for/first ([diag (in-list diags)]
                    #:when (string=? (Diagnostic-source diag)
                                     "Typed Racket"))
          diag))
      (check-not-false typed-diag)
      (check-equal? (Diagnostic-range typed-diag)
                    (Range (Pos 1 5) (Pos 1 8)))
      (check-true
        (string-contains? (Diagnostic-message typed-diag)
                          "expected: Number"))
      ;; Exception text wraps the tooltip message; publish only the tooltip.
      (check-false
        (for/or ([diag (in-list diags)])
          (and (string=? (Diagnostic-source diag) "Racket")
               (string-contains? (Diagnostic-message diag)
                                 "Type Checker"))))))

  (test-case
    "Document hover renders collapsed same-file headers for declarations and uses"
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

    ;; No hover annotation. Source detail alone opens the card and sets range.
    (define use-hover (doc-hover d (Pos 2 5)))
    (check-not-false use-hover)
    (check-equal? (Hover-range use-hover)
                  (Range (Pos 2 5) (Pos 2 10)))
    (check-equal? (Hover-contents use-hover)
                  (string-append
                    "**Source**\n"
                    "```racket\n(let ([count (length (list 1))])\n  ...\n```"))

    ;; Headers and macro binders get outer context. Binder names are not matched.
    (check-equal?
      (Hover-contents (doc-hover d (Pos 3 9)))
      (string-append
        "**Source**\n"
        "```racket\n(define (parse-config raw) raw)\n```\n\n"
        "**Mouse-over status**\n\n"
        "1 bound occurrence"))
    ;; A function use shows the declaration, not the call site.
    (check-equal?
      (Hover-contents (doc-hover d (Pos 5 2)))
      (string-append
        "**Source**\n"
        "```racket\n(define (parse-config raw) raw)\n```"))
    (check-equal?
      (Hover-contents (doc-hover d (Pos 4 12)))
      (string-append
        "**Source**\n"
        "```racket\n(for/list ([element (list 1)]) element)\n```\n\n"
        "**Mouse-over status**\n\n"
        "1 bound occurrence")))

  (test-case
    "Document hover balances compact clauses with collapsed headers"
    (define d
      (make-doc
        "file:///tmp/hover-detail-header.rkt"
        "#lang racket\n(let ([count (length (list 1))]\n      [limit 10])\n  (+ count limit))\n(define (fib\n         n)\n  n)\n(define answer (string-length \"input\"))\n(define (parse raw) ; accepts overrides\n  raw)\n"))
    (check-true (doc-expand! d))

    ;; A later binding gets its own clause, not a prefix with earlier bindings.
    (check-equal?
      (Hover-contents (doc-hover d (Pos 2 7)))
      (string-append
        "**Source**\n"
        "```racket\n[limit 10]\n```\n\n"
        "**Mouse-over status**\n\n"
        "1 bound occurrence"))
    ;; No same-line header. Show the full nearest form.
    (check-equal?
      (Hover-contents (doc-hover d (Pos 5 9)))
      (string-append
        "**Source**\n"
        "```racket\n(fib\n         n)\n```\n\n"
        "**Mouse-over status**\n\n"
        "1 bound occurrence"))
    ;; A complete one-line declaration stays complete.
    (check-equal?
      (Hover-contents (doc-hover d (Pos 7 8)))
      (string-append
        "**Source**\n"
        "```racket\n(define answer (string-length \"input\"))\n```\n\n"
        "**Mouse-over status**\n\n"
        "no bound occurrences"))
    ;; A same-line header keeps its comment and marks the omitted body.
    (check-equal?
      (Hover-contents (doc-hover d (Pos 8 9)))
      (string-append
        "**Source**\n"
        "```racket\n(define (parse raw) ; accepts overrides\n  ...\n```\n\n"
        "**Mouse-over status**\n\n"
        "no bound occurrences")))

  (test-case
    "Document hover renders leading comments for declarations and local bindings"
    (define d
      (make-doc
        "file:///tmp/hover-detail-comments.rkt"
        "#lang racket\n;; Produces the next Fibonacci value.\n;; Kept separate from callers for reuse.\n(define (fib value)\n  value)\n(let (\n      ;; Counts visits in this branch.\n      [count 1])\n  count)\n"))
    (check-true (doc-expand! d))
    (check-equal?
      (Hover-contents (doc-hover d (Pos 3 9)))
      (string-append
        "**Source**\n"
        "```racket\n;; Produces the next Fibonacci value.\n;; Kept separate from callers for reuse.\n(define (fib value)\n  ...\n```\n\n"
        "**Mouse-over status**\n\n"
        "no bound occurrences"))
    (check-equal?
      (Hover-contents (doc-hover d (Pos 7 7)))
      (string-append
        "**Source**\n"
        "```racket\n;; Counts visits in this branch.\n[count 1]\n```\n\n"
        "**Mouse-over status**\n\n"
        "1 bound occurrence")))

  (test-case
    "Document hover leaves separated and trailing comments unattached"
    (define d
      (make-doc
        "file:///tmp/hover-detail-comment-boundaries.rkt"
        "#lang racket\n;; Separated from the declaration.\n\n(define (separated value) value)\n(displayln 1) ; Explains the display call.\n(define (trailing value) value)\n"))
    (check-true (doc-expand! d))
    (define separated-hover
      (Hover-contents (doc-hover d (Pos 3 9))))
    (define trailing-hover
      (Hover-contents (doc-hover d (Pos 5 9))))
    (check-false (string-contains? separated-hover "Separated from"))
    (check-false (string-contains? trailing-hover "Explains the display")))

  (test-case
    "Document hover bounds leading comment blocks independently from code"
    (define long-comment-line
      (string-append ";; " (make-string 205 #\x)))
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
    (define contents
      (Hover-contents (doc-hover d (Pos 12 9))))
    (check-true
      (string-contains?
        contents
        (string-append ";; " (make-string 197 #\x) "...")))
    (check-true (string-contains? contents ";; Extra comment line 8"))
    (check-false (string-contains? contents ";; Extra comment line 9"))
    (check-true (string-contains? contents "...\n(define (documented value) value)")))

  (test-case
    "Document hover renders a collapsed binding header"
    (define d
      (make-doc
        "file:///tmp/hover-detail-context.rkt"
        "#lang racket\n(define (format-lines original-lines formatted-lines start-ln end-ln)\n  (for/list ([original-line (in-list original-lines)]\n             [formatted-line (in-list formatted-lines)]\n             [ln (in-naturals)]\n             #:break (> ln end-ln)\n             #:when (and (<= start-ln ln end-ln)\n                         (not (string=? original-line formatted-line))))\n    (list ln formatted-line)))\n"))
    (check-true (doc-expand! d))
    (check-equal?
      (Hover-contents (doc-hover d (Pos 4 14)))
      (string-append
        "**Source**\n"
        "```racket\n[ln (in-naturals)]\n```\n\n"
        "**Mouse-over status**\n\n"
        "3 bound occurrences")))

  (test-case
    "Document hover shows the complete nearest struct form"
    (define d
      (make-doc
        "file:///tmp/hover-detail-struct.rkt"
        "#lang racket\n(struct RopeNode\n  (left\n   right\n   chars\n   newlines\n   height)\n  #:transparent)\n"))
    (check-true (doc-expand! d))
    (check-equal?
      (Hover-contents (doc-hover d (Pos 1 8)))
      (string-append
        "**Source**\n"
        "```racket\n(struct RopeNode\n  (left\n   right\n   chars\n   newlines\n   height)\n  #:transparent)\n```\n\n"
        "**Mouse-over status**\n\n"
        "no bound occurrences")))

  (test-case
    "Document hover keeps imported symbols out of same-file source detail"
    (define d
      (make-doc "file:///tmp/hover-detail-import.rkt"
                "#lang racket\n(list)\n"))
    (check-true (doc-expand! d))
    (define imported-hover (doc-hover d (Pos 1 1)))
    (check-not-false imported-hover)
    (define contents (Hover-contents imported-hover))
    (check-false (string-contains? contents "**Source**"))
    (check-true (string-contains? contents "[Online docs]"))
    (check-true (string-contains? contents "imported from"))
    (define imported-from-start
      (caar (regexp-match-positions #rx"imported from" contents)))
    (define online-docs-start
      (caar (regexp-match-positions #rx"\\[Online docs\\]" contents)))
    (check-true (< imported-from-start online-docs-start)))

  (test-case
    "Document hover keeps cross-file workspace bindings out of source detail"
    (define temp-dir (make-temporary-file "hover-detail~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (define helper-path (build-path temp-dir "helper.rkt"))
        (call-with-output-file helper-path
          #:exists 'truncate
          (lambda (out)
            (display "#lang racket\n(provide shared-value)\n(define shared-value 1)\n" out)))
        (define main-path (build-path temp-dir "main.rkt"))
        (define d
          (make-doc (string-append "file://" (path->string main-path))
                    "#lang racket\n(require \"helper.rkt\")\nshared-value\n"))
        (check-true (doc-expand! d))
        (define imported-hover (doc-hover d (Pos 2 1)))
        (check-not-false imported-hover)
        (check-true (string-contains? (Hover-contents imported-hover)
                                      "imported from")))
      (lambda ()
        (delete-directory/files temp-dir))))

  (test-case
    "Document hover renders best-effort source detail while its trace is stale"
    (define d
      (make-doc "file:///tmp/hover-detail-stale.rkt"
                "#lang racket\n(let ([count 1]) (+ count 1))\n"))
    (check-true (doc-expand! d))
    (doc-apply-edit! d
                     (Range (Pos 1 13) (Pos 1 14))
                     "2")
    (doc-update-version! d 1)
    ;; No hover annotation. Shifted ranges still build a source-only card from
    ;; the edited buffer. The binding link may be wrong or incomplete.
    (check-equal? (Hover-contents (doc-hover d (Pos 1 22)))
                  (string-append
                    "**Source**\n"
                    "```racket\n(let ([count 2]) (+ count 1))\n```"))
    (define declaration-hover (doc-hover d (Pos 1 7)))
    (check-not-false declaration-hover)
    (check-true (string-contains? (Hover-contents declaration-hover)
                                  "(let ([count 2])")))

  (test-case
    "Document hover replays retained source detail after an insertion"
    (define d
      (make-doc "file:///tmp/hover-detail-shifted.rkt"
                "#lang racket\n(let ([count 1]) (+ count 1))\n"))
    (check-true (doc-expand! d))
    ;; The old trace must shift the declaration key and the detail's nested
    ;; display ranges before the next expansion finishes.
    (doc-apply-edit! d
                     (Range (Pos 1 0) (Pos 1 0))
                     "  ")
    (doc-update-version! d 1)
    (check-equal?
      (Hover-contents (doc-hover d (Pos 1 24)))
      (string-append
        "**Source**\n"
        "```racket\n(let ([count 1]) (+ count 1))\n```"))
    (check-true
      (string-contains? (Hover-contents (doc-hover d (Pos 1 9)))
                        "(let ([count 1])")))

  (test-case
    "Document hover truncates source forms by characters and lines"
    (define long-line
      (string-append "\"" (make-string 600 #\x) "\""))
    (define short-lines
      (string-join (make-list 12 "  1") "\n"))
    (define text
      (string-append "#lang racket\n"
                     "(let ([value (list " long-line "\n"
                     short-lines
                     "\n)]) value)\n"))
    (define d (make-doc "file:///tmp/hover-detail-truncate.rkt" text))
    (check-true (doc-expand! d))
    (define result (Hover-contents (doc-hover d (Pos 1 7))))
    (check-true (string-prefix? result "**Source**\n```racket\n"))
    (define match
      (regexp-match #px"```racket\n((?:.|\n)*)\n```" result))
    (check-not-false match)
    (define code (cadr match))
    (check-true (<= (string-length code) 1004))
    (check-true (<= (add1 (length (regexp-match-positions* #px"\n" code))) 11))
    (check-true (string-suffix? code "\n  ...")))

  (test-case
    "Document hover renders Rhombus structural forms when Rhombus is available"
    (define rhombus-available?
      (with-handlers ([exn:fail? (lambda (_exn) #f)])
        (dynamic-require 'rhombus/main #f)
        #t))
    (when rhombus-available?
      (define d
        (make-doc "file:///tmp/hover-detail.rhm"
                  "#lang rhombus\ndef value = 1\nfun parse_value(raw): raw\n"))
      (check-true (doc-expand! d))
      ;; When two forms share a start offset, the first top-level def must not
      ;; inherit the Rhombus root aggregate range.
      (check-equal?
        (Hover-contents (doc-hover d (Pos 1 4)))
        (string-append
          "**Source**\n"
          "```rhombus\ndef value = 1\n```\n\n"
          "**Mouse-over status**\n\n"
          "no bound occurrences"))
      (check-equal?
        (Hover-contents (doc-hover d (Pos 2 4)))
        (string-append
          "**Source**\n"
          "```rhombus\nfun parse_value(raw): raw\n```\n\n"
          "**Mouse-over status**\n\n"
          "no bound occurrences"))
      (define comment-d
        (make-doc "file:///tmp/hover-detail-comments.rhm"
                  "#lang rhombus\n// Converts the value.\nfun documented(value): value\n"))
      (check-true (doc-expand! comment-d))
      (check-equal?
        (Hover-contents (doc-hover comment-d (Pos 2 4)))
        (string-append
          "**Source**\n"
          "```rhombus\n// Converts the value.\nfun documented(value): value\n```\n\n"
          "**Mouse-over status**\n\n"
          "no bound occurrences"))
      ;; A bound use must find detail through `occurrence-at`, not only
      ;; through definition targets.
      (define use-d
        (make-doc "file:///tmp/hover-detail-use.rhm"
                  "#lang rhombus\nfun parse_value(raw): raw\nparse_value(1)\n"))
      (check-true (doc-expand! use-d))
      (check-equal?
        (Hover-contents (doc-hover use-d (Pos 2 0)))
        (string-append
          "**Source**\n"
          "```rhombus\nfun parse_value(raw): raw\n```"))))

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
    "doc-symbols-hierarchical returns nested DocumentSymbols"
    (define text
#<<END
#lang racket
(define x 1)
(define (f y)
  (define inner 2)
  inner)
(module+ test
  (check-equal? (f 1) 2))
(struct point (x y))
END
      )
    (define d (make-doc "file:///test.rkt" text))
    (define syms (doc-symbols-hierarchical d))
    (check-equal? (map DocumentSymbol-name syms) '("x" "f" "test" "point"))
    (check-equal? (map DocumentSymbol-kind syms)
                  (list SymbolKind-Variable
                        SymbolKind-Function
                        SymbolKind-Module
                        SymbolKind-Struct))
    ;; `x`: the range covers the whole define form, the selection range only
    ;; covers the identifier.
    (define x-sym (first syms))
    (check-equal? (DocumentSymbol-range x-sym) (Range (Pos 1 0) (Pos 1 12)))
    (check-equal? (DocumentSymbol-selectionRange x-sym) (Range (Pos 1 8) (Pos 1 9)))
    (check-equal? (DocumentSymbol-children x-sym) '())
    ;; `f`: the range spans the whole body and the nested define shows up as
    ;; a child symbol.
    (define f-sym (second syms))
    (check-equal? (DocumentSymbol-range f-sym) (Range (Pos 2 0) (Pos 4 8)))
    (check-equal? (DocumentSymbol-selectionRange f-sym) (Range (Pos 2 9) (Pos 2 10)))
    (check-equal? (map DocumentSymbol-name (DocumentSymbol-children f-sym))
                  '("inner"))
    (define inner-sym (first (DocumentSymbol-children f-sym)))
    (check-equal? (DocumentSymbol-kind inner-sym) SymbolKind-Variable)
    (check-equal? (DocumentSymbol-range inner-sym) (Range (Pos 3 2) (Pos 3 18)))
    (check-equal? (DocumentSymbol-selectionRange inner-sym) (Range (Pos 3 10) (Pos 3 15)))
    ;; `test` submodule: non-definition forms inside it contribute no children.
    (define test-sym (third syms))
    (check-equal? (DocumentSymbol-children test-sym) '())
    ;; `point` struct
    (define point-sym (fourth syms))
    (check-equal? (DocumentSymbol-range point-sym) (Range (Pos 7 0) (Pos 7 20)))
    (check-equal? (DocumentSymbol-selectionRange point-sym) (Range (Pos 7 8) (Pos 7 13))))

  (test-case
    "doc-symbols-hierarchical skips definitions in non-definition forms heads"
    (define text
#<<END
#lang racket
(let ([x 1])
  x)
(displayln "hello")
END
      )
    (define d (make-doc "file:///test.rkt" text))
    (check-equal? (doc-symbols-hierarchical d) '()))

  (test-case
    "doc-symbols-hierarchical handles container forms and class members"
    (define text
#<<END
#lang racket
(provide foo
         bar)
(define foo%
  (class object%
    (init-field x)
    (field [y 0] z)
    (define/public (m) x)))
(define (foo lst)
  (match lst
    [(list a) a]))
END
      )
    (define d (make-doc "file:///test.rkt" text))
    (define syms (doc-symbols-hierarchical d))
    (check-equal? (map DocumentSymbol-name syms) '("provide" "foo%" "foo"))
    ;; `provide`: a container form named after its head so scrolling inside a
    ;; long provide keeps a sticky header; provided names are not symbols.
    (define provide-sym (first syms))
    (check-equal? (DocumentSymbol-kind provide-sym) SymbolKind-Namespace)
    (check-equal? (DocumentSymbol-range provide-sym) (Range (Pos 1 0) (Pos 2 13)))
    (check-equal? (DocumentSymbol-selectionRange provide-sym) (Range (Pos 1 1) (Pos 1 8)))
    (check-equal? (DocumentSymbol-children provide-sym) '())
    ;; `foo%` wraps a `class` container holding the member symbols.
    (define foo%-sym (second syms))
    (check-equal? (map DocumentSymbol-name (DocumentSymbol-children foo%-sym))
                  '("class"))
    (define class-sym (first (DocumentSymbol-children foo%-sym)))
    (check-equal? (DocumentSymbol-kind class-sym) SymbolKind-Class)
    (check-equal? (DocumentSymbol-selectionRange class-sym) (Range (Pos 4 3) (Pos 4 8)))
    (check-equal? (map DocumentSymbol-name (DocumentSymbol-children class-sym))
                  '("x" "y" "z" "m"))
    (check-equal? (map DocumentSymbol-kind (DocumentSymbol-children class-sym))
                  (list SymbolKind-Field
                        SymbolKind-Field
                        SymbolKind-Field
                        SymbolKind-Function))
    ;; `y` is declared via a binding group: its range covers `[y 0]`.
    (define y-sym (second (DocumentSymbol-children class-sym)))
    (check-equal? (DocumentSymbol-range y-sym) (Range (Pos 6 11) (Pos 6 16)))
    (check-equal? (DocumentSymbol-selectionRange y-sym) (Range (Pos 6 12) (Pos 6 13)))
    ;; `match` inside a function body is a container child of the function.
    (define foo-sym (third syms))
    (check-equal? (map DocumentSymbol-name (DocumentSymbol-children foo-sym))
                  '("match"))
    (define match-sym (first (DocumentSymbol-children foo-sym)))
    (check-equal? (DocumentSymbol-kind match-sym) SymbolKind-Object)
    (check-equal? (DocumentSymbol-range match-sym) (Range (Pos 9 2) (Pos 10 17)))
    (check-equal? (DocumentSymbol-children match-sym) '()))

  (test-case
    "doc-symbols-hierarchical handles require forms"
    (define text
#<<END
#lang racket
(require racket/match
         racket/list)
END
      )
    (define d (make-doc "file:///test.rkt" text))
    (define syms (doc-symbols-hierarchical d))
    (check-equal? (map DocumentSymbol-name syms) '("require"))
    (define require-sym (first syms))
    (check-equal? (DocumentSymbol-kind require-sym) SymbolKind-Namespace)
    (check-equal? (DocumentSymbol-range require-sym) (Range (Pos 1 0) (Pos 2 21)))
    (check-equal? (DocumentSymbol-selectionRange require-sym) (Range (Pos 1 1) (Pos 1 8)))
    (check-equal? (DocumentSymbol-children require-sym) '()))

  (test-case
    "doc-symbols-hierarchical tolerates incomplete code"
    ;; The document is mid-edit: the define form is not closed yet. The open
    ;; form is closed at end-of-document so the symbol is still reported.
    (define d (make-doc "file:///test.rkt" "#lang racket\n(define (broken"))
    (define syms (doc-symbols-hierarchical d))
    (check-equal? (map DocumentSymbol-name syms) '("broken"))
    (check-equal? (DocumentSymbol-kind (first syms)) SymbolKind-Function)
    (check-equal? (DocumentSymbol-range (first syms))
                  (Range (Pos 1 0) (Pos 1 15))))

  (test-case
    "doc-symbols-hierarchical language guard"
    ;; The tree builder tracks s-expression paren nesting, which means nothing
    ;; in non-sexp languages: Rhombus form extents are set by `:` blocks and
    ;; indentation, so paren-derived symbols and ranges would be bogus. Return
    ;; no symbols instead.
    (define rhombus-doc
      (make-doc "file:///test.rhm"
                "#lang rhombus\ndef y = (match x\n         | 1: 2\n         | ~else: 3)\n"))
    (check-equal? (doc-symbols-hierarchical rhombus-doc) '())
    (define scribble-doc
      (make-doc "file:///test.scrbl"
                "#lang scribble/manual\n@(define foo 1)\n"))
    (check-equal? (doc-symbols-hierarchical scribble-doc) '()))

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
