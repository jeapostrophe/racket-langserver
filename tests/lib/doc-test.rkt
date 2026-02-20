#lang racket

(module+ test
  (require rackunit
           "../../doc.rkt"
           "../../internal-types.rkt"
           "../../interfaces.rkt"
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
    (doc-update! d
                 (Range (Pos 0 6) (Pos 0 11))
                 "racket")
    (check-equal? (doc-get-text d) "hello racket")

    ;; Insert "!" at end
    ;; "hello racket" length is 12
    (doc-update! d
                 (Range (Pos 0 12) (Pos 0 12))
                 "!")
    (check-equal? (doc-get-text d) "hello racket!"))

  (test-case
    "Document deletions and complex updates"
    (define d (make-doc "file:///test.rkt" "12345"))

    ;; Delete "234" (indices 1 to 4)
    ;; "12345"
    ;;  01234
    (doc-update! d
                 (Range (Pos 0 1) (Pos 0 4))
                 "")
    (check-equal? (doc-get-text d) "15")

    ;; Prepend "0"
    (doc-update! d
                 (Range (Pos 0 0) (Pos 0 0))
                 "0")
    (check-equal? (doc-get-text d) "015")

    ;; Replace everything
    (doc-update! d
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
    "Document symbols"
    (define text "#lang racket\n(define x 1)\n")
    (define d (make-doc "file:///test.rkt" text))
    (define syms (doc-get-symbols d))
    ;; lexer positions are 1-based in this symbol map:
    ;; define: 15..21, x: 22..23, 1: 24..25
    (check-equal? (interval-map-ref syms 15 #f) (list "define" 'symbol))
    (check-equal? (interval-map-ref syms 22 #f) (list "x" 'symbol))
    (check-equal? (interval-map-ref syms 24 #f) (list "1" 'constant))
    (check-false (interval-map-ref syms 21 #f) "space should not be a symbol"))

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

    ;; Inside { : pos 5 '}'.
    ;; The logic treats { as normal char, so it skips it.
    ;; It sees ] at 6 (wait text index: 0:( 1:  2:[ 3:  4:{ 5:  6:] 7:  8:) )
    ;; Let's re-index carefully:
    ;; ( [ { ] )
    ;; 012345678
    ;; pos 5 is ' '. Before it is '{' at 4.
    ;; It loops back. ] at 6 is AFTER 5.
    ;; Loop goes 5->4->3->2. 2 is '['.
    ;; So inside { (at 5) it finds [.
    (check-equal? (doc-find-containing-paren d3 5) 2)

    ;; Unmatched close
    (define d4 (make-doc "file:///test.rkt" " ) ("))
    ;; 0123
    (check-equal? (doc-find-containing-paren d4 1) #f))

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
    "Token guessing"
    (define text "foo bar-baz \"str\"")
    ;; 01234567890123456
    ;; "foo bar-baz " is 12 chars.
    ;; "str" starts at 12.
    (define d (make-doc "file:///test.rkt" text))

    ;; "foo" at 2 ('o') -> "foo"
    (check-equal? (doc-guess-token d 2) "foo")
    ;; "bar-baz" at 10 ('z') -> "bar-baz"
    (check-equal? (doc-guess-token d 10) "bar-baz")
    ;; Space at 3 -> "" (as per logic)
    (check-equal? (doc-guess-token d 3) "")
    ;; Quote at 12 -> ""
    (check-equal? (doc-guess-token d 12) "")
    ;; Inside string at 15 ('r') -> "str"
    (check-equal? (doc-guess-token d 15) "str")
    ;; Start of file at 0 ('f') -> "f"
    (check-equal? (doc-guess-token d 0) "f"))

  (test-case
    "Range tokens (Semantic Tokens)"
    (define text "#lang racket\n(define x 1)")
    (define d (make-doc "file:///test.rkt" text))

    (define before-expand (doc-range-tokens d (Range (Pos 0 0) (Pos 1 11))))
    (check-true (empty? before-expand) "tokens should be empty before doc-expand!")

    (check-true (doc-expand! d))
    (define after-expand (doc-range-tokens d (Range (Pos 0 0) (Pos 1 11))))
    (check-false (empty? after-expand) "tokens should exist after doc-expand!")

    ;; Encoded semantic tokens are a flat list of integers in groups of 5.
    (check-equal? (modulo (length after-expand) 5) 0)
    (check-true (andmap exact-nonnegative-integer? after-expand)))

  (test-case
    "Formatting"
    ;; doc.rkt `doc-format-edits` uses `indenter` from trace.
    (define text "(define x\n1)")
    (define d (make-doc "file:///test.rkt" text))
    (define opts
      (FormattingOptions #:tab-size 2
                         #:insert-spaces #t
                         #:trim-trailing-whitespace #t
                         #:insert-final-newline #f
                         #:trim-final-newlines #f
                         #:key #f)) ;; tab-size 2
    (define edits (doc-format-edits d (Range (Pos 0 0) (Pos 2 0)) #:formatting-options opts))
    (check-equal? (length edits) 3)
    (check-true (andmap TextEdit? edits))
    (check-equal? (map TextEdit-newText edits) (list "" "" "  "))

    ;; Test with tab size 4
    (define opts4
      (FormattingOptions #:tab-size 4
                         #:insert-spaces #t
                         #:trim-trailing-whitespace #t
                         #:insert-final-newline #f
                         #:trim-final-newlines #f
                         #:key #f))
    (define edits4 (doc-format-edits d (Range (Pos 0 0) (Pos 2 0)) #:formatting-options opts4))
    (check-equal? (length edits4) 3)
    (check-true (andmap TextEdit? edits4))
    (check-equal? (map TextEdit-newText edits4) (list "" "" "  ")))

  (test-case
    "Apply TextEdits"
    (define text "(define x\n1)")
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
      (list (TextEdit (Range (Pos 0 9) (Pos 0 9)) "")
            (TextEdit (Range (Pos 1 2) (Pos 1 2)) "")
            (TextEdit (Range (Pos 1 0) (Pos 1 0)) "  ")))
    (doc-apply-edits! d edits)
    (check-equal? (doc-get-text d) "(define x\n  1)"))

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

  (test-case
    "doc-get-decl on a binding usage"
    (define-values (d _uri) (make-expanded-doc))
    ;; "x" at line 2, char 0 is a usage → resolves to local declaration
    (define-values (start end decl) (doc-get-decl d (Pos 2 0)))
    (check-equal? start 26 "usage start pos")
    (check-equal? end 27 "usage end pos")
    (check-false (Decl-filename decl) "local binding has no filename")
    (check-equal? (Decl-left decl) 21 "declaration left pos")
    (check-equal? (Decl-right decl) 22 "declaration right pos"))

  (test-case
    "doc-get-decl on the definition site"
    (define-values (d _uri) (make-expanded-doc))
    ;; "x" in (define x 1) at line 1, char 8
    (define-values (start end decl) (doc-get-decl d (Pos 1 8)))
    (check-equal? start 21 "definition start pos")
    (check-equal? end 22 "definition end pos")
    (check-false (Decl-filename decl) "local binding has no filename")
    (check-equal? (Decl-left decl) 21)
    (check-equal? (Decl-right decl) 22))

  (test-case
    "doc-get-decl on imported 'define'"
    (define-values (d _uri) (make-expanded-doc))
    ;; "define" at line 1, char 1 is an imported symbol
    (define-values (start end decl) (doc-get-decl d (Pos 1 1)))
    (check-equal? start 14 "imported define start pos")
    (check-equal? end 20 "imported define end pos")
    (check-not-false (Decl-filename decl) "imported binding has a filename")
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
    "doc-symbols returns lexed symbols with correct positions"
    (define-values (d uri) (make-expanded-doc))
    (define result (doc-symbols d uri))
    ;; Symbols come from lexer, should have: define, x (defn), 1, x (usage).
    ;; Note: lexer positions are 1-based but get-symbols uses them directly.
    ;; Exact results depend on the lexer, but we can check structure and known entries.
    (check-equal? (length result) 4)
    ;; Check that "define" symbol is present with kind=SymbolKind-Variable (13)
    (define define-sym (findf (λ (s) (equal? (SymbolInformation-name s) "define")) result))
    (check-not-false define-sym)
    (check-equal? (SymbolInformation-kind define-sym) 13)
    ;; Check that "1" is present with kind=SymbolKind-Constant (14)
    (define one-sym (findf (λ (s) (equal? (SymbolInformation-name s) "1")) result))
    (check-not-false one-sym)
    (check-equal? (SymbolInformation-kind one-sym) 14))

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

  )

