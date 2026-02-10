#lang racket

(module+ test
  (require rackunit
           "../../doc.rkt"
           "../../struct.rkt"
           "../../interfaces.rkt"
           racket/class
           racket/file
           json)

  (test-case
    "Document creation and basic accessors"
    (define d (new-doc "file:///test.rkt" "hello world" 1))
    (check-equal? (Doc-version d) 1)
    (check-equal? (Doc-uri d) "file:///test.rkt")
    (check-equal? (send (Doc-text d) get-text) "hello world"))

  (test-case
    "Document update"
    (define d (new-doc "file:///test.rkt" "hello world" 1))
    ;; Replace "world" with "racket"
    ;; "hello world"
    ;; 01234567890
    ;; world starts at 6, len 5.
    (doc-update! d 0 6 0 11 "racket")
    (check-equal? (send (Doc-text d) get-text) "hello racket")

    ;; Insert "!" at end
    ;; "hello racket" length is 12
    (doc-update! d 0 12 0 12 "!")
    (check-equal? (send (Doc-text d) get-text) "hello racket!"))

  (test-case
    "Document deletions and complex updates"
    (define d (new-doc "file:///test.rkt" "12345" 1))

    ;; Delete "234" (indices 1 to 4)
    ;; "12345"
    ;;  01234
    (doc-update! d 0 1 0 4 "")
    (check-equal? (send (Doc-text d) get-text) "15")

    ;; Prepend "0"
    (doc-update! d 0 0 0 0 "0")
    (check-equal? (send (Doc-text d) get-text) "015")

    ;; Replace everything
    (doc-update! d 0 0 0 3 "cleaned")
    (check-equal? (send (Doc-text d) get-text) "cleaned"))

  (test-case
    "Document position conversion"
    (define text "line1\nline2\nline3")
    (define d (new-doc "file:///test.rkt" text 1))

    ;; check doc-pos
    ;; line1\n is 6 chars (0-5)
    ;; line2 starts at 6
    (check-equal? (doc-pos d 0 0) 0)
    (check-equal? (doc-pos d 1 0) 6)
    (check-equal? (doc-pos d 2 0) 12)

    ;; check doc-line/ch
    (define-values (l c) (doc-line/ch d 6))
    (check-equal? l 1)
    (check-equal? c 0))

  (test-case
    "Document symbols"
    (define text "#lang racket\n(define x 1)\n")
    (define d (new-doc "file:///test.rkt" text 1))
    ;; This might require more complex setup if get-symbols depends on more things
    ;; But let's try basic check
    (define syms (doc-get-symbols d))
    (check-not-false syms))

  (test-case
    "Find containing paren"
    (define text "(list 1 2)")
    (define d (new-doc "file:///test.rkt" text 1))
    ;; (list 1 2)
    ;; 0123456789
    ;; inside `list` at 2
    (check-equal? (doc-find-containing-paren d 2) 0)
    ;; at 1 (just after open paren)
    (check-equal? (doc-find-containing-paren d 1) 0)

    (define text2 "((a) b)")
    (define d2 (new-doc "file:///test.rkt" text2 1))
    ;; ((a) b)
    ;; 0123456
    ;; inside (a) at 2 ('a')
    (check-equal? (doc-find-containing-paren d2 2) 1)
    ;; inside outer at 5 ('b')
    (check-equal? (doc-find-containing-paren d2 5) 0)

    ;; Edge cases
    (define text3 "( [ { ] )")
    ;; 012345678
    (define d3 (new-doc "file:///test.rkt" text3 1))

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
    (define d4 (new-doc "file:///test.rkt" " ) (" 1))
    ;; 0123
    (check-equal? (doc-find-containing-paren d4 1) #f))

  (test-case
    "Document meta updates"
    (define d (new-doc "file:///test.rkt" "v1" 1))
    (doc-update-version! d 2)
    (check-equal? (Doc-version d) 2)
    (doc-update-uri! d "file:///test2.rkt")
    (check-equal? (Doc-uri d) "file:///test2.rkt")
    (doc-reset! d "v2")
    (check-equal? (send (Doc-text d) get-text) "v2"))

  (test-case
    "Document line/pos calc"
    (define text "line1\nline2")
    ;; line1\n is 6 chars, line2 is 5 chars. Total 11.
    (define d (new-doc "file:///test.rkt" text 1))
    ;; doc-endpos
    (check-equal? (doc-endpos d) 11)
    ;; doc-line-start-pos
    (check-equal? (doc-line-start-pos d 1) 6)
    ;; doc-line-end-pos
    (check-equal? (doc-line-end-pos d 0) 5) ;; excludes newline
    (check-equal? (doc-line-end-pos d 1) 11))

  (test-case
    "Token guessing"
    (define text "foo bar-baz \"str\"")
    ;; 01234567890123456
    ;; "foo bar-baz " is 12 chars.
    ;; "str" starts at 12.
    (define d (new-doc "file:///test.rkt" text 1))

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
    (define d (new-doc "file:///test.rkt" text 1))

    ;; Just check it returns a list (maybe empty if expansion hasn't happened)
    (define tokens (doc-range-tokens d 0 20))
    (check-pred list? tokens)

    ;; If tokens are generated, check structure
    (when (not (empty? tokens))
      (define first-token (first tokens))
      ;; deltaLine, deltaStart, length, tokenType, tokenModifiers
      (check-equal? (length first-token) 5)
      (check-pred exact-nonnegative-integer? (first first-token))))

  (test-case
    "Formatting"
    ;; doc.rkt `format!` uses `indenter` from trace.
    (define text "(define x\n1)")
    (define d (new-doc "file:///test.rkt" text 1))
    (define opts (FormattingOptions 2 #t #t #f #f #f)) ;; tab-size 2
    (define edits (format! d 0 0 2 0 #:formatting-options opts))
    (check-pred list? edits)

    ;; Test with tab size 4
    (define opts4 (FormattingOptions 4 #t #t #f #f #f))
    (define edits4 (format! d 0 0 2 0 #:formatting-options opts4))
    (check-pred list? edits4))

  (test-case
    "Get definition"
    (define tmp-file (make-temporary-file "test~a.rkt"))
    (define text "#lang racket\n(define x 1)\nx")
    (with-output-to-file tmp-file #:exists 'replace (lambda () (display text)))

    (define def-range (get-definition-by-id tmp-file 'x))
    (check-true (or (*Range? def-range) (hash? def-range)))

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
    (define d (new-doc uri text 1))
    (doc-expand! d)
    (values d uri))

  (define (make-pos line char)
    (Pos #:line line #:char char))
  (define (make-range st-ln st-ch ed-ln ed-ch)
    (Range #:start (make-pos st-ln st-ch) #:end (make-pos ed-ln ed-ch)))

  (test-case
    "doc-get-decl on a binding usage"
    (define-values (d uri) (make-expanded-doc))
    ;; "x" at line 2, char 0 is a usage → resolves to local declaration
    (define-values (start end decl) (doc-get-decl d 2 0))
    (check-equal? start 26 "usage start pos")
    (check-equal? end 27 "usage end pos")
    (check-pred Decl? decl)
    (check-false (Decl-filename decl) "local binding has no filename")
    (check-equal? (Decl-left decl) 21 "declaration left pos")
    (check-equal? (Decl-right decl) 22 "declaration right pos"))

  (test-case
    "doc-get-decl on the definition site"
    (define-values (d uri) (make-expanded-doc))
    ;; "x" in (define x 1) at line 1, char 8
    (define-values (start end decl) (doc-get-decl d 1 8))
    (check-equal? start 21 "definition start pos")
    (check-equal? end 22 "definition end pos")
    (check-pred Decl? decl)
    (check-false (Decl-filename decl) "local binding has no filename")
    (check-equal? (Decl-left decl) 21)
    (check-equal? (Decl-right decl) 22))

  (test-case
    "doc-get-decl on imported 'define'"
    (define-values (d uri) (make-expanded-doc))
    ;; "define" at line 1, char 1 is an imported symbol
    (define-values (start end decl) (doc-get-decl d 1 1))
    (check-equal? start 14 "imported define start pos")
    (check-equal? end 20 "imported define end pos")
    (check-pred Decl? decl)
    (check-not-false (Decl-filename decl) "imported binding has a filename")
    ;; Imported symbols have left=0, right=0
    (check-equal? (Decl-left decl) 0)
    (check-equal? (Decl-right decl) 0))

  (test-case
    "doc-get-decl on literal returns #f"
    (define-values (d uri) (make-expanded-doc))
    ;; "1" at line 1, char 10 is a literal
    (define-values (start end decl) (doc-get-decl d 1 10))
    (check-false decl "literal should not have a declaration"))

  (test-case
    "doc-get-bindings returns usage ranges for local x"
    (define-values (d uri) (make-expanded-doc))
    (define-values (_s _e decl) (doc-get-decl d 2 0))
    (define bindings (doc-get-bindings d decl))
    ;; Should contain exactly the usage of "x" at line 2, char 0..1
    (check-equal? (length bindings) 1)
    (check-equal? (first bindings) (make-range 2 0 2 1)))

  (test-case
    "doc-completion returns x in items"
    (define-values (d uri) (make-expanded-doc))
    (define result (doc-completion d 2 1))
    (check-equal? (hash-ref result 'isIncomplete) #t)
    (define items (hash-ref result 'items))
    (check-pred list? items)
    ;; "x" should be among the completions
    (check-not-false
      (findf (λ (i) (equal? (hash-ref i 'label) "x")) items)
      "x should be in completions"))

  (test-case
    "doc-definition for local x usage"
    (define-values (d uri) (make-expanded-doc))
    ;; "x" at line 2 → definition at line 1 char 8..9
    (define result (doc-definition d uri 2 0))
    (check-equal? (hash-ref result 'uri) uri)
    (check-equal? (hash-ref result 'range) (make-range 1 8 1 9)))

  (test-case
    "doc-definition for imported define"
    (define-values (d uri) (make-expanded-doc))
    ;; "define" at line 1 → jumps to external file
    (define result (doc-definition d uri 1 1))
    (check-pred hash? result)
    ;; URI should point to the racket source file, not our test file
    (check-not-equal? (hash-ref result 'uri) uri
                      "imported define should point to external file"))

  (test-case
    "doc-definition returns null for literal"
    (define-values (d uri) (make-expanded-doc))
    (define result (doc-definition d uri 1 10))
    (check-equal? result (json-null)))

  (test-case
    "doc-references for local x"
    (define-values (d uri) (make-expanded-doc))
    ;; "x" usage at (2,0) is a local binding, so doc-references returns bindings
    (define result (doc-references d uri 2 0 #t))
    (check-pred list? result)
    (check-equal? (length result) 1)
    (define ref (first result))
    (check-equal? (hash-ref ref 'uri) uri)
    (check-equal? (hash-ref ref 'range) (make-range 2 0 2 1)))

  (test-case
    "doc-highlights for local x"
    (define-values (d uri) (make-expanded-doc))
    ;; "x" usage at (2,0): highlights should include both usage and declaration
    (define result (doc-highlights d 2 0))
    (check-equal? (length result) 2)
    ;; First highlight: the usage (binding) at line 2
    (check-equal? (hash-ref (first result) 'range) (make-range 2 0 2 1))
    ;; Second highlight: the declaration at line 1
    (check-equal? (hash-ref (second result) 'range) (make-range 1 8 1 9)))

  (test-case
    "doc-rename local x to y"
    (define-values (d uri) (make-expanded-doc))
    ;; Rename "x" at definition site (1,8) to "y"
    (define result (doc-rename d uri 1 8 "y"))
    (check-pred hash? result)
    (define changes (hash-ref result 'changes))
    (define edits (hash-ref changes (string->symbol uri)))
    (check-equal? (length edits) 2 "should produce 2 edits (defn + usage)")
    ;; First edit: declaration at line 1 char 8..9
    (check-equal? (hash-ref (first edits) 'newText) "y")
    (check-equal? (hash-ref (first edits) 'range) (make-range 1 8 1 9))
    ;; Second edit: usage at line 2 char 0..1
    (check-equal? (hash-ref (second edits) 'newText) "y")
    (check-equal? (hash-ref (second edits) 'range) (make-range 2 0 2 1)))

  (test-case
    "doc-rename on imported define returns null"
    (define-values (d uri) (make-expanded-doc))
    (define result (doc-rename d uri 1 1 "my-define"))
    (check-equal? result (json-null)))

  (test-case
    "doc-prepare-rename for local x"
    (define-values (d uri) (make-expanded-doc))
    ;; "x" at definition site (1,8) → renamable, returns its range
    (define result (doc-prepare-rename d 1 8))
    (check-equal? result (make-range 1 8 1 9)))

  (test-case
    "doc-prepare-rename on imported define returns null"
    (define-values (d uri) (make-expanded-doc))
    (define result (doc-prepare-rename d 1 1))
    (check-equal? result (json-null)))

  (test-case
    "doc-symbols returns lexed symbols with correct positions"
    (define-values (d uri) (make-expanded-doc))
    (define result (doc-symbols d uri))
    ;; Symbols come from lexer, should have: define, x (defn), 1, x (usage).
    ;; Note: lexer positions are 1-based but get-symbols uses them directly.
    ;; Exact results depend on the lexer, but we can check structure and known entries.
    (check-equal? (length result) 4)
    ;; Check that "define" symbol is present with kind=SymbolKind-Variable (13)
    (define define-sym (findf (λ (s) (equal? (hash-ref s 'name) "define")) result))
    (check-not-false define-sym)
    (check-equal? (hash-ref define-sym 'kind) 13)
    ;; Check that "1" is present with kind=SymbolKind-Constant (14)
    (define one-sym (findf (λ (s) (equal? (hash-ref s 'name) "1")) result))
    (check-not-false one-sym)
    (check-equal? (hash-ref one-sym 'kind) 14))

  )

