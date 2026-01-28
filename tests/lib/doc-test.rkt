#lang racket

(require rackunit
         "../../doc.rkt"
         "../../struct.rkt"
         racket/class
         racket/file)

(provide (all-defined-out))

(define (test-doc)
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
  )

(module+ test
  (test-doc))

