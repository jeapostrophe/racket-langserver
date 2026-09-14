#lang racket/base

(require rackunit
         racket/class
         racket/list
         racket/runtime-path
         racket/sandbox
         racket/string
         "../../common/interfaces.rkt"
         "../../common/path-util.rkt"
         "../../doclib/doc.rkt"
         "../../doclib/formatter/drracket.rkt"
         "../../doclib/formatter/textoid.rkt"
         "../../doclib/lexer.rkt")

(define-runtime-path fixtures-directory "../fixtures")

(define (newlines . lines)
  (string-join lines (string #\newline)))

(define (options tab-size insert-spaces)
  (FormattingOptions #:tab-size tab-size
                     #:insert-spaces insert-spaces
                     #:trim-trailing-whitespace #f
                     #:insert-final-newline #f
                     #:trim-final-newlines #f
                     #:extras (hasheq)))

(define default-options (options 2 #t))

(define color-textoid<%>
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (dynamic-require 'syntax-color/color-textoid 'color-textoid<%>)))

(define standard-racket-indentation?
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (procedure? (dynamic-require 'syntax-color/racket-indentation
                                 'racket-amount-to-indent))))

(define (textoid-observations textoid)
  (define end (send textoid last-position))
  (for/list ([position (in-range (add1 end))])
    (define-values (token-start token-end)
      (send textoid get-token-range position))
    (list (send textoid classify-position position)
          token-start
          token-end
          (send textoid forward-match position end)
          (send textoid backward-match position 0)
          (send textoid backward-containing-sexp position 0)
          (send textoid position-paragraph position))))

(define (fixture-uri name)
  (path->uri (build-path fixtures-directory name)))

(define (make-performance-document line-count misindented?)
  (string-join
    (cons "#lang racket/base"
          (for/list ([line (in-range (sub1 line-count))])
            (format "~a(define value-~a ~a)"
                    (if misindented? "  " "")
                    line
                    line)))
    "\n"))

(module+ test
  (test-case
    "headless textoid implements the public interface"
    (define textoid (make-textoid (newlines "(define x" "1)")))
    (when color-textoid<%>
      (check-true (is-a? textoid color-textoid<%>)))
    (check-equal? (send textoid paragraph-count) 2)
    (check-equal? (send textoid position-paragraph 0) 0)
    (check-equal? (send textoid position-paragraph 10) 1)
    (check-equal? (send textoid get-text) (newlines "(define x" "1)"))
    (check-equal? (send textoid get-text 10) "1)")
    (define attributes (send textoid classify-position* 0))
    (check-true (immutable? attributes))
    (check-equal? (hash-ref attributes 'type) 'parenthesis)
    (check-equal? (send textoid paragraph-start-position 99 #t) 12)
    (check-equal? (send textoid paragraph-end-position 99 #t) 12)
    (check-equal? (send (make-textoid " x")
                        skip-whitespace 1 'backward #f)
                  0)
    (define paren-textoid (make-textoid "(foo)"))
    (check-equal? (send paren-textoid forward-match 1 5) 4)
    (check-false (send paren-textoid backward-match 1 0))
    (check-equal? (send paren-textoid backward-match 2 0) 1)
    (check-equal? (send paren-textoid backward-containing-sexp 0 0) 0))

  (test-case
    "indexed navigation preserves cursor and cutoff boundaries"
    (define textoid (make-textoid "(a (b) c)"))
    (check-equal? (send textoid forward-match 0 9) 9)
    (check-false (send textoid forward-match 0 8))
    (check-equal? (send textoid forward-match 1 9) 2)
    (check-equal? (send textoid forward-match 2 9) 6)
    (check-equal? (send textoid backward-match 5 0) 4)
    (check-equal? (send textoid backward-match 6 0) 3)
    (check-false (send textoid backward-match 6 4))
    (check-false (send (make-textoid "foobar") backward-match 2 2))
    (check-equal? (send textoid backward-containing-sexp 7 0) 1)
    (check-false (send textoid backward-containing-sexp 7 1))
    (define mismatched (make-textoid "([)]"))
    (check-false (send mismatched forward-match 0 4))
    (check-false (send mismatched forward-match 1 4))
    (define trailing-newline (make-textoid "one\n"))
    (check-equal? (send trailing-newline paragraph-count) 2)
    (check-equal? (send trailing-newline position-paragraph 4) 1)
    (check-equal? (send trailing-newline paragraph-end-position 1) 4)
    (check-equal? (send (make-textoid "") paragraph-count) 1))

  (test-case
    "snapshot-backed textoid preserves standard Racket behavior"
    (for ([text (in-list
                  (list
                    (newlines "#lang racket/base"
                              "(define (f x)"
                              "  '#hash((a . 1)) ; comment"
                              "  #;[ignored] `(,x ,@'(2)))")
                    "([mismatched})"
                    "#| block\ncomment |# (list \"text\")\n"))])
      (define independently-lexed (make-textoid text))
      (define snapshot-backed
        (make-textoid-from-lexer-snapshot (build-lexer-snapshot text)))
      (check-equal? (textoid-observations snapshot-backed)
                    (textoid-observations independently-lexed))))

  (test-case
    "malformed snapshot spans fall back to independent lexing"
    (define text "x")
    (define malformed
      (LexerSnapshot text (vector (LexerTokenSpan 1 99 'symbol))))
    (check-equal?
      (textoid-observations (make-textoid-from-lexer-snapshot malformed))
      (textoid-observations (make-textoid text)))
    (define gapped
      (LexerSnapshot "ab" (vector (LexerTokenSpan 0 1 'symbol))))
    (check-equal?
      (textoid-observations (make-textoid-from-lexer-snapshot gapped))
      (textoid-observations (make-textoid "ab"))))

  (test-case
    "leading-whitespace updates agree with a fresh full lex"
    (for ([example (in-list
                     (list (list (newlines "  (define" "x)") 0 2 "")
                           (list (newlines "(define" "x)") 1 0 "  ")
                           (list (newlines "(define" "   x)") 1 3 " ")))])
      (define text (first example))
      (define line (second example))
      (define delete-amount (third example))
      (define insert-text (fourth example))
      (define textoid (make-textoid text))
      (define updated
        (textoid-replace-leading-whitespace textoid line delete-amount insert-text))
      (check-not-false updated)
      (define line-start (send (make-textoid text) paragraph-start-position line))
      (define expected-text
        (string-append (substring text 0 line-start)
                       insert-text
                       (substring text (+ line-start delete-amount))))
      (check-equal? (textoid-content updated) expected-text)
      (check-equal? (textoid-observations updated)
                    (textoid-observations (make-textoid expected-text))))
    (check-false
      (textoid-replace-leading-whitespace (make-textoid " x") 0 1 "\n")))

  (test-case
    "line-prefix updates preserve delimiter settings across rebuilds"
    (define parens '((|(| |]|)))
    (for ([textoid (in-list
                     (list (make-textoid "(x]" #:paren-matches parens)
                           (make-textoid-from-lexer-snapshot
                             (build-lexer-snapshot "(x]") #:paren-matches parens)))])
      ;; No preceding whitespace token exists, so the first insertion rebuilds.
      (define rebuilt (textoid-replace-line-prefix! textoid 0 0 " "))
      (check-not-false rebuilt)
      (check-false (eq? textoid rebuilt))
      (check-equal? (textoid-content textoid) "(x]")
      (check-equal? (send rebuilt forward-match 1 5) 4)
      (define updated (textoid-replace-line-prefix! rebuilt 0 1 "  "))
      (check-eq? updated rebuilt)
      (check-equal? (textoid-observations updated)
                    (textoid-observations (make-textoid "  (x]" #:paren-matches parens)))))

  (test-case
    "line-prefix rebuilds retain the reader directory and preserve state on failure"
    (define header "#lang reader \"formatter-hook-reader.rkt\"")
    (define text (newlines header "value"))
    (for ([textoid (in-list
                     (list (make-textoid text #:source-directory fixtures-directory)
                           (make-textoid-from-lexer-snapshot
                             (build-lexer-snapshot text (fixture-uri "custom-hook-source.rkt"))
                             #:source-directory fixtures-directory)))])
      (define rebuilt (textoid-replace-line-prefix! textoid 1 0 ";"))
      (check-not-false rebuilt)
      (check-equal? (textoid-content textoid) text)
      (check-equal?
        (textoid-observations rebuilt)
        (textoid-observations
          (make-textoid (newlines header ";value") #:source-directory fixtures-directory)))
      (define before (textoid-observations rebuilt))
      (check-false
        (textoid-replace-line-prefix!
          rebuilt 0 (string-length header)
          "#lang reader \"formatter-failing-lexer-reader.rkt\""))
      (check-equal? (textoid-content rebuilt) (newlines header ";value"))
      (check-equal? (textoid-observations rebuilt) before)))

  (test-case
    "many whitespace updates agree with one fresh lex"
    (define line-count 50)
    (define original
      (string-join
        (for/list ([line (in-range line-count)])
          (format "  (define value-~a ~a)" line line))
        "\n"))
    (define incrementally-updated (make-textoid original))
    (for ([line (in-range line-count)])
      (check-not-false
        (textoid-replace-leading-whitespace incrementally-updated line 2 "")))
    (define expected (regexp-replace* #px"(?m:^  )" original ""))
    (check-equal? (textoid-content incrementally-updated) expected)
    (check-equal? (textoid-observations incrementally-updated)
                  (textoid-observations (make-textoid expected))))

  (test-case
    "standard Racket indentation is headless and ignores formatting options"
    (define text (newlines "#lang racket/base" "(define x" "1)"))
    (define expected
      (list (TextEdit (Range (Pos 2 0) (Pos 2 0)) "  ")))
    (cond
      [standard-racket-indentation?
       (check-equal?
         (drracket-format-edits text
                                2
                                2
                                #:formatting-options default-options
                                #:racket-fallback? #t)
         expected)
       (check-equal?
         (drracket-format-edits text
                                2
                                2
                                #:formatting-options (options 8 #f)
                                #:racket-fallback? #t)
         expected)
       (check-equal?
         (drracket-format-edits text
                                3
                                3
                                #:formatting-options default-options
                                #:racket-fallback? #t)
         '())]
      [else
       (check-exn
         #rx"syntax-color-lib 1.4 or newer"
         (lambda ()
           (drracket-format-edits text
                                  2
                                  2
                                  #:formatting-options default-options
                                  #:racket-fallback? #t)))]))

  (test-case
    "standard Racket formatting reuses only a matching lexer snapshot"
    (when standard-racket-indentation?
      (define text (newlines "#lang racket/base" "(define x" "1)"))
      (define expected
        (drracket-format-edits text
                               0
                               2
                               #:formatting-options default-options
                               #:racket-fallback? #t))
      (check-equal?
        (drracket-format-edits text
                               0
                               2
                               #:formatting-options default-options
                               #:racket-fallback? #t
                               #:lexer-snapshot (build-lexer-snapshot text))
        expected)
      (check-equal?
        (drracket-format-edits text
                               0
                               2
                               #:formatting-options default-options
                               #:racket-fallback? #t
                               #:lexer-snapshot (build-lexer-snapshot "different"))
        expected)))

  (test-case
    "fallback indentation observes earlier line corrections"
    ;; DrRacket computes indentation sequentially, but the returned LSP edits
    ;; remain line-relative to the original document and can be applied together.
    (when standard-racket-indentation?
      (define text (newlines "  (define" "x)"))
      (define edits
        (drracket-format-edits text
                               0
                               1
                               #:formatting-options default-options
                               #:racket-fallback? #t))
      (check-equal? (map TextEdit-newText edits) (list "" "  "))
      (check-equal? (map TextEdit-range edits)
                    (list (Range (Pos 0 0) (Pos 0 2))
                          (Range (Pos 1 0) (Pos 1 0))))))

  (test-case
    "practical whole-document indentation stays within its latency budget"
    (when standard-racket-indentation?
      (define line-count 1000)
      (with-limits
        4
        #f
        (for ([misindented? (in-list '(#f #t))])
          (define text (make-performance-document line-count misindented?))
          (define edits
            (drracket-format-edits text
                                   0
                                   (sub1 line-count)
                                   #:formatting-options default-options
                                   #:racket-fallback? #t))
          (check-equal? (length edits)
                        (if misindented? (sub1 line-count) 0))))))

  (test-case
    "Scribble uses its language indentation hook without loading GUI"
    (define text
      (newlines "#lang scribble/base"
                "@itemlist["
                "@item{one}"
                "]"))
    (define gui-before? (module-declared? 'racket/gui/base))
    (define framework-before? (module-declared? 'framework))
    (define scribble-indent-before?
      (module-declared? 'scribble/private/indentation))
    (define edits
      (drracket-format-edits text
                             0
                             3
                             #:formatting-options default-options))
    (check-equal? edits
                  (list (TextEdit (Range (Pos 2 0) (Pos 2 0)) " ")
                        (TextEdit (Range (Pos 3 0) (Pos 3 0)) " ")))
    (check-equal? (module-declared? 'racket/gui/base) gui-before?)
    (check-equal? (module-declared? 'framework) framework-before?)
    (check-equal? (module-declared? 'scribble/private/indentation)
                  scribble-indent-before?))

  (test-case
    "at-exp uses the GUI-free Scribble indenter"
    (define text
      (newlines "#lang at-exp racket/base"
                "@itemlist["
                "@item{one}"
                "]"))
    (define gui-before? (module-declared? 'racket/gui/base))
    (define framework-before? (module-declared? 'framework))
    (define scribble-indent-before?
      (module-declared? 'scribble/private/indentation))
    (define edits
      (drracket-format-edits text
                             0
                             3
                             #:formatting-options default-options))
    (check-equal? edits
                  (list (TextEdit (Range (Pos 2 0) (Pos 2 0)) " ")
                        (TextEdit (Range (Pos 3 0) (Pos 3 0)) " ")))
    (check-equal? (module-declared? 'racket/gui/base) gui-before?)
    (check-equal? (module-declared? 'framework) framework-before?)
    (check-equal? (module-declared? 'scribble/private/indentation)
                  scribble-indent-before?))

  (test-case
    "Scribble embedded Racket uses standard indentation when the hook declines"
    (when standard-racket-indentation?
      (define text
        (newlines "#lang scribble/base"
                  "@(define (foo . a)"
                  "(bar b))"))
      (check-equal?
        (drracket-format-edits text
                               0
                               2
                               #:formatting-options default-options)
        (list (TextEdit (Range (Pos 2 0) (Pos 2 0)) "   ")))))

  (test-case
    "an unrecognized reader language can provide range indentation"
    (define text
      (newlines "#lang reader \"formatter-hook-reader.rkt\""
                "value"))
    (define doc
      (make-doc (fixture-uri "custom-hook-source.rkt") text))
    (define expected
      (list (TextEdit (Range (Pos 1 0) (Pos 1 0)) ">>>")))
    (check-equal?
      (doc-format-edits doc
                        (Range (Pos 1 0) (Pos 1 5))
                        #:backend 'drracket
                        #:formatting-options default-options)
      expected)
    (check-equal?
      (doc-format-edits doc
                        (Range (Pos 1 0) (Pos 1 5))
                        #:formatting-options default-options)
      expected))

  (test-case
    "language indentation replaces all leading whitespace"
    (define text
      (newlines "#lang reader \"formatter-hook-reader.rkt\""
                "\u00a0value"))
    (check-equal?
      (drracket-format-edits text
                             1
                             1
                             #:formatting-options default-options
                             #:lexer-snapshot (build-lexer-snapshot text)
                             #:src-dir fixtures-directory
                             #:interactive? #t)
      (list (TextEdit (Range (Pos 1 0) (Pos 1 1)) "   "))))

  (test-case
    "a failing language hook is contained"
    (define text
      (newlines "#lang reader \"formatter-hook-reader.rkt\""
                "fail-hook"))
    (define doc
      (make-doc (fixture-uri "failing-hook-source.rkt") text))
    (check-equal?
      (doc-format-edits doc
                        (Range (Pos 1 0) (Pos 1 9))
                        #:backend 'drracket
                        #:formatting-options default-options)
      '()))

  (test-case
    "a failing language lexer is contained before hooks run"
    (define text
      (newlines "#lang reader \"formatter-failing-lexer-reader.rkt\""
                "value"))
    (define doc
      (make-doc (fixture-uri "failing-lexer-source.rkt") text))
    (check-equal?
      (doc-format-edits doc
                        (Range (Pos 1 0) (Pos 1 5))
                        #:backend 'drracket
                        #:formatting-options default-options)
      '()))

  (test-case
    "an unsupported non-s-expression language returns no edits"
    (define text
      (newlines "#lang reader \"formatter-no-hook-reader.rkt\""
                "value"))
    (define doc
      (make-doc (fixture-uri "no-hook-source.rkt") text))
    (check-equal?
      (doc-format-edits doc
                        (Range (Pos 1 0) (Pos 1 5))
                        #:backend 'drracket
                        #:formatting-options default-options)
      '())))
