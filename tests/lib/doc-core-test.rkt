#lang racket/base

(module+ test
  (require "../silent-logger.rkt"
           rackunit
           "../../common/interfaces.rkt"
           "../../doclib/doc.rkt"
           "../../doclib/lexer.rkt")

  (test-case "Document creation and basic accessors"
    (define d (make-doc "file:///test.rkt" "hello world"))
    (check-equal? (Doc-version d) 0)
    (check-equal? (Doc-uri d) "file:///test.rkt")
    (check-false (Doc-contribution d))
    (check-equal? (doc-get-text d) "hello world"))

  (test-case "Document update"
    (define d (make-doc "file:///test.rkt" "hello world"))
    (doc-apply-edit! d (Range (Pos 0 6) (Pos 0 11)) "racket")
    (check-equal? (doc-get-text d) "hello racket")
    (doc-apply-edit! d (Range (Pos 0 12) (Pos 0 12)) "!")
    (check-equal? (doc-get-text d) "hello racket!"))

  (test-case "Document deletions and complex updates"
    (define d (make-doc "file:///test.rkt" "12345"))
    (doc-apply-edit! d (Range (Pos 0 1) (Pos 0 4)) "")
    (check-equal? (doc-get-text d) "15")
    (doc-apply-edit! d (Range (Pos 0 0) (Pos 0 0)) "0")
    (check-equal? (doc-get-text d) "015")
    (doc-apply-edit! d (Range (Pos 0 0) (Pos 0 3)) "cleaned")
    (check-equal? (doc-get-text d) "cleaned"))

  (test-case "Document position conversion"
    (define d (make-doc "file:///test.rkt" "line1\nline2\nline3"))
    (check-equal? (doc-pos->abs-pos d (Pos 0 0)) 0)
    (check-equal? (doc-pos->abs-pos d (Pos 1 0)) 6)
    (check-equal? (doc-pos->abs-pos d (Pos 2 0)) 12)
    (check-equal? (doc-abs-pos->pos d 6) (Pos 1 0)))

  (test-case "Find containing paren"
    (define d (make-doc "file:///test.rkt" "(list 1 2)"))
    (check-equal? (doc-find-containing-paren d 2) 0)
    (check-equal? (doc-find-containing-paren d 1) 0)
    (check-equal? (doc-find-containing-paren d 9) 0)

    (define d2 (make-doc "file:///test.rkt" "((a) b)"))
    (check-equal? (doc-find-containing-paren d2 2) 1)
    (check-equal? (doc-find-containing-paren d2 5) 0)

    (define d3 (make-doc "file:///test.rkt" "( [ { ] )"))
    (check-equal? (doc-find-containing-paren d3 3) 2)
    (check-equal? (doc-find-containing-paren d3 5) 4)

    (define d4 (make-doc "file:///test.rkt" " ) ("))
    (check-false (doc-find-containing-paren d4 1)))

  (test-case "Find containing paren ignores parens inside strings"
    (define d (make-doc "file:///test.rkt" "(foo \"(\")"))
    (check-equal? (doc-find-containing-paren d 6) 0))

  (test-case "Document meta updates"
    (define d (make-doc "file:///test.rkt" "v1"))
    (doc-update-version! d 2)
    (check-equal? (Doc-version d) 2)
    (doc-update-uri! d "file:///test2.rkt")
    (check-equal? (Doc-uri d) "file:///test2.rkt")
    (doc-reset! d "v2")
    (check-equal? (doc-get-text d) "v2"))

  (test-case "Document line/pos calc"
    (define d (make-doc "file:///test.rkt" "line1\nline2"))
    (check-equal? (doc-end-abs-pos d) 11)
    (check-equal? (doc-line-start-abs-pos d 1) 6)
    (check-equal? (doc-line-end-abs-pos d 0) 5)
    (check-equal? (doc-line-end-abs-pos d 1) 11))

  (test-case "doc-token-at returns the token at the given position"
    (define d (make-doc "file:///test.rkt" "foo bar-baz \"str\""))
    (define (token-summary token)
      (list (LexerEntry-start token)
            (LexerEntry-end token)
            (LexerEntry-text token)
            (LexerEntry-type token)))
    (check-equal? (token-summary (doc-token-at d 2)) '(0 3 "foo" symbol))
    (check-equal? (token-summary (doc-token-at d 10)) '(4 11 "bar-baz" symbol))
    (check-equal? (token-summary (doc-token-at d 3)) '(3 4 " " white-space))
    (check-equal? (token-summary (doc-token-at d 12)) '(12 17 "\"str\"" string))
    (check-equal? (token-summary (doc-token-at d 15)) '(12 17 "\"str\"" string))
    (check-equal? (token-summary (doc-token-at d 16)) '(12 17 "\"str\"" string))
    (check-equal? (token-summary (doc-token-at d 0)) '(0 3 "foo" symbol)))

  (test-case "doc-token-at works on a non-sexp document without depending on body forest"
    (define d (make-doc "file:///test.scrbl" "#lang scribble/manual\n@section{Hi}\n"))
    (check-equal? (LexerEntry-text (doc-token-at d 6)) "#lang scribble/manual")
    (check-equal? (LexerEntry-type (doc-token-at d 6)) 'lang-directive)
    (check-equal? (doc-token-prefix-at d 6) "#lang s"))

  (test-case "doc-body-forest builds a forest for unknown languages"
    (define d (make-doc "file:///test.unknown" "#lang not-a-real-language\n(define x 1)\n"))
    (check-not-false (doc-body-forest d)))

  (test-case "doc-find-containing-paren works for unknown languages"
    (define d (make-doc "file:///test.unknown" "#lang not-a-real-language\n(define x 1)\n"))
    (check-equal? (doc-find-containing-paren d 28) 26))

  (test-case "doc-find-containing-paren fallback keeps the first form without a language header"
    (define d (make-doc "file:///test.rkt" "(first x)\n(second y)\n"))
    (check-equal? (doc-find-containing-paren d 2) 0)
    (check-equal? (doc-find-containing-paren d 12) 10))

  (test-case "Apply TextEdits"
    (define text "#lang racket/base\n(define x\n1)")
    (define d (make-doc "file:///test.rkt" text))
    (define opts
      (FormattingOptions #:tab-size 2
                         #:insert-spaces #t
                         #:trim-trailing-whitespace #t
                         #:insert-final-newline #f
                         #:trim-final-newlines #f
                         #:extras (hasheq)))
    (define edits (doc-format-edits d (Range (Pos 0 0) (Pos 2 0)) #:formatting-options opts))
    (check-equal? edits (list (TextEdit (Range (Pos 2 0) (Pos 2 2)) "  1)")))
    (check-equal? (LexerEntry-type (doc-token-at d 19)) 'symbol)
    (doc-apply-edits! d edits)
    (check-equal? (doc-get-text d) "#lang racket/base\n(define x\n  1)")
    (define updated-token (doc-token-at d (doc-pos->abs-pos d (Pos 2 2))))
    (check-true (LexerEntry? updated-token))
    (check-equal? (LexerEntry-type updated-token) 'constant)
    (check-equal? (LexerEntry-text updated-token) "1"))

  (test-case "Apply TextEdits invalidates lexer snapshot across spanning tokens"
    (define d (make-doc "file:///test.rkt" "#lang racket\n#| comment |#\nfoo\n"))
    (check-equal? (LexerEntry-type (doc-token-at d (doc-pos->abs-pos d (Pos 2 0)))) 'symbol)
    (doc-apply-edits! d (list (TextEdit (Range (Pos 1 11) (Pos 1 13)) "")))
    (define comment-token (doc-token-at d (doc-pos->abs-pos d (Pos 2 0))))
    (check-true (LexerEntry? comment-token))
    (check-equal? (LexerEntry-type comment-token) 'error)
    (check-equal? (LexerEntry-text comment-token) "#| comment \nfoo\n")))
