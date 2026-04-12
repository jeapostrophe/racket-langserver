#lang racket

(module+ test
  (require rackunit
           "../../doclib/doc.rkt"
           "../../doclib/lexer.rkt"
           "../../common/interfaces.rkt")

  (test-case
    "build-lexer-snapshot enumerates public lexer entries"
    (define snapshot (build-lexer-snapshot "#lang racket\n(define x 1)\n"))
    (define entries (for/list ([entry (in-lexer-snapshot snapshot)]) entry))
    (define (entry-summary entry)
      (list (LexerEntry-start entry)
            (LexerEntry-end entry)
            (LexerEntry-text entry)
            (LexerEntry-type entry)))
    ;; Cached lexer positions are normalized to the doc's 0-based offsets.
    (check-not-false (member (list 13 14 "(" 'parenthesis)
                             (map entry-summary entries)))
    (check-not-false (member (list 14 20 "define" 'symbol)
                             (map entry-summary entries)))
    (check-not-false (member (list 20 21 " " 'white-space)
                             (map entry-summary entries)))
    (check-not-false (member (list 21 22 "x" 'symbol)
                             (map entry-summary entries)))
    (check-not-false (member (list 23 24 "1" 'constant)
                             (map entry-summary entries)))
    (check-not-false (member (list 24 25 ")" 'parenthesis)
                             (map entry-summary entries))))

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
                  (list 18 19 "(" 'parenthesis))

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
