#lang racket/base

(module+ test
  (require "doc-test-support.rkt"
           rackunit
           racket/class
           racket/list
           racket/match
           racket/string
           "../../common/interfaces.rkt"
           "../../common/source.rkt"
           "../../doclib/doc.rkt"
           "../../doclib/doc-trace.rkt"
           "../../doclib/editor.rkt"
           "../../doclib/lexer.rkt")

  (for ([spec (in-list (list (list "exact range" (Pos 2 8) (Pos 2 9))
                             (list "overlapping range" (Pos 2 7) (Pos 2 9))))])
    (match-define (list label start end) spec)
    (test-case (format "Document code action for ~a" label)
      (define d (make-doc "file:///tmp/code-action-test.rkt" "#lang racket/base\n\n(define x 0)\n"))
      (doc-expand! d)
      (define actions (doc-code-action d (Range start end)))
      (check-false (empty? actions) "actions should not be empty")
      (check-equal? (CodeAction-title (first actions)) "Add prefix `_` to ignore")))

  (for ([spec (in-list
                (list
                  (list "basic call" "#lang racket/base\n\n(list )\n" (Pos 2 6) "list")
                  (list "single-character function" "#lang racket/base\n\n(+ )\n" (Pos 2 3) "+")
                  (list "spaces after open paren" "#lang racket/base\n\n(  list )\n" (Pos 2 8) "list")
                  (list "newline after open paren" "#lang racket/base\n\n(\n  list )\n" (Pos 3 7) "list")
                  (list "comment after open paren" "#lang racket/base\n\n( ; comment\n  list )\n" (Pos 3 7) "list")
                  (list "after nested closing paren" "#lang racket/base\n\n(list (+ 1 2))\n" (Pos 2 13) "list")))])
    (match-define (list label text pos expected-callee) spec)
    (test-case (format "Document signature help: ~a" label)
      (define d (make-doc "file:///tmp/signature-help-test.rkt" text))
      (doc-expand! d)
      (define help (doc-signature-help d pos))
      (check-not-false help)
      (define sigs (SignatureHelp-signatures help))
      (check-false (empty? sigs))
      (check-true (string-contains? (SignatureInformation-label (first sigs)) expected-callee))))

  (for ([spec (in-list
                (list
                  (list "outside closed form" "#lang racket/base\n\n(list)\n" (Pos 2 6))
                  (list "non-symbol callee expression" "#lang racket/base\n\n((lambda (x) x) )\n" (Pos 2 16))
                  (list "at buffer start" "#lang racket/base\n\n(list )\n" (Pos 0 0))))])
    (match-define (list label text pos) spec)
    (test-case (format "Document signature help returns #f: ~a" label)
      (define d (make-doc "file:///tmp/signature-help-none-test.rkt" text))
      (doc-expand! d)
      (check-false (doc-signature-help d pos))))

  (define (make-doc-with-online-completions text prefix->completions)
    (define uri "file:///tmp/completion-prefix-test.rkt")
    (define d (make-doc uri text))
    (define test-trace%
      (class build-trace%
        (super-new [source (path->source (string->path "/tmp/completion-prefix-test.rkt"))]
                   [doc-text (new lsp-editor%)]
                   [lexer-state (build-lexer-state text uri)])
        (define/override (get-completions) '())
        (define/override (get-online-completions str-before-cursor)
          (hash-ref prefix->completions str-before-cursor '()))))
    (define trace (new test-trace%))
    (doc-update-trace! d trace (send trace get-contribution) (Doc-version d))
    d)

  (test-case "doc-completion returns x in items"
    (define d (make-expanded-doc))
    (define result (doc-completion d (Pos 2 1)))
    (check-equal? (CompletionList-isIncomplete result) #t)
    (check-not-false
      (findf (lambda (i) (equal? (CompletionItem-label i) "x"))
             (CompletionList-items result))
      "x should be in completions"))

  (test-case "doc-completion at buffer start returns a list"
    (define d (make-expanded-doc))
    (check-pred CompletionList? (doc-completion d (Pos 0 0))))

  (test-case "doc-completion forwards exact online completion prefixes"
    (for ([spec (in-list
                  (list (list "foo" (hash "" '(alpha beta)) (Pos 0 0) '("alpha" "beta"))
                        (list "#lang racket/base\n(require foo/bar)\n"
                              (hash "foo/bar" '(symbol-prefix))
                              (Pos 1 16)
                              '("symbol-prefix"))
                        (list "#lang racket/base\n(require \"foo/bar\")\n"
                              (hash "foo/bar" '(string-prefix))
                              (Pos 1 17)
                              '("string-prefix"))
                        (list "#lang racket/base\n(require 'foo/bar)\n"
                              (hash "" '(empty-prefix) "'" '(quote-prefix))
                              (Pos 1 10)
                              '("empty-prefix"))
                        (list "#lang racket/base\n(require #:only-in racket/base)\n"
                              (hash "" '(empty-prefix) "#:only-in" '(keyword-prefix))
                              (Pos 1 18)
                              '("empty-prefix"))))])
      (match-define (list text prefix->completions pos expected) spec)
      (define d (make-doc-with-online-completions text prefix->completions))
      (define result (doc-completion d pos))
      (check-equal? (map CompletionItem-label (CompletionList-items result)) expected))))
