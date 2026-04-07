#lang racket

(module+ test
  (require rackunit
           "../../doclib/doc.rkt"
           "../../doclib/lexer.rkt"
           "../../common/interfaces.rkt"
           data/interval-map)

  (test-case
    "build-lexer-snapshot extracts symbol ranges"
    (define snapshot (build-lexer-snapshot "#lang racket\n(define x 1)\n"))
    (define syms (lexer-snapshot-symbols snapshot))
    ;; Cached lexer positions are normalized to the doc's 0-based offsets:
    ;; define: 14..20, x: 21..22, 1: 23..24
    (check-equal? (interval-map-ref syms 14 #f) (list "define" SymbolKind-Variable))
    (check-equal? (interval-map-ref syms 21 #f) (list "x" SymbolKind-Variable))
    (check-equal? (interval-map-ref syms 23 #f) (list "1" SymbolKind-Constant))
    (check-false (interval-map-ref syms 20 #f) "space should not be a symbol"))

  (test-case
    "lexer snapshot position queries return token and symbol entries"
    (define snapshot
      (build-lexer-snapshot "#lang racket/base\n(define greeting \"hello\")\n"))
    (check-equal?
      (lexer-snapshot-symbol-at snapshot 36)
      (list "\"hello\"" SymbolKind-String))
    (check-false (lexer-snapshot-symbol-at snapshot 18))

    (define token (lexer-snapshot-token-at snapshot 36))
    (check-true (LexerEntry? token))
    (check-equal? (LexerEntry-text token) "\"hello\"")
    (check-equal? (LexerEntry-type token) 'string)
    (check-false (lexer-snapshot-token-at snapshot 18)))

  (test-case
    "Document symbol cache invalidates on edits and resets"
    (define d (make-doc "file:///test.rkt" "foo"))
    (define initial-syms (doc-get-symbols d))
    (check-equal? (interval-map-ref initial-syms 0 #f)
                  (list "foo" SymbolKind-Variable))

    (doc-apply-edit! d
                     (Range (Pos 0 0) (Pos 0 3))
                     "bar")
    (define edited-syms (doc-get-symbols d))
    (check-false (eq? initial-syms edited-syms)
                 "edits should invalidate the cached symbol map")
    (check-equal? (interval-map-ref edited-syms 0 #f)
                  (list "bar" SymbolKind-Variable))

    (doc-reset! d "\"baz\"")
    (define reset-syms (doc-get-symbols d))
    (check-false (eq? edited-syms reset-syms)
                 "resets should invalidate the cached symbol map")
    (check-equal? (interval-map-ref reset-syms 0 #f)
                  (list "\"baz\"" SymbolKind-String)))

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
