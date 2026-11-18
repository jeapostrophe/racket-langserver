#lang racket/base

(module+ test
  (require rackunit
           "../../../common/interfaces.rkt"
           "../../../common/json-util.rkt"
           "../../json-dsl.rkt"
           "../with-document.rkt")

  (define uri "file:///test.rkt")
  (define code
#<<END
#lang racket/base

(define x 0)

(let ([x 1])
  x)
END
    )

  (define (pos-range start-line start-char end-line end-char)
    (Range (Pos start-line start-char) (Pos end-line end-char)))

  (define (symbol-entry name kind start-line start-char end-line end-char)
    (SymbolInformation name kind (Location uri (pos-range start-line start-char end-line end-char))))

  (test-case "definition, documentHighlight, documentSymbol, and references locate bound occurrences"
    (with-document uri code
      (lambda (lsp)
        (define def-result
          (lsp-result! lsp "textDocument/definition" uri (json {#:position (->jsexpr (Pos 5 2))})))
        (check-equal? (jsexpr->Location def-result)
                      (Location uri (pos-range 4 7 4 8)))

        (define highlight-result
          (lsp-result! lsp "textDocument/documentHighlight" uri (json {#:position (->jsexpr (Pos 5 2))})))
        (check-equal? (map jsexpr->DocumentHighlight highlight-result)
                      (list (DocumentHighlight (pos-range 4 7 4 8))
                            (DocumentHighlight (pos-range 5 2 5 3))))

        (check-equal?
          (map jsexpr->SymbolInformation (lsp-result! lsp "textDocument/documentSymbol" uri))
          (list (symbol-entry "define" SymbolKind-Variable 2 1 2 7)
                (symbol-entry "x" SymbolKind-Variable 2 8 2 9)
                (symbol-entry "0" SymbolKind-Constant 2 10 2 11)
                (symbol-entry "let" SymbolKind-Variable 4 1 4 4)
                (symbol-entry "x" SymbolKind-Variable 4 7 4 8)
                (symbol-entry "1" SymbolKind-Constant 4 9 4 10)
                (symbol-entry "x" SymbolKind-Variable 5 2 5 3)))

        (define ref-result
          (lsp-result! lsp
                       "textDocument/references"
                       uri
                       (json {#:position (->jsexpr (Pos 4 7))
                              #:context {#:includeDeclaration #t}})))
        (check-equal? (map jsexpr->Location ref-result)
                      (list (Location uri (pos-range 4 7 4 8))
                            (Location uri (pos-range 5 2 5 3))))))))
