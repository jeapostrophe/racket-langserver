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
#lang racket
(define x 1)
(define (f y)
  (define inner 2)
  inner)
END
    )

  ;; A client that declares hierarchicalDocumentSymbolSupport receives a
  ;; DocumentSymbol tree instead of a flat SymbolInformation list.
  (test-case "textDocument/documentSymbol returns hierarchical symbols when supported"
    (with-document uri code
      #:capabilities (json {#:textDocument {#:documentSymbol {#:hierarchicalDocumentSymbolSupport #t}}})
      (lambda (lsp)
        (check-equal?
          (map jsexpr->DocumentSymbol (lsp-result! lsp "textDocument/documentSymbol" uri))
          (list
            (DocumentSymbol "x"
                            SymbolKind-Variable
                            (Range (Pos 1 0) (Pos 1 12))
                            (Range (Pos 1 8) (Pos 1 9))
                            '())
            (DocumentSymbol "f"
                            SymbolKind-Function
                            (Range (Pos 2 0) (Pos 4 8))
                            (Range (Pos 2 9) (Pos 2 10))
                            (list
                              (DocumentSymbol "inner"
                                              SymbolKind-Variable
                                              (Range (Pos 3 2) (Pos 3 18))
                                              (Range (Pos 3 10) (Pos 3 15))
                                              '())))))))))
