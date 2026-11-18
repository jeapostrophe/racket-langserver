#lang racket/base

;; Struct field hints and Typed Racket type hints share one document, and an
;; annotated field spec still reads as the field name.
(module+ test
  (require racket/list
           rackunit
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "inlay-hint-support.rkt"
           "with-document.rkt")

  (define uri "file:///test.rkt")
  (define code
#<<END
#lang typed/racket

(struct point ([x : Integer] [y : Integer]))
(define p (point 1 2))
END
    )

  (test-case "textDocument/inlayHint combines Typed Racket type hints and parameter hints"
    (with-document uri code
      (lambda (lsp)
        (define hints (inlay-hints-for-range lsp uri))
        (define (by-kind kind)
          (define expected (->jsexpr kind))
          (for/list ([hint (in-list hints)]
                     #:when (= expected (hash-ref hint 'kind)))
            (hint->triple hint)))
        (check-equal? (by-kind InlayHintKind-Parameter) '((3 17 "x ") (3 19 "y ")))
        (check-equal? (map (lambda (triple) (take triple 2)) (by-kind InlayHintKind-Type))
                      '((3 9)))))))
