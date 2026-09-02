#lang racket

;; Struct field hints and Typed Racket type hints share one document, and an
;; annotated field spec still reads as the field name.

(define uri "file:///test.rkt")

(define code
#<<END
#lang typed/racket

(struct point ([x : Integer] [y : Integer]))
(define p (point 1 2))
END
  )

(module+ test
  (require rackunit
           "with-document.rkt")

  (define (hint->triple hint)
    (define position (hash-ref hint 'position))
    (list (hash-ref position 'line)
          (hash-ref position 'character)
          (hash-ref hint 'label)))

  (with-document uri code
    (λ (lsp)

      (define inlay-hint-req
        (make-request lsp
                      "textDocument/inlayHint"
                      (hasheq 'textDocument
                              (hasheq 'uri uri)
                              'range
                              (hasheq 'start (hasheq 'line 0 'character 0)
                                      'end (hasheq 'line 30 'character 0)))))
      (client-send lsp inlay-hint-req)

      (let* ([resp (client-wait-response inlay-hint-req)]
             [hints (hash-ref resp 'result)]
             [by-kind (λ (kind)
                        (for/list ([hint (in-list hints)]
                                   #:when (= kind (hash-ref hint 'kind)))
                          (hint->triple hint)))])
        ;; InlayHintKind.Parameter, the arguments of (point 1 2)
        (check-equal? (by-kind 2)
                      (list '(3 17 "x ")
                            '(3 19 "y ")))
        ;; InlayHintKind.Type, the inferred type of p
        (check-equal? (map (λ (triple) (take triple 2)) (by-kind 1))
                      (list '(3 9)))))))
