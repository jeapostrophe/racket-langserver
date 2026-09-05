#lang racket

(define uri "file:///test.rkt")

(define code
#<<END
#lang racket

(struct point (x y))
(struct point3 point (z))

(point 1 2)
(point3 1 2 3)
(define x 1)
(point x 2)
(point-x (point 1 2))
(point 1)
(struct opt (a [b #:auto]) #:transparent)
(opt 1)
(define-struct pair (l r))
(make-pair 1 2)
(match (point 9 8) [(point a b) a])
(struct/contract vec ([dx real?] [dy real?]))
(vec 1 2)
(struct/contract vec3 vec ([dz real?]))
(vec3 1 2 3)
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
             [hints (hash-ref resp 'result)])
        (check-equal?
          (map hint->triple hints)
          (list
            ;; (point 1 2)
            '(5 7 "x ")
            '(5 9 "y ")
            ;; (point3 1 2 3), the supertype's fields come first
            '(6 8 "x ")
            '(6 10 "y ")
            '(6 12 "z ")
            ;; (point x 2), an argument already named after its field is left
            ;; alone. (point 1) and (define x 1) are not constructor calls of
            ;; matching length, and (make-pair 1 2) has no definition Check
            ;; Syntax can resolve.
            '(8 9 "y ")
            ;; (point-x (point 1 2)), the accessor is not a constructor and
            ;; the nested call still gets hints
            '(9 16 "x ")
            '(9 18 "y ")
            ;; (opt 1), the #:auto field is filled by the struct, not the call
            '(12 5 "a ")
            ;; (match (point 9 8) [(point a b) a]), both the call and the
            ;; pattern name their fields
            '(15 14 "x ")
            '(15 16 "y ")
            '(15 27 "x ")
            '(15 29 "y ")
            ;; (vec 1 2), `struct/contract` declares fields the same way, the
            ;; contract sitting where a field option would
            '(17 5 "dx ")
            '(17 7 "dy ")
            ;; (vec3 1 2 3), and it subtypes the same way too
            '(19 6 "dx ")
            '(19 8 "dy ")
            '(19 10 "dz ")))

        (for ([hint (in-list hints)])
          ;; InlayHintKind.Parameter
          (check-equal? (hash-ref hint 'kind) 2))

        (check-equal? (hash-ref (first hints) 'tooltip)
                      "field x of struct point")))))
