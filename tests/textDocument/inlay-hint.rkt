#lang racket/base

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

  (test-case "textDocument/inlayHint emits parameter hints for struct constructors and patterns"
    (with-document uri code
      (lambda (lsp)
        (define hints (inlay-hints-for-range lsp uri))
        (check-equal?
          (map hint->triple hints)
          (list
            ;; (point 1 2)
            '(5 7 "x ")
            '(5 9 "y ")
            ;; (point3 1 2 3)
            '(6 8 "x ")
            '(6 10 "y ")
            '(6 12 "z ")
            ;; (point x 2) skips argument already named `x`
            '(8 9 "y ")
            ;; (point-x (point 1 2))
            '(9 16 "x ")
            '(9 18 "y ")
            ;; (opt 1) skips #:auto field
            '(12 5 "a ")
            ;; (match (point 9 8) [(point a b) a])
            '(15 14 "x ")
            '(15 16 "y ")
            '(15 27 "x ")
            '(15 29 "y ")
            ;; (vec 1 2) and (vec3 1 2 3)
            '(17 5 "dx ")
            '(17 7 "dy ")
            '(19 6 "dx ")
            '(19 8 "dy ")
            '(19 10 "dz ")))
        (for ([hint (in-list hints)])
          (check-equal? (hash-ref hint 'kind) (->jsexpr InlayHintKind-Parameter)))
        (check-equal? (hash-ref (first hints) 'tooltip)
                      "field x of struct point")))))
