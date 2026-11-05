#lang racket/base

(require rackunit
         racket/runtime-path)

(define-runtime-path doc "../../doclib/doc.rkt")
(define-runtime-path adapter "../../doclib/external/resyntax.rkt")
(define-runtime-path controller-path "../../lsp/source-controller.rkt")

(module+ test
  (test-case "loading Doc defers Resyntax until availability is queried"
    (define imports 0)
    (parameterize ([current-namespace (make-base-namespace)])
      (define resolver (current-module-name-resolver))
      (define (map-path path)
        (when (eq? path 'resyntax/grimoire/source)
          (set! imports (add1 imports)))
        (if (and (symbol? path) (regexp-match? #rx"^resyntax(/|$)" (symbol->string path)))
            'racket/base
            path))
      (parameterize ([current-module-name-resolver
                      (case-lambda
                        [(path relative stx load?) (resolver (map-path path) relative stx load?)]
                        [(path namespace) (resolver path namespace)])])
        (dynamic-require doc #f)
        (check-equal? imports 0)
        (define make-controller (dynamic-require controller-path 'make-source-controller))
        (define stop (dynamic-require `(submod ,controller-path owner) 'source-controller-shutdown!))
        (define controller (make-controller))
        (stop controller)
        (check-equal? imports 0)
        (define available? (dynamic-require adapter 'resyntax-available?))
        (check-false (available?))
        (check-true (positive? imports))
        (define count imports)
        (check-false (available?))
        (check-equal? imports count)
        (define run (dynamic-require adapter 'run-resyntax))
        (check-equal? (run "#lang racket\n(or 1 (or 2 3))" "file:///test.rkt") '())))))
