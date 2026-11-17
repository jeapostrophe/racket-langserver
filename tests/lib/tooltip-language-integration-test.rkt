#lang racket/base

;; Real producer coverage for both tooltip ingestion mechanisms. Skips silently
;; when nanopass or pie is not installed; the dedicated CI job installs both.
(module+ test
  (require "../silent-logger.rkt"
           rackunit
           racket/string
           "../../common/interfaces.rkt"
           "../../doclib/doc.rkt")

  (define (module-available? module-path)
    (with-handlers ([exn:fail? (lambda (_exn) #f)])
      (dynamic-require module-path #f)
      #t))

  (define (expanded-hover uri text position)
    (define doc (make-doc uri text))
    (check-true (doc-expand! doc))
    (define hover (doc-hover doc position))
    (check-not-false hover)
    (Hover-contents hover))

  (test-case "Nanopass post-syntax properties produce mouse-over status"
    (when (module-available? 'nanopass)
      (define contents
        (expanded-hover
          "file:///tmp/nanopass-tooltip-integration.rkt"
          (string-append
            "#lang racket\n"
            "(require nanopass)\n"
            "(define variable? symbol?)\n"
            "(define-language L0\n"
            "  (terminals (variable (x)))\n"
            "  (Expr (e) x))\n"
            "(define value\n"
            "  (with-output-language (L0 Expr) `x))\n")
          (Pos 7 27)))
      (check-true (string-prefix? contents "**Mouse-over status**"))
      (check-true (string-contains? contents "Language L0:"))
      (check-false (string-contains? contents "**Log tooltip**"))
      (check-false (string-contains? contents "**Type**"))))

  (test-case "Pie online payloads produce one log tooltip"
    (when (module-available? 'pie)
      (define contents
        (expanded-hover
          "file:///tmp/pie-tooltip-integration.rkt"
          (string-append
            "#lang pie\n"
            "(claim lst (List Nat))\n"
            "(define lst (:: 1 nil))\n"
            "lst\n")
          (Pos 3 1)))
      (check-equal? (length (regexp-match* #rx"\\*\\*Log tooltip\\*\\*" contents)) 1)
      (check-true (string-contains? contents "(List Nat)"))
      (check-false (string-contains? contents "**Mouse-over status**"))
      (check-false (string-contains? contents "**Type**")))))
