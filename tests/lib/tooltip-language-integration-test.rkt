#lang racket/base

;; Real producer coverage for both tooltip ingestion mechanisms. These packages
;; remain optional for normal development; the dedicated CI job requires them.

(require rackunit
         "../../common/interfaces.rkt"
         "../../doclib/doc.rkt"
         racket/string)

(define require-tooltip-languages?
  (string=? (or (getenv "RACKET_LANGSERVER_REQUIRE_TOOLTIP_LANGUAGES") "")
            "1"))

(define (module-available? module-path)
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (dynamic-require module-path #f)
    #t))

(define nanopass-available?
  (module-available? 'nanopass))
(define pie-available?
  (module-available? 'pie))

(define (expanded-hover uri text position)
  (define doc
    (make-doc uri text))
  (check-true (doc-expand! doc))
  (define hover
    (doc-hover doc position))
  (check-not-false hover)
  (Hover-contents hover))

(module+ test
  (when require-tooltip-languages?
    (check-true nanopass-available?
                "the tooltip-language CI job must install nanopass")
    (check-true pie-available?
                "the tooltip-language CI job must install pie"))

  (test-case
    "Nanopass post-syntax properties produce mouse-over status"
    (when nanopass-available?
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
          ;; The space between L0 and Expr is covered by the Nanopass tooltip,
          ;; but not by an identifier-specific binding status.
          (Pos 7 27)))
      (check-true (string-prefix? contents "**Mouse-over status**"))
      (check-true (string-contains? contents "Language L0:"))
      (check-false (string-contains? contents "**Log tooltip**"))
      (check-false (string-contains? contents "**Type**"))))

  (test-case
    "Pie online payloads produce one log tooltip"
    (when pie-available?
      (define contents
        (expanded-hover
          "file:///tmp/pie-tooltip-integration.rkt"
          (string-append
            "#lang pie\n"
            "(claim lst (List Nat))\n"
            "(define lst (:: 1 nil))\n"
            "lst\n")
          (Pos 3 1)))
      ;; Pie publishes through both successful expansion and the online log.
      ;; Equal ranges use the later log record, so only its mechanism is shown.
      (check-equal?
        (length (regexp-match* #rx"\\*\\*Log tooltip\\*\\*" contents))
        1)
      (check-true (string-contains? contents "(List Nat)"))
      (check-false (string-contains? contents "**Mouse-over status**"))
      (check-false (string-contains? contents "**Type**")))))
