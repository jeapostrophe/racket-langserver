#lang racket/base

(module+ test
  (require "doc-test-support.rkt"
           rackunit
           racket/file
           racket/match
           racket/string
           "../../common/interfaces.rkt"
           "../../common/source.rkt"
           "../../doclib/check-syntax.rkt"
           "../../doclib/doc.rkt"
           "../../doclib/internal-types.rkt")

  (define (language-header-diagnostics diags)
    (for/list ([diag (in-list diags)]
               #:when (string=? (Diagnostic-source diag) "Language Header Check"))
      diag))

  (define (find-diagnostic-by-message diags expected-message)
    (for/first ([diag (in-list diags)]
                #:when (string=? (Diagnostic-message diag) expected-message))
      diag))

  (test-case "Expansion logs preserve emission order"
    (define text
      (string-append
        "#lang racket/base\n"
        "(require (for-syntax racket/base))\n"
        "(begin-for-syntax\n"
        "  (log-message (current-logger) 'info 'order-test \"first\" 'first)\n"
        "  (log-message (current-logger) 'info 'order-test \"second\" 'second))\n"))
    (define path (string->path "/tmp/expansion-log-order-test.rkt"))
    (define trace (make-trace path text))
    (define result (expand-source (path->source path) (open-input-string text) trace))
    (define order-test-data
      (for/list ([record (in-list (ExpandResult-logs result))]
                 #:when (eq? (vector-ref record 3) 'order-test))
        (vector-ref record 2)))
    (check-equal? order-test-data '(first second)))

  (test-case "Document diagnostics report missing language headers"
    (define text "(define x 1)\n")
    (define diags
      (check-syntax-diagnostics (uri->source "file:///tmp/missing-language-test.rkt") text))
    (define diag
      (find-diagnostic-by-message
        diags
        "Missing language header. Start the file with `#lang <language>`, `#reader <reader>`, or `(module <name> <language> ...)`."))
    (check-not-false diag)
    (check-equal? (Diagnostic-source diag) "Language Header Check")
    (check-equal? (Diagnostic-range diag)
                  (Range (Pos 0 0) (Pos 0 (string-length "(define x 1)")))))

  (test-case "Document diagnostics accept unknown language headers"
    (define diags
      (check-syntax-diagnostics (uri->source "file:///tmp/unknown-language-test.rkt")
                                "#lang not-a-real-language\n1\n"))
    (check-equal? (language-header-diagnostics diags) '()))

  (test-case "Relative imports in a document without a directory are refused, not resolved from CWD"
    (define root (make-temporary-file "unbased-import~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (display-to-file "#lang racket/base\n(provide value)\n(define value 1)\n"
                         (build-path root "dependency.rkt"))
        (parameterize ([current-directory root])
          (define message
            "Relative imports need a save location. Save this document to import files relative to it.")
          (define resolver-receives-syntax?
            (let ([resolve (current-module-name-resolver)]
                  [received #f])
              (parameterize ([current-namespace (make-base-namespace)]
                             [current-module-name-resolver
                              (case-lambda
                                [(name namespace) (resolve name namespace)]
                                [(module relative-to stx load?)
                                 (when (equal? module "dependency.rkt") (set! received (syntax? stx)))
                                 (resolve module relative-to stx load?)])])
                (expand '(module probe racket/base (require "dependency.rkt"))))
              received))
          (define (located range)
            (if resolver-receives-syntax? range (Range (Pos 0 0) (Pos 0 17))))
          (define (refusals text)
            (for/list ([diag (in-list (check-syntax-diagnostics (uri->source "untitled:Untitled-1") text))]
                       #:when (string-contains? (Diagnostic-message diag) message))
              (Diagnostic-range diag)))
          (check-equal? (refusals "#lang racket/base\n(require \"dependency.rkt\")\nvalue\n")
                        (list (located (Range (Pos 1 9) (Pos 1 25)))))
          (check-equal? (refusals "#lang racket/base\n(require (submod \"dependency.rkt\" x))\n")
                        (list (located (Range (Pos 1 9) (Pos 1 36)))))
          (check-equal? (refusals "#lang racket/base\n(require racket/list (file \"/no/such/file.rkt\"))\n")
                        '())))
      (lambda () (delete-directory/files root))))

  (for ([spec (in-list
                (list
                  (list "simplify missing collection messages"
                        "file:///tmp/missing-collection-test.rkt"
                        "#lang racke\n"
                        (string-append
                          "Cannot find language \"racke\".\n"
                          "  module path: racke/lang/reader\n"
                          "Check that the language name is correct and the package is installed."))
                  (list "do not label requires as #lang failures"
                        "file:///tmp/missing-require-collection-test.rkt"
                        "#lang racket/base\n(require racke/lang/reader)\n"
                        (string-append
                          "Cannot find language \"racke\".\n"
                          "  module path: racke/lang/reader\n"
                          "Check that the language name is correct and the package is installed."))
                  (list "report missing require modules"
                        "file:///tmp/missing-require-module-test.rkt"
                        "#lang racket/base\n(require foo/bar)\n"
                        (string-append
                          "Cannot find module \"foo/bar\" in collection \"foo\".\n"
                          "Check that the module name is correct and the package is installed."))))])
    (match-define (list label uri text expected-message) spec)
    (test-case (format "Document diagnostics ~a" label)
      (define diags (check-syntax-diagnostics (uri->source uri) text))
      (define diag (find-diagnostic-by-message diags expected-message))
      (check-not-false diag)
      (check-equal? (Diagnostic-source diag) "Racket")))

  (test-case "Document diagnostics count UTF-16 code units after astral characters"
    (define diags
      (check-syntax-diagnostics (uri->source "file:///tmp/utf16-diagnostic-test.rkt")
                                "#lang racket/base\n\"\U0001F600\" unbound-name\n"))
    (define diag
      (for/first ([diag (in-list diags)]
                  #:when (regexp-match? #rx"unbound-name" (Diagnostic-message diag)))
        diag))
    (check-not-false diag)
    (check-equal? (Diagnostic-range diag) (Range (Pos 1 5) (Pos 1 17))))

  (test-case "Document diagnostics count a tab as one UTF-16 code unit"
    (define (read-error-range text)
      (define diags
        (check-syntax-diagnostics (uri->source "file:///tmp/tab-diagnostic-test.rkt") text))
      (check-equal? (length diags) 1)
      (Diagnostic-range (car diags)))
    (check-equal? (read-error-range "#lang racket/base\n\t(\n") (Range (Pos 1 1) (Pos 1 2)))
    (check-equal? (read-error-range "#lang racket/base\n\t\"\U0001F600\" (\n")
                  (Range (Pos 1 6) (Pos 1 7))))

  (test-case "Document diagnostics count each CRLF as two characters"
    (define (diagnostic-ranges text)
      (map Diagnostic-range
           (check-syntax-diagnostics (uri->source "file:///tmp/crlf-diagnostic-test.rkt") text)))
    (check-equal? (diagnostic-ranges "#lang racket/base\r\n(void)\r\nunbound-name\r\n")
                  (list (Range (Pos 2 0) (Pos 2 12))))
    (check-equal? (diagnostic-ranges "#lang racket/base\r\n\"\U0001F600\"\r\n(\r\n")
                  (list (Range (Pos 2 0) (Pos 2 1)))))

  (test-case "Document diagnostics accept wrapped #lang headers"
    (check-equal?
      (language-header-diagnostics
        (check-syntax-diagnostics (uri->source "file:///tmp/at-exp-language-test.rkt")
                                  "#lang at-exp racket\n@(+ 1 2)\n"))
      '())
    (check-equal?
      (language-header-diagnostics
        (check-syntax-diagnostics (uri->source "file:///tmp/s-exp-language-test.rkt")
                                  "#lang s-exp racket/base\n(+ 1 2)\n"))
      '()))

  (test-case "Document diagnostics use first line range for empty language spans"
    (define diags
      (check-syntax-diagnostics (uri->source "file:///tmp/empty-language-test.rkt")
                                "#lang \n(define x 1)\n"))
    (define diag
      (find-diagnostic-by-message
        diags
        "Incomplete language header. Provide the missing language or reader name."))
    (check-not-false diag)
    (check-equal? (Diagnostic-source diag) "Language Header Check")
    (check-equal? (Diagnostic-range diag)
                  (Range (Pos 0 0) (Pos 0 (string-length "#lang ")))))

  (test-case "rktd data file: no missing #lang or expansion diagnostics"
    (define d (make-doc "file:///tmp/doc-test-data.rktd" "((a . 1) (b . 2))"))
    (check-true (doc-expand! d))
    (check-equal? (doc-diagnostics d) '()))

  (test-case "rktd data file: read errors are still reported"
    (define d (make-doc "file:///tmp/doc-test-data.rktd" "((a . 1)"))
    (check-false (doc-expand! d)))

  (test-case "rkt file without #lang still reports missing #lang"
    (define d (make-doc "file:///tmp/doc-test-nolang.rkt" "1234"))
    (check-true (doc-expand! d))
    (check-true (for/or ([diag (in-list (doc-diagnostics d))])
                  (regexp-match? #rx"#lang" (Diagnostic-message diag)))))

  (test-case "Typed Racket tooltip decoder routes type errors into diagnostics"
    (when (typed-racket-available?)
      (define diags
        (check-syntax-diagnostics
          (uri->source "file:///tmp/typed-racket-error-test.rkt")
          "#lang typed/racket\n(+ 1 \"x\")\n"))
      (define typed-diag
        (for/first ([diag (in-list diags)]
                    #:when (string=? (Diagnostic-source diag) "Typed Racket"))
          diag))
      (check-not-false typed-diag)
      (check-equal? (Diagnostic-range typed-diag) (Range (Pos 1 5) (Pos 1 8)))
      (check-true (string-contains? (Diagnostic-message typed-diag) "expected: Number"))
      (check-false
        (for/or ([diag (in-list diags)])
          (and (string=? (Diagnostic-source diag) "Racket")
               (string-contains? (Diagnostic-message diag) "Type Checker")))))))
