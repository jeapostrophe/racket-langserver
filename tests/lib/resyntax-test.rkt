#lang racket/base

(module+ test
  (require "../silent-logger.rkt"
           rackunit
           racket/file
           racket/list
           "../../common/interfaces.rkt"
           "../../common/path-util.rkt"
           "../../doclib/doc.rkt"
           "../../doclib/internal-types.rkt")

  (define nested-or-code "#lang racket\n(or 1 (or 2 3))")

  (define (nested-or-single-result)
    (define d (make-doc "file:///test.rkt" nested-or-code))
    (define results (doc-resyntax d))
    (check-equal? (length results) 1)
    (values d (first results)))

  (when (doc-resyntax-available?)
    (test-case "doc-get-resyntax-results: initial state is empty"
      (define d (make-doc "file:///test.rkt" "sample code"))
      (check-equal? (doc-get-resyntax-results d) '()))

    (test-case "doc-update-resyntax-result!: update resyntax results"
      (define d (make-doc "file:///test.rkt" "sample code"))
      (define res1 (Resyntax-Result #:start 0 #:end 6 #:message "Test message" #:rule-name 'rule1 #:new-text "new"))
      (define res2 (Resyntax-Result #:start 7 #:end 11 #:message "Another message" #:rule-name 'rule2 #:new-text "code"))
      (define results (list res1 res2))
      (doc-update-resyntax-result! d results)
      (check-equal? (doc-get-resyntax-results d) results))

    (test-case "resyntax-result->diag: converts single-line and multi-line ranges"
      (define d1 (make-doc "file:///test.rkt" "hello world"))
      (define diag1
        (resyntax-result->diag
          d1
          (Resyntax-Result #:start 0 #:end 5 #:message "Say bye" #:rule-name 'greeting #:new-text "goodbye")))
      (check-equal? (Diagnostic-message diag1) "[greeting] Say bye")
      (check-equal? (Diagnostic-severity diag1) DiagnosticSeverity-Information)
      (check-equal? (Diagnostic-source diag1) "Resyntax")
      (check-equal? (Diagnostic-range diag1) (Range (Pos 0 0) (Pos 0 5)))

      (define d2 (make-doc "file:///test.rkt" "line1\nline2\nline3"))
      (define diag2
        (resyntax-result->diag
          d2
          (Resyntax-Result #:start 6 #:end 11 #:message "Update line2" #:rule-name 'line-rule #:new-text "LINE2")))
      (check-equal? (Diagnostic-range diag2) (Range (Pos 1 0) (Pos 1 5))))

    (test-case "resyntax-result->code-action: converts to CodeAction with edit"
      (define uri "file:///test.rkt")
      (define d (make-doc uri "replace this"))
      (define res (Resyntax-Result #:start 0 #:end 7 #:message "Fix it" #:rule-name 'fix #:new-text "skip"))
      (define action (resyntax-result->code-action d res))
      (check-equal? (CodeAction-title action) "Apply rule [fix]")
      (check-equal? (CodeAction-kind action) "quickfix")
      (check-equal? (length (CodeAction-diagnostics action)) 1)
      (check-false (CodeAction-isPreferred action))
      (define changes (WorkspaceEdit-changes (CodeAction-edit action)))
      (define edits (hash-ref changes (string->symbol uri)))
      (check-equal? (length edits) 1)
      (check-equal? (TextEdit-newText (first edits)) "skip"))

    (test-case "doc-resyntax and doc-resyntax! return Resyntax-Result lists"
      (define d1 (make-doc "file:///test.rkt" "(+ 1 2)"))
      (check-equal? (doc-get-resyntax-results d1) '())
      (doc-resyntax! d1)
      (check-pred (lambda (rs) (and (list? rs) (andmap Resyntax-Result? rs)))
                  (doc-get-resyntax-results d1))

      (define d2 (make-doc "file:///test.rkt" "(define x 1)"))
      (define results (doc-resyntax d2))
      (check-pred list? results)
      (check-true (andmap Resyntax-Result? results)))

    (define (sample-resyntax-result)
      (Resyntax-Result #:start 13
                       #:end 28
                       #:message "Nested `or` expressions can be flattened."
                       #:rule-name 'nested-or-to-flat-or
                       #:new-text "(or 1 2 3)"))

    (test-case "doc-resyntax: finds nested OR simplification and maps ranges"
      (define-values (d res) (nested-or-single-result))
      (check-equal? (Resyntax-Result-start res) 13)
      (check-equal? (Resyntax-Result-end res) 28)
      (check-equal?
        (Resyntax-Result-message res)
        "Nested `or` expressions can be flattened into a single, equivalent `or` expression.")
      (check-equal? (Resyntax-Result-rule-name res) 'nested-or-to-flat-or)
      (check-equal? (Resyntax-Result-new-text res) "(or 1 2 3)")
      (define diag-range (Diagnostic-range (resyntax-result->diag d res)))
      (check-equal? (doc-pos->abs-pos d (Range-start diag-range)) 13)
      (check-equal? (doc-pos->abs-pos d (Range-end diag-range)) 28)
      (define action-edits
        (hash-ref (WorkspaceEdit-changes (CodeAction-edit (resyntax-result->code-action d res)))
                  (string->symbol (Doc-uri d))))
      (check-equal? (TextEdit-range (first action-edits)) diag-range))

    (test-case "doc-diagnostics and doc-get-resyntax-results invalidate on edit"
      (define d (make-doc "file:///test.rkt" nested-or-code))
      (define res (sample-resyntax-result))
      (doc-update-resyntax-result! d (list res))
      (define diags (doc-diagnostics d))
      (check-true
        (for/or ([diag (in-list diags)])
          (and (string=? (Diagnostic-source diag) "Resyntax")
               (string=? (Diagnostic-message diag)
                         (format "[~a] ~a"
                                 (Resyntax-Result-rule-name res)
                                 (Resyntax-Result-message res))))))
      (doc-reset! d (string-append nested-or-code "\n"))
      (check-equal? (doc-get-resyntax-results d) '())
      (check-false
        (for/or ([diag (in-list (doc-diagnostics d))])
          (string=? (Diagnostic-source diag) "Resyntax"))))

    (test-case "doc-code-action filters by range intersection and edit invalidation"
      (define d (make-doc "file:///test.rkt" nested-or-code))
      (define res (sample-resyntax-result))
      (doc-update-resyntax-result! d (list res))
      (define overlapping
        (Range (doc-abs-pos->pos d (sub1 (Resyntax-Result-start res)))
               (doc-abs-pos->pos d (add1 (Resyntax-Result-start res)))))
      (define (has-rule-action? actions)
        (for/or ([action (in-list actions)])
          (string=? (CodeAction-title action)
                    (format "Apply rule [~a]" (Resyntax-Result-rule-name res)))))
      (check-true (has-rule-action? (doc-code-action d overlapping)))
      (check-false (has-rule-action? (doc-code-action d (Range (Pos 0 0) (Pos 0 5)))))
      (doc-reset! d (string-append nested-or-code "\n"))
      (check-false (has-rule-action? (doc-code-action d overlapping))))

    (test-case "relative imports resolve from the document directory"
      (define root (make-temporary-file "resyntax-rel-test~a" 'directory))
      (dynamic-wind
        void
        (lambda ()
          (define sub-dir (build-path root "sub"))
          (make-directory sub-dir)
          (display-to-file "#lang racket/base\n(provide val)\n(define val 42)\n"
                           (build-path sub-dir "helper.rkt"))
          (define main-path (build-path sub-dir "main.rkt"))
          (display-to-file "#lang racket/base\n" main-path)
          (define code "#lang racket/base\n(require \"helper.rkt\")\n(or 1 (or 2 3))\n")
          (define doc (make-doc (path->uri main-path) code))
          (parameterize ([current-directory root])
            (define results (doc-resyntax doc))
            (check-equal? (length results) 1)
            (check-equal? (Resyntax-Result-rule-name (first results)) 'nested-or-to-flat-or)))
        (lambda () (delete-directory/files root))))))
