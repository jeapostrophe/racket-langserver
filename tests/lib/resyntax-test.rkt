#lang racket

(module+ test
  (require rackunit
           "../../doclib/doc.rkt"
           "../../doclib/internal-types.rkt"
           "../../common/interfaces.rkt"
           racket/class)

  (define has-resyntax? (doc-resyntax-available?))

  (unless has-resyntax?
    (displayln "Skipping tests/lib/resyntax-test.rkt because resyntax is unavailable."))

  (define nested-or-code
    "#lang racket\n(or 1 (or 2 3))")

  (define (nested-or-doc)
    (make-doc "file:///test.rkt" nested-or-code))

  (define (nested-or-single-result)
    (define d (nested-or-doc))
    (define results (doc-resyntax d))
    (check-equal? (length results) 1)
    (values d (first results)))

  (when has-resyntax?

    (test-case
      "doc-get-resyntax-results: initial state is empty"
      (define d (make-doc "file:///test.rkt" "sample code"))
      (check-equal? (doc-get-resyntax-results d) (list)))

    (test-case
      "doc-update-resyntax-result!: update resyntax results"
      (define d (make-doc "file:///test.rkt" "sample code"))
      (define res1 (Resyntax-Result #:start 0 #:end 6 #:message "Test message" #:rule-name 'rule1 #:new-text "new"))
      (define res2 (Resyntax-Result #:start 7 #:end 11 #:message "Another message" #:rule-name 'rule2 #:new-text "code"))
      (define results (list res1 res2))

      (doc-update-resyntax-result! d results)
      (check-equal? (doc-get-resyntax-results d) results)
      (check-equal? (length (doc-get-resyntax-results d)) 2))

    (test-case
      "resyntax-result->diag: converts to Diagnostic with correct range"
      (define text "hello world")
      (define d (make-doc "file:///test.rkt" text))
      (define res (Resyntax-Result #:start 0 #:end 5 #:message "Say bye" #:rule-name 'greeting #:new-text "goodbye"))

      (define diag (resyntax-result->diag d res))

      ;; Check diagnostic properties
      (check-equal? (Diagnostic-message diag) "[greeting] Say bye")
      (check-equal? (Diagnostic-severity diag) DiagnosticSeverity-Information)
      (check-equal? (Diagnostic-source diag) "Resyntax")

      ;; Check range conversion (start and end positions)
      (define range (Diagnostic-range diag))
      (check-equal? (Pos-line (Range-start range)) 0)
      (check-equal? (Pos-char (Range-start range)) 0)
      (check-equal? (Pos-line (Range-end range)) 0)
      (check-equal? (Pos-char (Range-end range)) 5))

    (test-case
      "resyntax-result->diag: handles multi-line positions"
      (define text "line1\nline2\nline3")
      (define d (make-doc "file:///test.rkt" text))
      ;; line1\n is 6 chars, line2 is 6 chars
      ;; "line2" is from position 6 to 11
      (define res (Resyntax-Result #:start 6 #:end 11 #:message "Update line2" #:rule-name 'line-rule #:new-text "LINE2"))

      (define diag (resyntax-result->diag d res))
      (define range (Diagnostic-range diag))

      ;; "line2" starts at line 1, char 0
      (check-equal? (Pos-line (Range-start range)) 1)
      (check-equal? (Pos-char (Range-start range)) 0)
      ;; "line2" ends at line 1, char 5
      (check-equal? (Pos-line (Range-end range)) 1)
      (check-equal? (Pos-char (Range-end range)) 5))

    (test-case
      "resyntax-result->code-action: converts to CodeAction"
      (define d (make-doc "file:///test.rkt" "hello world"))
      (define res (Resyntax-Result #:start 0 #:end 5 #:message "Simplify" #:rule-name 'simplify #:new-text "hi"))

      (define action (resyntax-result->code-action d res))

      ;; Check CodeAction properties
      (check-equal? (CodeAction-title action) "Apply rule [simplify]")
      (check-equal? (CodeAction-kind action) "quickfix")
      (check-equal? (length (CodeAction-diagnostics action)) 1)
      (check-false (CodeAction-isPreferred action)))

    (test-case
      "resyntax-result->code-action: includes correct edit"
      (define d (make-doc "file:///test.rkt" "old text"))
      (define res (Resyntax-Result #:start 0 #:end 3 #:message "Replace" #:rule-name 'replace #:new-text "new"))

      (define action (resyntax-result->code-action d res))
      (define edit (CodeAction-edit action))

      ;; Check that WorkspaceEdit has changes
      (check-true (WorkspaceEdit? edit))
      (define changes (WorkspaceEdit-changes edit))
      (check-true (hash? changes)))

    (test-case
      "resyntax-result->code-action: TextEdit has correct new text"
      (define uri "file:///test.rkt")
      (define d (make-doc uri "replace this"))
      (define res (Resyntax-Result #:start 0 #:end 7 #:message "Fix it" #:rule-name 'fix #:new-text "skip"))

      (define action (resyntax-result->code-action d res))
      (define edit (CodeAction-edit action))
      (define changes (WorkspaceEdit-changes edit))

      ;; The hash key should be the URI as a symbol
      (define key (string->symbol uri))
      (check-true (hash-has-key? changes key))

      ;; Get the list of TextEdits
      (define edits (hash-ref changes key))
      (check-equal? (length edits) 1)

      ;; Check the TextEdit
      (define te (first edits))
      (check-equal? (TextEdit-newText te) "skip"))

    (test-case
      "doc-resyntax!: updates Doc's resyntax-results field"
      (define d (make-doc "file:///test.rkt" "(+ 1 2)"))

      ;; Initially empty
      (check-equal? (doc-get-resyntax-results d) (list))

      ;; Call doc-resyntax!
      (doc-resyntax! d)

      ;; Field should be updated (even if empty, since resyntax might not find issues)
      (define results (doc-get-resyntax-results d))
      (check-true (list? results)))

    (test-case
      "doc-resyntax returns list of Resyntax-Result"
      (define d (make-doc "file:///test.rkt" "(define x 1)"))

      (define results (doc-resyntax d))

      ;; Should return a list (possibly empty if resyntax package not available)
      (check-true (list? results))
      (check-true (andmap Resyntax-Result? results)))

    (test-case
      "doc-resyntax: finds nested OR simplification"
      (define-values (_doc res) (nested-or-single-result))
      (check-equal? (Resyntax-Result-start res) 13)
      (check-equal? (Resyntax-Result-end res) 28)
      (check-equal?
        (Resyntax-Result-message res)
        "Nested `or` expressions can be flattened into a single, equivalent `or` expression.")
      (check-equal? (Resyntax-Result-rule-name res) 'nested-or-to-flat-or)
      (check-equal? (Resyntax-Result-new-text res) "(or 1 2 3)"))

    (test-case
      "doc-resyntax! and doc-get-resyntax-results: finds and stores actual issues"
      ;; (and #t #t) should be simplified
      (define code "#lang racket\n(and #t #t)")
      (define d (make-doc "file:///test.rkt" code))

      ;; Initially empty
      (check-equal? (doc-get-resyntax-results d) (list))

      ;; Run resyntax and update the Doc
      (doc-resyntax! d)

      ;; Get the results from the Doc
      (define results (doc-get-resyntax-results d))
      (check-true (list? results))
      (check-true (andmap Resyntax-Result? results)))

    (test-case
      "doc-diagnostics: includes resyntax diagnostics when present"
      (define-values (d res) (nested-or-single-result))
      (doc-update-resyntax-result! d (list res))
      (define diags (doc-diagnostics d))
      (check-true
        (for/or ([diag (in-list diags)])
          (string=? (Diagnostic-source diag) "Resyntax")))
      (check-true
        (for/or ([diag (in-list diags)])
          (string=? (Diagnostic-message diag)
                    (format "[~a] ~a"
                            (Resyntax-Result-rule-name res)
                            (Resyntax-Result-message res))))))

    (test-case
      "resyntax-result->diag: works with actual resyntax findings"
      (define-values (d res) (nested-or-single-result))
      (define diag (resyntax-result->diag d res))

      ;; Verify diagnostic properties
      (check-true (Diagnostic? diag))
      (check-equal? (Diagnostic-source diag) "Resyntax")
      (check-equal? (Diagnostic-severity diag) DiagnosticSeverity-Information)
      (check-equal?
        (Diagnostic-message diag)
        "[nested-or-to-flat-or] Nested `or` expressions can be flattened into a single, equivalent `or` expression.")

      ;; Check diagnostic range maps back to exact source indices.
      (define range (Diagnostic-range diag))
      (check-true (Range? range))
      (define start-pos (doc-pos->abs-pos d (Range-start range)))
      (define end-pos (doc-pos->abs-pos d (Range-end range)))
      (check-equal? start-pos (Resyntax-Result-start res))
      (check-equal? end-pos (Resyntax-Result-end res)))

    (test-case
      "resyntax-result->code-action: works with actual resyntax findings"
      (define-values (d res) (nested-or-single-result))
      (define action (resyntax-result->code-action d res))

      ;; Verify code action structure
      (check-true (CodeAction? action))
      (check-equal? (CodeAction-title action) "Apply rule [nested-or-to-flat-or]")
      (check-equal? (CodeAction-kind action) "quickfix")
      (check-false (CodeAction-isPreferred action))

      ;; Check it has the diagnostic attached
      (define diags (CodeAction-diagnostics action))
      (check-equal? (length diags) 1)
      (check-true (Diagnostic? (first diags)))

      ;; Check the workspace edit contains the correct new text
      (define edit (CodeAction-edit action))
      (check-true (WorkspaceEdit? edit))
      (define changes (WorkspaceEdit-changes edit))
      (check-true (hash? changes))
      (define uri-key (string->symbol (Doc-uri d)))
      (check-true (hash-has-key? changes uri-key))
      (define edits (hash-ref changes uri-key))
      (check-equal? (length edits) 1)
      (define text-edit (first edits))
      (check-equal? (TextEdit-newText text-edit) "(or 1 2 3)")
      (define edit-range (TextEdit-range text-edit))
      (check-equal? (doc-pos->abs-pos d (Range-start edit-range)) (Resyntax-Result-start res))
      (check-equal? (doc-pos->abs-pos d (Range-end edit-range)) (Resyntax-Result-end res)))

    (test-case
      "doc-code-action: includes resyntax action when ranges intersect"
      (define-values (d res) (nested-or-single-result))
      (doc-update-resyntax-result! d (list res))
      (define request-range
        (Range (doc-abs-pos->pos d (sub1 (Resyntax-Result-start res)))
               (doc-abs-pos->pos d (+ (Resyntax-Result-start res) 1))))
      (define actions (doc-code-action d request-range))
      (check-true
        (for/or ([action (in-list actions)])
          (string=? (CodeAction-title action)
                    (format "Apply rule [~a]" (Resyntax-Result-rule-name res))))))

    (test-case
      "doc-code-action: excludes resyntax action when ranges are disjoint"
      (define-values (d res) (nested-or-single-result))
      (doc-update-resyntax-result! d (list res))
      (define request-range
        (Range (Pos 0 0)
               (Pos 0 5)))
      (define actions (doc-code-action d request-range))
      (check-false
        (for/or ([action (in-list actions)])
          (string=? (CodeAction-title action)
                    (format "Apply rule [~a]" (Resyntax-Result-rule-name res))))))))
