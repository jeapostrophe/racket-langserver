#lang racket

(module+ test
  (require rackunit
           "../../doc.rkt"
           json)

  (test-case
    "Document signature help"
    (define text
#<<END
#lang racket/base

(list )
END
      )
    (define uri "file:///tmp/signature-help-test.rkt")

    (define d (new-doc uri text 1))
    (doc-expand! d)

    ;; (list ) -> cursor at line 2, character 6 (inside the closing paren)
    ;; "l" is at 2:1
    ;; "(" is at 2:0
    ;; "list" ends at 2:5
    ;; " " is at 2:5
    ;; ")" is at 2:6
    ;; so inside we want 2:6
    (define help (doc-signature-help d 2 6))

    (check-not-equal? help (json-null) "help should not be null")
    (check-true (hash-has-key? help 'signatures) "help should have signatures")
    (define sigs (hash-ref help 'signatures))
    (check-false (empty? sigs) "signatures should not be empty")
    (define first-sig (first sigs))
    (check-true (string-contains? (hash-ref first-sig 'label) "list") "label should contain 'list'")
    )
  )
