#lang racket

(module+ test
  (require rackunit
           "../../doc.rkt"
           "../../interfaces.rkt")

  (test-case
    "Document code action"
    (define text
#<<END
#lang racket/base

(define x 0)
END
      )
    (define uri "file:///tmp/code-action-test.rkt")

    (define d (new-doc uri text 1))
    (doc-expand! d)

    ;; x is at line 2, character 8
    (define start (Pos #:line 2 #:char 8))
    (define end (Pos #:line 2 #:char 9))

    (define actions (doc-code-action d start end))

    (check-false (empty? actions) "actions should not be empty")
    (define act (first actions))
    (check-equal? (hash-ref act 'title) "Add prefix `_` to ignore")
    )
  )

