#lang racket

(module+ test
  (require rackunit
           "../../doc.rkt")

  (test-case
    "Document hover"
    (define text
#<<END
#lang racket
(list)
END
      )
    (define uri "file:///tmp/hover-test.rkt")

    (define d (new-doc uri text 1))
    (doc-expand! d)

    (define h (doc-hover d 1 1))

    (define result (hash-ref h 'contents))
    (check-true (string-contains? result "Returns a newly allocated list"))
    )
  )
