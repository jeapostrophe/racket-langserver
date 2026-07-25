#lang racket/base

;; If a transform returns #f once, later expands keep it #f.

(require rackunit
         "../../doclib/service/position-journal.rkt")

(define (expand-position value start end)
  (if (>= value start)
      (+ value (- end start))
      value))

(define (contract-position value start end)
  (cond
    [(<= value start) value]
    [(>= value end) (- value (- end start))]
    [else #f]))

(module+ test
  (test-case
    "position journal preserves a value without later edits"
    (define journal (make-position-journal))
    (check-equal?
      (position-journal-replay journal
                               6
                               expand-position
                               contract-position)
      6))

  (test-case
    "position journal replays edits in order"
    (define journal (make-position-journal))
    (position-journal-record-expand! journal 3 5)
    (position-journal-record-contract! journal 4 5)
    (check-equal?
      (position-journal-replay journal
                               6
                               expand-position
                               contract-position)
      7))

  (test-case
    "position journal preserves an invalidated value"
    (define journal (make-position-journal))
    (position-journal-record-contract! journal 4 6)
    (position-journal-record-expand! journal 2 4)
    (check-false
      (position-journal-replay journal
                               5
                               expand-position
                               contract-position))))
