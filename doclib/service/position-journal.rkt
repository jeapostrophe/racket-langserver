#lang racket/base

(require racket/match)

;; Edit log for positions stored inside service values.
;;
;; Interval-map keys update on every edit. Positions inside stored values update
;; only when something reads them. An old snapshot can stay valid without
;; rewriting every stored value on each keystroke.
;;
;; Hover details hold many absolute ranges. Rebuilding them on every edit was
;; too slow, so we use this log instead.
;;
;; Rule: each stored value belongs to one snapshot. Record edits only after that
;; snapshot is filled. `reset!` clears the log when the maps are cleared.

(provide make-position-journal
         position-journal-reset!
         position-journal-record-expand!
         position-journal-record-contract!
         position-journal-replay)

;; One edit to a span in the document. Newest edits are first in `entries`.
(struct Position-Edit (kind start end)
  #:transparent)

(struct Position-Journal ([entries #:mutable])
  #:transparent)

(define (make-position-journal)
  (Position-Journal '()))

(define (position-journal-reset! journal)
  (set-Position-Journal-entries! journal '()))

(define (position-journal-record-expand! journal start end)
  (record-edit! journal 'expand start end))

(define (position-journal-record-contract! journal start end)
  (record-edit! journal 'contract start end))

(define (position-journal-replay journal value expand-value contract-value)
  (replay-edits (Position-Journal-entries journal)
                value
                expand-value
                contract-value))

(define (record-edit! journal kind start end)
  (set-Position-Journal-entries!
    journal
    (cons (Position-Edit kind start end)
          (Position-Journal-entries journal))))

;; Replay edits from oldest to newest. Each step uses positions from just before
;; that edit. A step may return #f to drop the value. Later steps cannot bring
;; it back.
(define (replay-edits entries value expand-value contract-value)
  (cond
    [(null? entries) value]
    [else
     (define earlier-value
       (replay-edits (cdr entries)
                     value
                     expand-value
                     contract-value))
     (and earlier-value
          (match (car entries)
            [(Position-Edit 'expand start end)
             (expand-value earlier-value start end)]
            [(Position-Edit 'contract start end)
             (contract-value earlier-value start end)]))]))
