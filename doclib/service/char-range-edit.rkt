#lang racket/base

;; How a position stored in a service moves when the document is edited.
;;
;; `expand` is an insert of (- end start) characters at `start`; `contract` is
;; a delete of the text in [start, end). A service that keeps snapshot
;; positions must move them the way the interval maps in other services move,
;; or a stored position and a queried one drift apart.

(require "../../common/interfaces.rkt")

(provide expand-position
         contract-position
         expand-char-range
         contract-char-range)

;; Text inserted at a position goes before it, so the position moves.
(define (expand-position pos start end)
  (if (>= pos start)
      (+ pos (- end start))
      pos))

;; A position inside deleted text collapses to the deletion start.
(define (contract-position pos start end)
  (define decrease (- end start))
  (cond
    [(<= pos start) pos]
    [(>= pos end) (- pos decrease)]
    [else start]))

;; An insert extends a range only when it starts strictly inside that range,
;; matching #:interior 'extend.
(define (expand-char-range range start end)
  (define increase (- end start))
  (define range-start (CharRange-start range))
  (define range-end (CharRange-end range))
  (CharRange (if (>= range-start start)
                 (+ range-start increase)
                 range-start)
             (if (> range-end start)
                 (+ range-end increase)
                 range-end)))

(define (contract-char-range range start end)
  (define range-start (contract-position (CharRange-start range) start end))
  (define range-end (contract-position (CharRange-end range) start end))
  (and (< range-start range-end)
       (CharRange range-start range-end)))
