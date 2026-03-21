#lang racket/base

(require "external/fixw.rkt"
         "../common/interfaces.rkt"
         racket/port)

(provide formatting)

(define (formatting text start-ln end-ln #:interactive? [interactive? #f])
  (define lines (port->lines (open-input-string text)))
  (define formatted-lines (get-formatted-lines text #:interactive? interactive?))
  (define diffs (diff-lines lines formatted-lines))
  (for/list ([diff (in-list diffs)]
             [ln (in-naturals)]
             [line (in-list lines)]
             #:when (and diff (<= start-ln ln end-ln)))
    (TextEdit #:range (Range (Pos ln 0)
                             (Pos ln (string-length line)))
              #:newText diff)))

(define (diff-lines olds news)
  (for/list ([old (in-list olds)]
             [new (in-list news)])
    (if (string=? old new)
        #f
        new)))

