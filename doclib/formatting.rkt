#lang racket/base

(require "external/fixw.rkt"
         "../common/interfaces.rkt"
         racket/port)

(provide formatting)

(define (formatting text start-ln end-ln
                    #:src-dir [src-dir #f]
                    #:interactive? [interactive? #f])
  (define original-lines (port->lines (open-input-string text)))
  (define formatted-lines
    (get-formatted-lines text src-dir #:interactive? interactive?))
  (for/list ([original-line (in-list original-lines)]
             [formatted-line (in-list formatted-lines)]
             [ln (in-naturals)]
             #:when (and (<= start-ln ln end-ln)
                         (not (string=? original-line formatted-line))))
    (TextEdit #:range (Range (Pos ln 0)
                             (Pos ln (string-length original-line)))
              #:newText formatted-line)))

