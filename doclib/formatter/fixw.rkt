#lang racket/base

(require racket/contract
         racket/port
         "../external/fixw.rkt"
         "../../common/interfaces.rkt")

(provide fixw-format-edits)

(define/contract (fixw-format-edits text start-ln end-ln
                                    #:formatting-options _options
                                    #:src-dir [src-dir #f]
                                    #:interactive? [interactive? #f])
  (->* (string?
         exact-nonnegative-integer?
         exact-nonnegative-integer?
         #:formatting-options FormattingOptions?)
       (#:src-dir (or/c path? #f)
        #:interactive? boolean?)
       (listof TextEdit?))
  ;; fixw gets its indentation policy from its own configuration. LSP formatting
  ;; options are intentionally accepted at the backend boundary and ignored.
  (define original-lines (port->lines (open-input-string text)))
  (define formatted-lines
    (get-formatted-lines text src-dir #:interactive? interactive?))
  (for/list ([original-line (in-list original-lines)]
             [formatted-line (in-list formatted-lines)]
             [ln (in-naturals)]
             #:break (> ln end-ln)
             #:when (and (<= start-ln ln end-ln)
                         (not (string=? original-line formatted-line))))
    (TextEdit #:range (Range (Pos ln 0)
                             (Pos ln (string-length original-line)))
              #:newText formatted-line)))
