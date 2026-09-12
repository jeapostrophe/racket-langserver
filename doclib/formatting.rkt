#lang racket/base

(require racket/contract
         "formatter/fixw.rkt"
         "../common/interfaces.rkt")

(provide formatting)

(define/contract (formatting text start-ln end-ln
                             #:formatting-options options
                             #:backend [backend 'fixw]
                             #:src-dir [src-dir #f]
                             #:interactive? [interactive? #f])
  (->* (string?
        exact-nonnegative-integer?
        exact-nonnegative-integer?
        #:formatting-options FormattingOptions?)
       (#:backend symbol?
        #:src-dir (or/c path? #f)
        #:interactive? boolean?)
       (listof TextEdit?))
  (case backend
    [(fixw)
     (fixw-format-edits text
                        start-ln
                        end-ln
                        #:formatting-options options
                        #:src-dir src-dir
                        #:interactive? interactive?)]
    [else
     (raise-arguments-error 'formatting
                            "formatter backend is not available"
                            "backend" backend)]))
