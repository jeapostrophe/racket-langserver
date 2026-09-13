#lang racket/base

(require racket/contract
         "formatter/drracket.rkt"
         "formatter/fixw.rkt"
         "lexer/snapshot.rkt"
         "../common/interfaces.rkt")

(provide formatting)

(define/contract (formatting text start-ln end-ln
                             #:formatting-options options
                             #:backend [backend 'fixw]
                             #:racket-fallback? [racket-fallback? #f]
                             #:lexer-snapshot [lexer-snapshot #f]
                             #:src-dir [src-dir #f]
                             #:interactive? [interactive? #f])
  (->* (string?
         exact-nonnegative-integer?
         exact-nonnegative-integer?
         #:formatting-options FormattingOptions?)
       (#:backend symbol?
        #:racket-fallback? boolean?
        #:lexer-snapshot (or/c LexerSnapshot? #f)
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
    [(drracket)
     (drracket-format-edits text
                            start-ln
                            end-ln
                            #:formatting-options options
                            #:racket-fallback? racket-fallback?
                            #:lexer-snapshot lexer-snapshot
                            #:src-dir src-dir
                            #:interactive? interactive?)]
    [else
     (raise-arguments-error 'formatting
                            "formatter backend is not available"
                            "backend" backend)]))
