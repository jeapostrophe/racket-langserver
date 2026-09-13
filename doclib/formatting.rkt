#lang racket/base

(require racket/class
         racket/contract
         "editor.rkt"
         "formatter/drracket.rkt"
         "formatter/fixw.rkt"
         "formatter/fmt.rkt"
         "lexer/snapshot.rkt"
         "../common/interfaces.rkt")

(provide formatting
         exn:fail:fmt-unavailable?)

(define/contract (formatting text start-ln end-ln
                             #:formatting-options options
                             #:backend [backend 'fixw]
                             #:editor [editor #f]
                             #:racket-fallback? [racket-fallback? #f]
                             #:lexer-snapshot [lexer-snapshot #f]
                             #:src-dir [src-dir #f]
                             #:interactive? [interactive? #f])
  (->* (string?
         exact-nonnegative-integer?
         exact-nonnegative-integer?
         #:formatting-options FormattingOptions?)
       (#:backend symbol?
        #:editor (or/c (is-a?/c lsp-editor%) #f)
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
    [(fmt)
     (define formatted (fmt-format-document text options))
     (if formatted
         (list (TextEdit #:range (Range (Pos 0 0)
                                        (abs-pos->Pos editor (send editor end-pos)))
                         #:newText formatted))
         '())]
    [else
     (raise-arguments-error 'formatting
                            "formatter backend is not available"
                            "backend" backend)]))
