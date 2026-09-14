#lang racket/base

(require racket/class
         racket/contract
         "editor.rkt"
         "formatter/drracket.rkt"
         "formatter/fixw.rkt"
         "formatter/fmt.rkt"
         (only-in "lexer.rkt"
                  LexerState?
                  LexerState-language-policy
                  LexerState-snapshot)
         (only-in "doc-lang.rkt"
                  Language-Policy-body-mode
                  sexp-format-language?)
         "../common/interfaces.rkt")

(provide formatting
         exn:fail:fmt?
         exn:fail:fmt-unavailable?)

(define (selected-backend backend policy text)
  (if (and (memq backend '(fixw fmt))
           (not (sexp-format-language? text policy)))
      'drracket
      backend))

(define/contract (formatting doc-text start-ln end-ln
                             #:formatting-options options
                             #:backend [backend 'fixw]
                             #:lexer-state [lexer-state #f]
                             #:src-dir [src-dir #f]
                             #:interactive? [interactive? #f])
  (->* ((is-a?/c lsp-editor%)
        exact-nonnegative-integer?
        exact-nonnegative-integer?
        #:formatting-options FormattingOptions?)
       (#:backend symbol?
        #:lexer-state (or/c LexerState? #f)
        #:src-dir (or/c path? #f)
        #:interactive? boolean?)
       (listof TextEdit?))
  ;; fixw and fmt format only recognized s-expression languages. Other
  ;; languages use DrRacket even when fixw or fmt is selected.
  (define text (send doc-text get-text))
  (define policy
    (and lexer-state (LexerState-language-policy lexer-state)))
  (case (selected-backend backend policy text)
    [(fixw)
     (fixw-format-edits text
                        start-ln
                        end-ln
                        #:formatting-options options
                        #:src-dir src-dir
                        #:interactive? interactive?)]
    [(drracket)
     (define racket-fallback?
       (and policy (eq? 'sexp (Language-Policy-body-mode policy))))
     (drracket-format-edits text
                            start-ln
                            end-ln
                            #:formatting-options options
                            #:racket-fallback? racket-fallback?
                            #:lexer-snapshot (and racket-fallback?
                                                  (LexerState-snapshot lexer-state))
                            #:src-dir src-dir
                            #:interactive? interactive?)]
    [(fmt)
     (define formatted (fmt-format-document text options))
     (if formatted
         (list (TextEdit #:range (Range (Pos 0 0)
                                        (abs-pos->Pos doc-text (send doc-text end-pos)))
                         #:newText formatted))
         '())]
    [else
     (raise-arguments-error 'formatting
                            "formatter backend is not available"
                            "backend" backend)]))
