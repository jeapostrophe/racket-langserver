#lang racket/base

(require "interface.rkt"
         racket/class
         racket/set
         data/interval-map
         "../interfaces.rkt"
         "../responses.rkt"
         "../path-util.rkt"
         "../settings.rkt"
         drracket/check-syntax
         "resyntax.rkt")

(provide diag%)

(define diag%
  (class base-service%
    (init-field src doc-text)
    (super-new)

    (define warn-diags (mutable-seteq))
    (define quickfixs (make-interval-map))

    (define/override (get)
      (list warn-diags quickfixs))

    (define/override (reset)
      (set-clear! warn-diags)
      (set! quickfixs (make-interval-map)))

    (define/override (walk-text text)
      (when (get-resyntax-enabled)
        (resyntax text doc-text src warn-diags quickfixs)))

    (define/override (syncheck:add-mouse-over-status src-obj start finish text)
      (when (string=? "no bound occurrences" text)
        (hint-unused-variable src-obj start finish)))

    ;; Mouse-over status
    (define (hint-unused-variable src-obj start finish)
      (unless (string=? "_" (send doc-text get-text start (add1 start)))
        (define diag
          (Diagnostic #:range (Range #:start (abs-pos->Pos doc-text start)
                                     #:end (abs-pos->Pos doc-text finish))
                      #:severity Diag-Information
                      #:source (path->uri src-obj)
                      #:message "unused variable"))

        (define code-action
          (CodeAction
            #:title "Add prefix `_` to ignore"
            #:kind "quickfix"
            #:diagnostics (list diag)
            #:isPreferred #f
            #:edit (WorkspaceEdit
                     #:changes
                     (hasheq (string->symbol (path->uri src-obj))
                             (list (TextEdit #:range (Range #:start (abs-pos->Pos doc-text start)
                                                            #:end (abs-pos->Pos doc-text start))
                                             #:newText "_"))))))

        (interval-map-set! quickfixs start (add1 finish) code-action)
        (set-add! warn-diags diag)))

    (define/override (syncheck:add-unused-require _src left right)
      (define diag (Diagnostic #:range (Range #:start (abs-pos->Pos doc-text left)
                                              #:end (abs-pos->Pos doc-text right))
                               #:severity Diag-Information
                               #:source "Racket"
                               #:message "unused require"))
      (set-add! warn-diags diag))))
