#lang racket/base

(require "interface.rkt"
         racket/class
         racket/set
         data/interval-map
         "../interfaces.rkt"
         "../responses.rkt"
         "../path-util.rkt"
         "../settings.rkt"
         drracket/check-syntax)

(require resyntax
         resyntax/default-recommendations
         resyntax/private/source
         resyntax/private/refactoring-result
         resyntax/private/string-replacement
         rebellion/base/range
         rebellion/base/comparator
         rebellion/collection/range-set)

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
        (resyntax text)))

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
      (set-add! warn-diags diag))

    (define (resyntax text)
      (define text-source (string-source text))
      (define all-lines (range-set (unbounded-range #:comparator natural<=>)))
      (define result-set
        (resyntax-analyze
          text-source
          #:suite default-recommendations
          #:lines all-lines))

      (define (add result)
        (define sr (refactoring-result-string-replacement result))
        (define char-start (string-replacement-start sr))
        (define char-end (string-replacement-original-end sr))
        (define message (refactoring-result-message result))
        (define range (Range #:start (abs-pos->Pos doc-text char-start)
                             #:end (abs-pos->Pos doc-text char-end)))
        (define new-text (string-replacement-render sr (source->string text-source)))

        (define rule-name (refactoring-result-rule-name result))
        (define diag
          (Diagnostic #:range range
                      #:severity Diag-Information
                      #:source "Resyntax"
                      #:message (format "[~a] ~a" rule-name message)))
        (define code-action
          (CodeAction
            #:title (format "Apply rule [~a]" rule-name)
            #:kind "quickfix"
            #:diagnostics (list diag)
            #:isPreferred #f
            #:edit (WorkspaceEdit
                     #:changes
                     (hasheq (string->symbol (path->uri src))
                             (list (TextEdit #:range range
                                             #:newText new-text))))))
        (set-add! warn-diags diag)
        (interval-map-set! quickfixs char-start char-end code-action))

      (for-each add (refactoring-result-set-results result-set)))))

