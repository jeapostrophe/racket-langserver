#lang racket/base

(provide resyntax)

(require racket/set
         data/interval-map
         "../interfaces.rkt"
         "../path-util.rkt"
         "dynamic-import.rkt")

(define has-resyntax? #t)

(define (disable-resyntax!)
  (set! has-resyntax? #f))

(dynamic-imports ('resyntax/private/source
                   string-source
                   source->string)
                 ('rebellion/base/range
                   unbounded-range)
                 ('rebellion/base/comparator
                   natural<=>)
                 ('rebellion/collection/range-set
                   range-set)
                 ('resyntax/private/string-replacement
                   string-replacement-start
                   string-replacement-original-end
                   string-replacement-render)
                 ('resyntax/private/refactoring-result
                   refactoring-result-string-replacement
                   refactoring-result-message
                   refactoring-result-rule-name
                   refactoring-result-set-results)
                 ('resyntax/default-recommendations
                   default-recommendations)
                 ('resyntax
                   resyntax-analyze)
                 disable-resyntax!)


(define (resyntax text doc-text src warn-diags quickfixs)
  (when has-resyntax?
    (resyntax-impl text doc-text src warn-diags quickfixs)))

(define (resyntax-impl text doc-text src warn-diags quickfixs)
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
                  #:severity DiagnosticSeverity-Information
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

  (for-each add (refactoring-result-set-results result-set)))

