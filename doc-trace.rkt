#lang racket/base

(require racket/class
         drracket/check-syntax
         "service/completion.rkt"
         "service/hover.rkt"
         "service/docs.rkt"
         "service/require.rkt"
         "service/definition.rkt"
         "service/diagnostic.rkt"
         "service/declaration.rkt"
         "service/highlight.rkt")

(define build-trace%
  (class (annotations-mixin object%)
    (init-field src doc-text indenter)
    (define hovers (new hover%))
    (define docs (new docs%))
    (define completions (new completion%))
    (define requires (new require%))
    (define definitions (new definition% [src src]))
    (define diag (new diag% [doc-text doc-text]))
    (define decls (new declaration%))
    (define semantic-tokens (new highlight% [src src] [doc-text doc-text]))

    (define services
      (list hovers
            docs
            completions
            requires
            definitions
            diag
            decls
            semantic-tokens))

    (define/public (reset)
      (for ([s services])
        (send s reset)))

    (define/public (expand start end)
      (for ([s services])
        (send s expand start end)))

    (define/public (contract start end)
      (for ([s services])
        (send s contract start end)))

    (define/public (walk-stx stx expanded-stx)
      (for ([s services])
        (send s walk-stx stx expanded-stx)))

    ;; Getters
    (define/public (get-indenter) indenter)
    (define/public (get-warn-diags) (car (send diag get)))
    (define/public (get-hovers) (send hovers get))
    (define/public (get-docs) (send docs get))
    (define/public (get-completions) (send completions get))
    (define/public (get-requires) (send requires get))
    (define/public (get-sym-decls) (car (send decls get)))
    (define/public (get-sym-bindings) (cadr (send decls get)))
    (define/public (get-definitions) (send definitions get))
    (define/public (get-quickfixs) (cadr (send diag get)))
    (define/public (get-semantic-tokens) (send semantic-tokens get))

    ;; Overrides
    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))

    ;; Definitions
    (define/override (syncheck:add-definition-target src-obj start end id mods)
      (for ([s services])
        (send s syncheck:add-definition-target src-obj start end id mods)))

    ;; Track requires
    (define/override (syncheck:add-require-open-menu text start finish file)
      (for ([s services])
        (send s syncheck:add-require-open-menu text start finish file)))

    (define/override (syncheck:add-mouse-over-status src-obj start finish text)
      (for ([s services])
        (send s syncheck:add-mouse-over-status src-obj start finish text)))

    ;; Docs
    (define/override (syncheck:add-docs-menu text start finish id label path def-tag url-tag)
      (for ([s services])
        (send s syncheck:add-docs-menu text start finish id label path def-tag url-tag)))

    (define/override (syncheck:add-jump-to-definition src-obj start end id filename submods)
      (for ([s services])
        (send s syncheck:add-jump-to-definition src-obj start end id filename submods)))

    ;; References
    (define/override (syncheck:add-arrow/name-dup _start-src-obj start-left start-right
                                                  _end-src-obj end-left end-right
                                                  _actual? _phase-level
                                                  require-arrow? _name-dup?)
      (for ([s services])
        (send s syncheck:add-arrow/name-dup
              _start-src-obj start-left start-right
              _end-src-obj end-left end-right
              _actual? _phase-level
              require-arrow? _name-dup?)))

    ;; Unused requires
    (define/override (syncheck:add-unused-require src left right)
      (for ([s services])
        (send s syncheck:add-unused-require src left right)))

    (define/override (syncheck:color-range src start end style)
      (for ([s services])
        (send s syncheck:color-range src start end style)))

    (super-new)))

(provide build-trace%)

