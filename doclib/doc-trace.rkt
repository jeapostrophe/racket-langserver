#lang racket/base

(require racket/class
         racket/dict
         racket/match
         racket/set
         racket/string
         drracket/check-syntax
         "check-syntax-compat.rkt"
         "service/completion.rkt"
         "service/hover/service.rkt"
         "service/docs.rkt"
         "service/require.rkt"
         "service/diagnostic.rkt"
         "service/declaration.rkt"
         "service/highlight.rkt"
         "service/tooltip-log.rkt"
         "service/typed-racket/service.rkt"
         "../common/interfaces.rkt"
         "../common/path-util.rkt"
         "internal-types.rkt")

(define build-trace%
  (class (check-syntax-annotations-mixin object%)
    (init-field src
                doc-text
                lexer-state)
    (define docs (new docs%))
    (define completions (new completion%))
    (define requires (new require%))
    (define diag (new diag%
                   [src src]
                   [doc-text doc-text]
                   [lexer-state lexer-state]))
    (define decls
      (new declaration%
        [src src]
        [doc-text doc-text]))
    (define hovers
      (new hover%
        [src src]
        [doc-text doc-text]
        [lexer-state lexer-state]))
    (define semantic-tokens (new highlight% [src src] [doc-text doc-text]))
    (define typed-racket
      (new typed-racket%
        [src src]
        [doc-text doc-text]))

    (define services
      (list hovers
            docs
            completions
            requires
            diag
            typed-racket
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

    (define/public (walk-stx expand-result)
      (for ([s services])
        (send s walk-stx expand-result)))

    (define/public (walk-log text)
      (for ([s services])
        (send s walk-log text))
      (for ([log (in-list text)]
            #:when (online-tooltip-log? log))
        (define tooltips
          (online-tooltip-log-tooltips log src))
        (case (tooltip-log-kind (online-tooltip-log-message log))
          [(typed-racket-inferred-type)
           (send typed-racket consume-inferred-tooltips tooltips)]
          [(typed-racket-type-error)
           (send typed-racket consume-type-error-tooltips tooltips)]
          [(mouse-over)
           (for ([tooltip (in-list tooltips)])
             (send this
                   add-log-tooltip
                   (Tooltip-source tooltip)
                   (Tooltip-start tooltip)
                   (Tooltip-end tooltip)
                   (Tooltip-text tooltip)))])))

    ;; Named reads for services. Do not add getters that expose interval-maps.
    (define/public (get-hover) hovers)
    (define/public (get-declaration) decls)
    (define/public (get-typed-racket) typed-racket)

    ;; Derive accepted cross-file state from this trace's frozen editor.
    (define/public (get-contribution)
      (define (abs->pos pos)
        (match-define (list line char)
          (send doc-text pos->line/char pos))
        (Pos line char))

      (define (range->location range)
        (define start (CharRange-start range))
        (define end
          (if (= start (CharRange-end range))
              (add1 start)
              (CharRange-end range)))
        (Location (path->uri src)
                  (Range (abs->pos start) (abs->pos end))))

      (define references
        (for/fold ([references (hash)])
                  ([entry (in-list (send decls module-binding-uses))])
          (define range (car entry))
          (define module-binding (cdr entry))
          (hash-update references
                       module-binding
                       (lambda (locations) (cons (range->location range) locations))
                       '())))
      (define definitions
        (for/hash ([entry (in-list (send decls module-binding-definitions))])
          (values (cdr entry) (range->location (car entry)))))
      (Doc-Contribution src references definitions))

    ;; Chosen over putting Typed Racket type-error diagnostics on diag%:
    ;; inferred types and type errors share one online-check-syntax channel
    ;; owned by typed-racket%. Union both sets here for LSP publish.
    ;;
    ;; Drop a diag% Type Checker entry when some Typed Racket tooltip message
    ;; is a substring of it (exception text is usually a longer wrapper).
    ;; Prefer the tooltip diagnostic. If tooltips produced nothing, keep the
    ;; exception so type errors do not disappear.
    (define/public (get-warn-diags)
      ;; Callers expect a mutable set. Fresh copy so they do not share the
      ;; diag% or typed-racket% stores.
      (define typed-racket-diags (send typed-racket get-diagnostics))
      (define diagnostics (mutable-seteq))
      (for ([diag (in-set (car (send diag get)))])
        (define message (Diagnostic-message diag))
        (define covered-by-typed-racket?
          (and (string-contains? message "Type Checker:")
               (for/or ([typed-diag (in-set typed-racket-diags)])
                 (string-contains? message (Diagnostic-message typed-diag)))))
        (unless covered-by-typed-racket?
          (set-add! diagnostics diag)))
      (set-union! diagnostics typed-racket-diags)
      diagnostics)
    (define/public (get-docs) (send docs get))
    (define/public (get-completions) (send completions get))
    (define/public (get-online-completions str-before-cursor)
      (send completions get-online-completions str-before-cursor))
    (define/public (get-requires) (send requires get))
    (define/public (get-quickfixs) (cadr (send diag get)))
    (define/public (get-semantic-tokens) (send semantic-tokens get))

    ;; Overrides
    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))

    ;; Definitions
    (define/override (syncheck:add-definition-target/phase-level+space
                       src-obj start end id mods phase+space)
      (for ([s services])
        (send s syncheck:add-definition-target/phase-level+space
              src-obj start end id mods phase+space)))

    (define/override (syncheck:unused-binder src-obj left right)
      (for ([s services])
        (send s syncheck:unused-binder src-obj left right)))

    ;; Track requires
    (define/override (syncheck:add-require-open-menu text start finish file)
      (for ([s services])
        (send s syncheck:add-require-open-menu text start finish file)))

    (define/override (syncheck:add-mouse-over-status src-obj start finish text)
      (for ([s services])
        (send s syncheck:add-mouse-over-status src-obj start finish text)))

    (define/public (add-log-tooltip src-obj start finish text)
      (for ([s services])
        (send s add-log-tooltip src-obj start finish text)))

    ;; Docs
    (define/override (syncheck:add-docs-menu text start finish id label path def-tag url-tag)
      (for ([s services])
        (send s syncheck:add-docs-menu text start finish id label path def-tag url-tag)))

    (define/override (syncheck:add-jump-to-definition/phase-level+space
                       src-obj start end id filename submods phase+space)
      (for ([s services])
        (send s syncheck:add-jump-to-definition/phase-level+space
              src-obj start end id filename submods phase+space)))

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
