#lang racket/base
(require racket/class
         racket/dict
         racket/set
         racket/contract/base
         drracket/check-syntax
         data/interval-map
         net/url
         "interfaces.rkt"
         "responses.rkt"
         "path-util.rkt"
         "docs-helpers.rkt")

(struct Decl (require? id left right) #:transparent)

(define build-trace%
  (class (annotations-mixin object%)
    (init-field src doc-text indenter)
    (define warn-diags (mutable-seteq))
    (define hovers (make-interval-map))
    (define docs (make-interval-map))
    (define completions (list))
    (define requires (make-interval-map))
    (define definitions (make-hash))
    (define quickfixs (make-interval-map))
    ;; decl -> (set pos ...)
    (define sym-decls (make-interval-map))
    ;; pos -> decl
    (define sym-bindings (make-interval-map))
    (define/public (reset)
      (set-clear! warn-diags)
      (set! hovers (make-interval-map))
      (set! docs (make-interval-map))
      (set! sym-decls (make-interval-map))
      (set! sym-bindings (make-interval-map))
      (set! requires (make-interval-map))
      (set! definitions (make-hash))
      (set! quickfixs (make-interval-map)))
    (define/public (expand start end)
      (define inc (- end start))
      (move-interior-intervals sym-decls (- start 1) inc)
      (move-interior-intervals sym-bindings (- start 1) inc)
      (map (lambda (int-map) (interval-map-expand! int-map start end)) (list hovers docs sym-decls sym-bindings)))
    (define/public (contract start end)
      (define dec (- start end))
      (move-interior-intervals sym-decls end dec)
      (move-interior-intervals sym-bindings end dec)
      (map (lambda (int-map) (interval-map-contract! int-map start end)) (list hovers docs sym-decls sym-bindings)))
    ;; some intervals are held inside of the interval maps... so we need to expand/contract these manually
    (define/private (move-interior-intervals int-map after amt)
      (dict-for-each int-map
                     (lambda (range decl-set)
                       (define result (cond
                                        [(Decl? decl-set)
                                         (define d-range (cons (Decl-left decl-set) (Decl-right decl-set)))
                                         (if (> (car d-range) after)
                                             (Decl (Decl-require? decl-set) #f (+ (car d-range) amt) (+ (cdr d-range) amt))
                                             #f)]
                                        [else
                                         (list->set (set-map decl-set (lambda (d-range)
                                                                        (if (> (car d-range) after)
                                                                            (cons (+ (car d-range) amt) (+ (cdr d-range) amt))
                                                                            d-range))))]))
                       (when result
                         (interval-map-set! int-map (car range) (cdr range) result)))))
    ;; Getters
    (define/public (get-indenter) indenter)
    (define/public (get-warn-diags) warn-diags)
    (define/public (get-hovers) hovers)
    (define/public (get-docs) docs)
    (define/public (get-completions) completions)
    (define/public (set-completions new-completions) (set! completions new-completions))
    (define/public (get-requires) requires)
    (define/public (get-sym-decls) sym-decls)
    (define/public (get-sym-bindings) sym-bindings)
    (define/public (get-definitions) definitions)
    (define/public (get-quickfixs) quickfixs)
    ;; Overrides
    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))
    ;; Definitions
    (define/override (syncheck:add-definition-target _src-obj start end id _mods)
      (hash-set! definitions id (Decl src id start end)))
    ;; Track requires
    (define/override (syncheck:add-require-open-menu _text start finish file)
      (interval-map-set! requires start finish file))
    ;; Mouse-over status
    (define (hint-unused-variable src-obj start finish)
      (unless (string=? "_" (send doc-text get-text start (add1 start)))
        (define diag (Diagnostic #:range (Range #:start (abs-pos->Pos doc-text start)
                                                #:end   (abs-pos->Pos doc-text finish))
                                 #:severity Diag-Information
                                 #:source (path->uri src-obj)
                                 #:message "unused variable"))

        (interval-map-set!
         quickfixs start (add1 finish)
         (CodeAction
          #:title "Add prefix `_` to ignore"
          #:kind "quickfix"
          #:diagnostics (list diag)
          #:isPreferred #f
          #:edit (WorkspaceEdit
                  #:changes
                  (hasheq (string->symbol (path->uri src-obj))
                          (TextEdit #:range (Range #:start (abs-pos->Pos doc-text start)
                                                   #:end   (abs-pos->Pos doc-text finish))
                                    #:newText "_")))
          #:data (Range #:start (abs-pos->Pos doc-text start)
                        #:end   (abs-pos->Pos doc-text finish))))

        (set-add! warn-diags diag)))
    (define/override (syncheck:add-mouse-over-status src-obj start finish text)
      ;; Infer a length of 1 for zero-length ranges in the document.
      ;; XXX This might not exactly match the behavior in DrRacket.
      (when (= start finish)
        (set! finish (add1 finish)))
      (when (string=? "no bound occurrences" text)
        (hint-unused-variable src-obj start finish))
      (interval-map-set! hovers start finish text))
    ;; Docs
    (define/override (syncheck:add-docs-menu _text start finish _id _label path def-tag url-tag)
      (when url
        (when (= start finish)
          (set! finish (add1 finish)))
        (define path-url (path->url path))
        (define link+tag (cond
                           [url-tag (struct-copy url path-url [fragment url-tag])]
                           [def-tag (struct-copy url path-url [fragment (def-tag->html-anchor-tag def-tag)])]
                           [else path-url]))
        (interval-map-set! docs start finish (list (url->string link+tag) def-tag))))
    (define/override (syncheck:add-jump-to-definition _src-obj start end id filename _submods)
      (define decl (Decl filename id 0 0))
      (interval-map-set! sym-bindings start (add1 end) decl))
    ;; References
    (define/override (syncheck:add-arrow/name-dup _start-src-obj start-left start-right
                                                  _end-src-obj end-left end-right
                                                  _actual? _phase-level
                                                  require-arrow? _name-dup?)
      (when (= start-left start-right)
        (set! start-right (add1 start-right)))
      (when (= end-left end-right)
        (set! end-right (add1 end-right)))
      ;; Mapping from doc declaration to set of bindings.
      (define prev-bindings (interval-map-ref sym-decls start-left set))
      (define new-bindings (set-add prev-bindings (cons end-left end-right)))
      (interval-map-set! sym-decls start-left start-right new-bindings)
      ;; Mapping from binding to declaration.
      (unless require-arrow?
        (define new-decl (Decl #f #f start-left start-right))
        (interval-map-set! sym-bindings end-left end-right new-decl)))
    ;; Unused requires
    (define/override (syncheck:add-unused-require _src left right)
      (define diag (Diagnostic #:range (Range #:start (abs-pos->Pos doc-text left)
                                              #:end   (abs-pos->Pos doc-text right))
                               #:severity Diag-Information
                               #:source "Racket"
                               #:message "unused require"))
      (set-add! warn-diags diag))
    (super-new)))

(provide build-trace%
         (contract-out
          [struct Decl ([require? any/c]
                        [id any/c]
                        [left exact-nonnegative-integer?]
                        [right exact-nonnegative-integer?])]))
