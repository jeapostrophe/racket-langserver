#lang racket/base

;; Typed Racket inlay hints.
;;
;; Typed Racket publishes an inferred type on the expression it belongs to,
;; never on the name bound to it: an atom carries its type over its whole span,
;; a compound expression carries it on its parens, and the shorthand
;; `(define (f x) ...)` carries the function type on the whole `define` form.
;; Each hint therefore pairs the expression owning the type with the name the
;; reader wants it next to. Bindings the source already annotates are skipped.
;;
;; Positions are absolute character offsets. The caller converts them and
;; applies the `#lang` gate.

(require racket/contract
         racket/class
         racket/list
         racket/match
         racket/set
         racket/string
         "../common/interfaces.rkt"
         "inlay-hint-source.rkt"
         (only-in "lexer/token-tree.rkt"
                  Token-Leaf?
                  Token-List?
                  Token-List-children
                  Token-Forest?
                  Token-Forest-nodes
                  token-node-children
                  token-node-start
                  token-node-end
                  non-skippable-node?
                  token-leaf-type?))

;; `label` is display text; `type-text` is the full type for the tooltip.
(struct/contract Inlay-Anchor
  ([pos exact-nonnegative-integer?]
   [label string?]
   [type-text string?])
  #:transparent)

(define *max-label-type-length* 48)

(define *let-form-heads*
  (set "let" "let*" "letrec" "let-values" "let*-values" "letrec-values"))

;; Typed Racket prints a multi-value type as "Value 1:\n  One\nValue 2:\n ...".
(define values-header-rx
  #px"(?m:^Value [0-9]+:[ \t]*\r?\n?)")

(define (split-values-types type-text)
  (cond
    [(regexp-match? values-header-rx type-text)
     (filter non-empty-string?
             (map string-trim (regexp-split values-header-rx type-text)))]
    [else (list type-text)]))

;; The leading space belongs to the label; asking for `paddingLeft` as well
;; renders two.
(define (type-label type-text)
  (define one-line (string-join (string-split type-text) " "))
  (string-append
    " : "
    (if (> (string-length one-line) *max-label-type-length*)
        (string-append (substring one-line 0 (sub1 *max-label-type-length*)) "…")
        one-line)))

;; `inferred-type-at` is the `typed-racket%` lookup: pos -> (values start end text).
(define/contract (typed-racket-inlay-anchors text forest inferred-type-at)
  (-> string?
      Token-Forest?
      (-> exact-nonnegative-integer? any)
      (listof Inlay-Anchor?))

  (define (node-text node)
    (substring text (token-node-start node) (token-node-end node)))

  (define (symbol-leaf? node)
    (and (Token-Leaf? node)
         (token-leaf-type? node 'symbol)))

  (define (symbol-leaf-text node)
    (and (symbol-leaf? node) (node-text node)))

  (define (meaningful nodes)
    (filter non-skippable-node? nodes))

  ;; The bound name of a function shorthand, `(define ((f a) b) ...)` included.
  (define (first-symbol-node nodes)
    (for/or ([node (in-list (meaningful nodes))])
      (if (symbol-leaf? node)
          node
          (first-symbol-node (token-node-children node)))))

  ;; A type belongs to `pos` only when its interval starts there; one merely
  ;; covering `pos` belongs to an enclosing expression.
  (define (type-at pos)
    (define-values (start _end type-text)
      (inferred-type-at pos))
    (and type-text (eqv? start pos) type-text))

  (define (anchor-for name-node type-text)
    (Inlay-Anchor (token-node-end name-node) (type-label type-text) type-text))

  (define (annotation? node)
    (equal? ":" (symbol-leaf-text node)))

  (define (annotation-head? nodes)
    (and (pair? nodes) (annotation? (first nodes))))

  ;; Names a sibling `(: name T)` form already annotates.
  (define (annotated-names siblings)
    (for*/set ([node (in-list siblings)]
               #:when (Token-List? node)
               [forms (in-value (meaningful (Token-List-children node)))]
               #:when (annotation-head? forms)
               [name (in-value (and (pair? (rest forms))
                                    (symbol-leaf-text (second forms))))]
               #:when name)
      name))

  (define (unannotated? name-node annotated)
    (not (set-member? annotated (node-text name-node))))

  ;; `binder` is a name, as in `(define x v)`, or a list of names, as in
  ;; `(define-values (a b) v)`.
  (define (binding-anchors binder value annotated)
    (define type-text (type-at (token-node-start value)))
    (cond
      [(not type-text) '()]
      [(symbol-leaf? binder)
       (if (unannotated? binder annotated)
           (list (anchor-for binder type-text))
           '())]
      [(Token-List? binder)
       (define names
         (filter symbol-leaf? (meaningful (Token-List-children binder))))
       (define types (split-values-types type-text))
       ;; Without one type per name there is no way to tell them apart.
       (cond
         [(and (pair? names) (= (length names) (length types)))
          (for/list ([name (in-list names)]
                     [value-type (in-list types)]
                     #:when (unannotated? name annotated))
            (anchor-for name value-type))]
         [else '()])]
      [else '()]))

  (define (define-anchors node args annotated)
    (match args
      ;; The function type sits on the whole form, so the hint goes after the
      ;; header, where a return type would be written.
      [(list (? Token-List? header) rest-forms ...)
       (define name-node (first-symbol-node (token-node-children header)))
       (define type-text (type-at (token-node-start node)))
       (if (and name-node
                type-text
                (not (annotation-head? rest-forms))
                (unannotated? name-node annotated))
           (list (anchor-for header type-text))
           '())]
      [(list (? symbol-leaf? binder) value _ ...)
       #:when (not (annotation? value))
       (binding-anchors binder value annotated)]
      [_ '()]))

  (define (let-anchors head args annotated)
    (define clause-list
      (match args
        ;; A named let keeps its clauses one form later.
        [(list (? symbol-leaf?) clauses _ ...)
         #:when (equal? head "let")
         clauses]
        [(list clauses _ ...) clauses]
        [_ #f]))
    (cond
      [(Token-List? clause-list)
       (append*
         (for/list ([clause (in-list (meaningful (Token-List-children clause-list)))])
           (match (and (Token-List? clause)
                       (meaningful (Token-List-children clause)))
             ;; A third form means `[x : T v]`, annotated already.
             [(list binder value) (binding-anchors binder value annotated)]
             [_ '()])))]
      [else '()]))

  (define (collect siblings)
    (define annotated (annotated-names siblings))
    (append*
      (for/list ([node (in-list siblings)]
                 #:when (Token-List? node))
        (list-anchors node annotated))))

  ;; Quoted data binds nothing, so prefix trees are not descended into.
  (define (list-anchors node annotated)
    (define forms (meaningful (Token-List-children node)))
    (define head (and (pair? forms) (symbol-leaf-text (first forms))))
    (define args (if (pair? forms) (rest forms) '()))
    (append
      (cond
        [(equal? head "define") (define-anchors node args annotated)]
        [(equal? head "define-values")
         (match args
           [(list binder value _ ...) (binding-anchors binder value annotated)]
           [_ '()])]
        [(and head (set-member? *let-form-heads* head))
         (let-anchors head args annotated)]
        [else '()])
      ;; Nested binding forms, including a `let` clause whose right-hand side
      ;; is itself one.
      (collect forms)))

  (collect (meaningful (Token-Forest-nodes forest))))

;; The hint source a language with a type checker publishes.
(define/contract (typed-racket-inlay-hints context req-start req-end)
  inlay-hint-source/c
  (define typed-racket-service
    (send (Inlay-Hint-Context-trace context) get-typed-racket))
  (define abs-pos->pos (Inlay-Hint-Context-abs-pos->pos context))
  (define anchors
    (typed-racket-inlay-anchors
      (Inlay-Hint-Context-text context)
      (Inlay-Hint-Context-forest context)
      (lambda (pos)
        (send typed-racket-service inferred-type-at pos))))
  (for/list ([anchor (in-list anchors)]
             #:when (<= req-start (Inlay-Anchor-pos anchor) req-end))
    (InlayHint #:position (abs-pos->pos (Inlay-Anchor-pos anchor))
               #:label (Inlay-Anchor-label anchor)
               #:kind InlayHintKind-Type
               #:tooltip (Inlay-Anchor-type-text anchor))))

(provide (struct-out Inlay-Anchor)
         typed-racket-inlay-anchors
         typed-racket-inlay-hints)
