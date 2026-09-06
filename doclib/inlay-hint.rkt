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
;; A hint is read from the name it sits on and from the expression whose type
;; it shows, so editing either drops it. A function's type is read from its
;; header alone: editing the body leaves the type shown until the next
;; analysis.

(require racket/contract
         racket/list
         racket/set
         racket/string
         syntax/parse
         "../common/interfaces.rkt"
         "inlay-hint-source.rkt"
         "syntax-query.rkt")

(define *max-label-type-length* 48)

(define-syntax-class code-list
  #:attributes ((forms 1))

  (pattern (forms ...)))

(define-syntax-class type-annotation
  #:attributes (name)
  #:datum-literals (:)

  (pattern (: name:id _ ...)))

(define-syntax-class let-form
  #:attributes ((clauses 1))
  #:datum-literals (let let* letrec let-values let*-values letrec-values)

  ;; A named let keeps its clauses one form later.
  (pattern (let _:id (clauses ...) _ ...))

  (pattern ((~or let let* letrec let-values let*-values letrec-values)
            (clauses ...)
            _ ...)))

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

;; The spans a hint is read from, or #f when the reader gave one of them no
;; position: without a span there is nothing to watch for edits.
(define (source-ranges . stxs)
  (define ranges (map syntax-char-range stxs))
  (and (andmap values ranges) ranges))

;; The hint source a language with a type checker publishes.
(define/contract (typed-racket-inlay-hints context stx)
  inlay-hint-source/c
  (define inferred-type-at (Inlay-Hint-Context-inferred-type-at context))

  ;; A type belongs to `pos` only when its interval starts there; one merely
  ;; covering `pos` belongs to an enclosing expression.
  (define (type-at pos)
    (define-values (start _end type-text)
      (inferred-type-at pos))
    (and type-text (eqv? start pos) type-text))

  (define (type-at-syntax owner)
    (define start (syntax-start owner))
    (and start (type-at start)))

  ;; The bound name of a function shorthand, `(define ((f a) b) ...)` included.
  (define (first-name forms)
    (for/or ([form (in-list forms)])
      (syntax-parse form
        [name:id #'name]
        [nested:code-list (first-name (attribute nested.forms))]
        [_ #f])))

  ;; Names a sibling `(: name T)` form already annotates.
  (define (annotated-names siblings)
    (for*/set ([sibling (in-list siblings)]
               [name (in-value (syntax-parse sibling
                                 [annotation:type-annotation (syntax-e #'annotation.name)]
                                 [_ #f]))]
               #:when name)
      name))

  (define (unannotated? name annotated)
    (not (set-member? annotated (syntax-e name))))

  (define (type-anchor name-stx type-text)
    (Inlay-Hint-Anchor (syntax-end name-stx)
                       InlayHintKind-Type
                       (type-label type-text)
                       type-text))

  (define (type-group sources anchors)
    (if (and sources (pair? anchors))
        (list (Inlay-Hint-Group sources anchors))
        '()))

  ;; `binder` is a name, as in `(define x v)`, or a list of names, as in
  ;; `(define-values (a b) v)`.
  (define (binding-groups binder value annotated)
    (define type-text (type-at-syntax value))
    (cond
      [(not type-text) '()]
      [else
       (syntax-parse binder
         [name:id
          (type-group (source-ranges binder value)
                      (if (unannotated? binder annotated)
                          (list (type-anchor binder type-text))
                          '()))]
         [(part ...)
          (define bound (filter identifier? (attribute part)))
          (define types (split-values-types type-text))
          ;; Without one type per name there is no way to tell them apart.
          (cond
            [(and (pair? bound) (= (length bound) (length types)))
             (type-group (source-ranges binder value)
                         (for/list ([name (in-list bound)]
                                    [value-type (in-list types)]
                                    #:when (unannotated? name annotated))
                           (type-anchor name value-type)))]
            [else '()])]
         [_ '()])]))

  ;; The function type sits on the whole form, so the hint goes after the
  ;; header, where a return type would be written.
  (define (function-groups form header annotated)
    (define name (first-name (syntax->list header)))
    (define type-text (type-at-syntax form))
    (if (and name type-text (unannotated? name annotated))
        (type-group (source-ranges header)
                    (list (type-anchor header type-text)))
        '()))

  (define (clause-groups clauses annotated)
    (append*
      (for/list ([clause (in-list clauses)])
        (syntax-parse clause
          ;; A third form means `[x : T v]`, annotated already.
          [(binder value) (binding-groups #'binder #'value annotated)]
          [_ '()]))))

  (define (collect siblings)
    (define annotated (annotated-names siblings))
    (append*
      (for/list ([form (in-list siblings)]
                 #:when (code-form? form))
        (form-groups form annotated))))

  ;; Quoted data binds nothing, so quotation forms are not descended into.
  (define (form-groups form annotated)
    (append
      (syntax-parse form
        #:datum-literals (define define-values :)
        ;; A type the source writes itself leaves nothing to show.
        [(define _:code-list : _ ...) '()]
        [(define _:id : _ ...) '()]
        [(define header:code-list _ ...)
         (function-groups form #'header annotated)]
        [(define binder:id value _ ...)
         (binding-groups #'binder #'value annotated)]
        [(define-values binder value _ ...)
         (binding-groups #'binder #'value annotated)]
        [bindings:let-form
         (clause-groups (attribute bindings.clauses) annotated)]
        [_ '()])
      ;; Nested binding forms, including a `let` clause whose right-hand side
      ;; is itself one.
      (collect (syntax->list form))))

  (collect (module-body-forms stx)))

(provide typed-racket-inlay-hints)
