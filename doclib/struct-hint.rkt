#lang racket/base

;; Struct field inlay hints: at a constructor call each argument gets the name
;; of the field it fills, so `(point 1 2)` reads as `(point x 1 y 2)`.
;;
;; The head of the call must resolve, through the binding graph, to the name of
;; a `struct` form in this document. An accessor resolves to a field name, which
;; sits in the field list rather than at the struct's name, so `(point-x p)` is
;; never taken for a constructor. `define-struct` binds a `make-` constructor
;; Check Syntax reports no definition for, so it is not covered.
;;
;; A hint is produced only when the whole field list is known and matches the
;; call: anything unread means no hint rather than a guessed one.
;;
;; Positions are absolute character offsets. The caller converts them and
;; applies the `#lang` gate.

(require racket/contract
         racket/class
         racket/list
         racket/match
         "../common/interfaces.rkt"
         "inlay-hint-source.rkt"
         (only-in "lexer/token-tree.rkt"
                  token-node?
                  Token-Leaf?
                  Token-List?
                  Token-List-children
                  Token-Forest?
                  Token-Forest-nodes
                  token-node-children
                  token-node-start
                  token-node-end
                  non-skippable-node?
                  token-leaf-type?)
         (only-in "lexer/tree-query.rkt"
                  token-forest-ancestors-at-pos))

(struct/contract Struct-Field-Anchor
  ([pos exact-nonnegative-integer?]
   [label string?]
   [tooltip string?])
  #:transparent)

;; A supertype chain long enough to hit this is a cycle in the making.
(define *max-super-depth* 16)

(define (token-node-text text node)
  (substring text (token-node-start node) (token-node-end node)))

(define (symbol-leaf? node)
  (and (Token-Leaf? node)
       (token-leaf-type? node 'symbol)))

(define (keyword-leaf? node)
  (and (Token-Leaf? node)
       (token-leaf-type? node 'hash-colon-keyword)))

(define (meaningful nodes)
  (filter non-skippable-node? nodes))

(define (auto-field? text parts)
  (for/or ([part (in-list parts)])
    (and (keyword-leaf? part)
         (equal? "#:auto" (token-node-text text part)))))

(struct/contract Field-Spec
  ([name string?]
   [auto? boolean?])
  #:transparent)

;; A reader answers #f for a node it cannot read, which rejects the whole list.
(define field-spec-reader/c
  (-> string? token-node? (or/c #f Field-Spec?)))

;; The struct fills an `#:auto` field itself, so it is not a constructor
;; argument.
(define/contract (racket-field-spec text node)
  field-spec-reader/c
  (define parts (and (Token-List? node)
                     (meaningful (Token-List-children node))))
  (cond
    [(symbol-leaf? node)
     (Field-Spec (token-node-text text node) #f)]
    [(and parts (pair? parts) (symbol-leaf? (first parts)))
     (Field-Spec (token-node-text text (first parts))
                 (auto-field? text parts))]
    [else #f]))

;; Typed Racket writes every field as `[name : Type]`, nothing more: a bare name
;; is a type error there, and per-field options are not part of the form at all.
(define/contract (typed-racket-field-spec text node)
  field-spec-reader/c
  (match (and (Token-List? node)
              (meaningful (Token-List-children node)))
    [(list name colon _type)
     #:when (and (symbol-leaf? name)
                 (symbol-leaf? colon)
                 (equal? ":" (token-node-text text colon)))
     (Field-Spec (token-node-text text name) #f)]
    [_ #f]))

;; How a dialect declares a struct: the heads of the forms that declare one, and
;; how each of their field specs is read.
(struct/contract Struct-Dialect
  ([form-heads (listof string?)]
   [read-field-spec field-spec-reader/c])
  #:transparent)

;; `struct/contract` shares `struct`'s shape, its contracts sitting where field
;; options would. Typed Racket has no such form.
(define racket-struct-dialect
  (Struct-Dialect '("struct" "struct/contract") racket-field-spec))

(define typed-racket-struct-dialect
  (Struct-Dialect '("struct") typed-racket-field-spec))

;; The specs in declaration order, or #f when one of them is unreadable.
(define/contract (parse-field-list read-field-spec text field-list)
  (-> field-spec-reader/c string? Token-List? (or/c #f (listof Field-Spec?)))
  (define specs
    (for/list ([node (in-list (meaningful (Token-List-children field-list)))])
      (read-field-spec text node)))
  (and (andmap Field-Spec? specs) specs))

(define/contract (constructor-field-names specs)
  (-> (listof Field-Spec?) (listof string?))
  (for/list ([spec (in-list specs)]
             #:unless (Field-Spec-auto? spec))
    (Field-Spec-name spec)))

;; `definition-at` is the `declaration%` lookup: pos -> (values start end),
;; both #f when the name has no definition in this document.
(define/contract (struct-field-inlay-anchors dialect text forest definition-at)
  (-> Struct-Dialect?
      string?
      Token-Forest?
      (-> exact-nonnegative-integer? any)
      (listof Struct-Field-Anchor?))

  (define (node-text node)
    (token-node-text text node))

  ;; The struct form whose own name spans [def-start, def-end), or #f.
  (define (struct-form-at def-start def-end)
    (match (token-forest-ancestors-at-pos forest def-start)
      [(list name-node form _ ...)
       #:when (and (symbol-leaf? name-node)
                   (= (token-node-start name-node) def-start)
                   (= (token-node-end name-node) def-end)
                   (Token-List? form))
       (match (meaningful (Token-List-children form))
         [(list head name _ ...)
          #:when (and (symbol-leaf? head)
                      (member (node-text head)
                              (Struct-Dialect-form-heads dialect))
                      (eq? name name-node))
          form]
         [_ #f])]
      [_ #f]))

  (define (definition-form-for use-node)
    (define-values (def-start def-end)
      (definition-at (token-node-start use-node)))
    (and def-start def-end (struct-form-at def-start def-end)))

  (define (struct-form-name form)
    (match (meaningful (Token-List-children form))
      [(list _head name _ ...) (node-text name)]
      [_ ""]))

  ;; Constructor arguments, supertype fields first, or #f when any part of the
  ;; chain cannot be read from this document.
  (define (constructor-fields form depth)
    (define forms (meaningful (Token-List-children form)))
    ;; An optional supertype, the field list, then struct options.
    (define after-name (if (> (length forms) 2) (drop forms 2) '()))
    (define super-node
      (and (pair? after-name)
           (symbol-leaf? (first after-name))
           (first after-name)))
    (define field-list
      (for/first ([node (in-list after-name)]
                  #:when (Token-List? node))
        node))
    (define own-specs
      (and field-list
           (parse-field-list (Struct-Dialect-read-field-spec dialect)
                             text
                             field-list)))
    (define own-names (and own-specs (constructor-field-names own-specs)))
    (cond
      [(not own-names) #f]
      [(not super-node) own-names]
      [(>= depth *max-super-depth*) #f]
      [else
       (define super-form (definition-form-for super-node))
       (define super-names
         (and super-form (constructor-fields super-form (add1 depth))))
       (and super-names (append super-names own-names))]))

  (define (application-anchors node)
    (match (meaningful (Token-List-children node))
      ;; A keyword argument moves the positions around, so nothing is hinted.
      [(list head args ..1)
       #:when (and (symbol-leaf? head)
                   (not (ormap keyword-leaf? args)))
       (define form (definition-form-for head))
       (define fields (and form (constructor-fields form 0)))
       (cond
         [(and fields (= (length fields) (length args)))
          (define name (struct-form-name form))
          (for/list ([field (in-list fields)]
                     [arg (in-list args)]
                     #:unless (equal? field (node-text arg)))
            (Struct-Field-Anchor (token-node-start arg)
                                 (string-append field " ")
                                 (format "field ~a of struct ~a" field name)))]
         [else '()])]
      [_ '()]))

  ;; Quoted data applies nothing, so prefix trees are not descended into.
  (define (collect nodes)
    (append*
      (for/list ([node (in-list nodes)]
                 #:when (Token-List? node))
        (append (application-anchors node)
                (collect (meaningful (Token-List-children node)))))))

  (collect (meaningful (Token-Forest-nodes forest))))

;; The hint source a language with `struct` forms publishes, reading them the way
;; that language writes them.
(define/contract ((struct-field-inlay-hints dialect) context req-start req-end)
  (-> Struct-Dialect? inlay-hint-source/c)
  (define declaration-service
    (send (Inlay-Hint-Context-trace context) get-declaration))
  (define abs-pos->pos (Inlay-Hint-Context-abs-pos->pos context))
  (define anchors
    (struct-field-inlay-anchors
      dialect
      (Inlay-Hint-Context-text context)
      (Inlay-Hint-Context-forest context)
      (lambda (pos)
        (define definition-range
          (send declaration-service definition-at pos))
        (if definition-range
            (values (CharRange-start definition-range)
                    (CharRange-end definition-range))
            (values #f #f)))))
  (for/list ([anchor (in-list anchors)]
             #:when (<= req-start (Struct-Field-Anchor-pos anchor) req-end))
    (InlayHint #:position (abs-pos->pos (Struct-Field-Anchor-pos anchor))
               #:label (Struct-Field-Anchor-label anchor)
               #:kind InlayHintKind-Parameter
               #:tooltip (Struct-Field-Anchor-tooltip anchor))))

(provide (struct-out Struct-Field-Anchor)
         (struct-out Struct-Dialect)
         field-spec-reader/c
         racket-field-spec
         typed-racket-field-spec
         racket-struct-dialect
         typed-racket-struct-dialect
         struct-field-inlay-anchors
         struct-field-inlay-hints)
