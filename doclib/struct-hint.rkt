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
;; The hints of a call are read from the call itself and from every `struct`
;; form in its supertype chain. What an argument fills is exactly what those
;; forms say, so editing any of them drops the hints until the document is
;; analyzed again.

(require racket/contract
         syntax/parse
         "../common/interfaces.rkt"
         "inlay-hint-source.rkt"
         "syntax-query.rkt")

;; A supertype chain long enough to hit this is a cycle in the making.
(define *max-super-depth* 16)

(struct/contract Field-Spec
  ([name string?]
   [auto? boolean?])
  #:transparent)

;; A reader answers #f for a spec it cannot read, which rejects the whole list.
(define field-spec-reader/c
  (-> syntax? (or/c #f Field-Spec?)))

;; The struct fills an `#:auto` field itself, so it is not a constructor
;; argument.
(define-syntax-class racket-field
  #:attributes (value)

  (pattern name:id
           #:attr value (Field-Spec (symbol->string (syntax-e #'name)) #f))

  (pattern (name:id option ...)
           #:attr value
           (Field-Spec
             (symbol->string (syntax-e #'name))
             (for/or ([option (in-list (attribute option))])
               (eq? '#:auto (syntax-e option))))))

;; Typed Racket writes every field as `[name : Type]`, nothing more: a bare name
;; is a type error there, and per-field options are not part of the form at all.
(define-syntax-class typed-racket-field
  #:attributes (value)
  #:datum-literals (:)

  (pattern (name:id : _type)
           #:attr value (Field-Spec (symbol->string (syntax-e #'name)) #f)))

(define/contract (racket-field-spec stx)
  field-spec-reader/c
  (syntax-parse stx
    [field:racket-field (attribute field.value)]
    [_ #f]))

(define/contract (typed-racket-field-spec stx)
  field-spec-reader/c
  (syntax-parse stx
    [field:typed-racket-field (attribute field.value)]
    [_ #f]))

;; How a dialect declares a struct: the heads of the forms that declare one, and
;; how each of their field specs is read.
(struct/contract Struct-Dialect
  ([form-heads (listof symbol?)]
   [read-field-spec field-spec-reader/c])
  #:transparent)

;; `struct/contract` shares `struct`'s shape, its contracts sitting where field
;; options would. Typed Racket has no such form.
(define racket-struct-dialect
  (Struct-Dialect '(struct struct/contract) racket-field-spec))

(define typed-racket-struct-dialect
  (Struct-Dialect '(struct) typed-racket-field-spec))

;; A form that names no field list is not a declaration this reader can use,
;; so it is not one of these shapes.
(define-syntax-class (struct-declaration struct-head?)
  #:attributes (name super field-list)

  ;; (struct child parent (field ...) option ...)
  (pattern (head:id name-id:id super-id:id fields option ...)
           #:when (struct-head? #'head)
           #:when (syntax->list #'fields)
           #:attr name #'name-id
           #:attr super #'super-id
           #:attr field-list #'fields)

  ;; (struct point (field ...) option ...)
  (pattern (head:id name-id:id fields option ...)
           #:when (struct-head? #'head)
           #:when (syntax->list #'fields)
           #:attr name #'name-id
           #:attr super #f
           #:attr field-list #'fields))

;; The specs in declaration order, or #f when one of them is unreadable.
(define/contract (parse-field-list read-field-spec field-list)
  (-> field-spec-reader/c syntax? (or/c #f (listof Field-Spec?)))
  (syntax-parse field-list
    [(field ...)
     (define specs (map read-field-spec (attribute field)))
     (and (andmap Field-Spec? specs) specs)]
    [_ #f]))

(define/contract (constructor-field-names specs)
  (-> (listof Field-Spec?) (listof string?))
  (for/list ([spec (in-list specs)]
             #:unless (Field-Spec-auto? spec))
    (Field-Spec-name spec)))

;; One `struct` form: the fields a constructor call of it takes, where its
;; supertype is named, and the span the whole form was read from. `fields` is
;; #f when the field list could not be read.
(struct/contract Struct-Decl
  ([name string?]
   [super-pos (or/c #f exact-nonnegative-integer?)]
   [fields (or/c #f (listof Field-Spec?))]
   [range CharRange?])
  #:transparent)

(struct/contract Call-Site
  ([head-pos exact-nonnegative-integer?]
   [args (listof (cons/c exact-nonnegative-integer? (or/c #f string?)))]
   [range CharRange?])
  #:transparent)

;; The declaration `form` makes, paired with the range Check Syntax reports for
;; the name it binds, or #f when the form declares no struct of this dialect.
(define (read-struct-declaration dialect form)
  (define (struct-head? head)
    (memq (syntax-e head) (Struct-Dialect-form-heads dialect)))

  (syntax-parse form
    [(~var decl (struct-declaration struct-head?))
     (define name (attribute decl.name))
     (define super (attribute decl.super))
     (define name-range (syntax-char-range name))
     (define form-range (syntax-char-range form))
     (and name-range
          form-range
          (cons name-range
                (Struct-Decl (symbol->string (syntax-e name))
                             (and super (syntax-start super))
                             (parse-field-list (Struct-Dialect-read-field-spec dialect)
                                               (attribute decl.field-list))
                             form-range)))]
    [_ #f]))

;; A call whose arguments could be named: a keyword argument moves the positions
;; around, so nothing is hinted for a call carrying one.
(define-syntax-class constructor-call
  #:attributes (head (args 1))

  (pattern (head-id:id args ...+)
           #:when (syntax-start #'head-id)
           #:when (andmap syntax-start (attribute args))
           #:when (not (ormap syntax-keyword (attribute args)))
           #:attr head #'head-id))

;; The call `form` makes, or #f when it is not one of those.
(define (read-call-site form)
  (syntax-parse form
    [call:constructor-call
     (define range (syntax-char-range form))
     (and range
          (Call-Site (syntax-start (attribute call.head))
                     (for/list ([arg (in-list (attribute call.args))])
                       (cons (syntax-start arg)
                             (and (identifier? arg)
                                  (symbol->string (syntax-e arg)))))
                     range))]
    [_ #f]))

;; The hint source a language with `struct` forms publishes, reading them the
;; way that language writes them.
(define/contract ((struct-field-inlay-hints dialect) context stx)
  (-> Struct-Dialect? inlay-hint-source/c)
  (define definition-at (Inlay-Hint-Context-definition-at context))
  ;; Declarations by the range Check Syntax reports for the name they bind, so
  ;; a use resolves to one in a single lookup.
  (define declarations (make-hash))
  (define calls '())

  (for-each-code-form
    (module-body-forms stx)
    (lambda (form)
      (define declaration (read-struct-declaration dialect form))
      (when declaration
        (hash-set! declarations (car declaration) (cdr declaration)))
      (define call (read-call-site form))
      (when call
        (set! calls (cons call calls)))))

  (define (declaration-at pos)
    (define range (definition-at pos))
    (and range (hash-ref declarations range #f)))

  ;; Constructor arguments in declaration order, supertype fields first, and
  ;; the forms they were read from. Names are #f when any part of the chain
  ;; cannot be read from this document.
  (define (constructor-fields declaration depth)
    (define own (Struct-Decl-fields declaration))
    (define super-pos (Struct-Decl-super-pos declaration))
    (define super
      (and super-pos
           (< depth *max-super-depth*)
           (declaration-at super-pos)))
    (define-values (super-names super-ranges)
      (cond
        [(not super-pos) (values '() '())]
        [super (constructor-fields super (add1 depth))]
        [else (values #f '())]))
    (if (and own super-names)
        (values (append super-names (constructor-field-names own))
                (cons (Struct-Decl-range declaration) super-ranges))
        (values #f '())))

  (define (call-group call)
    (define declaration (declaration-at (Call-Site-head-pos call)))
    (define-values (fields ranges)
      (if declaration
          (constructor-fields declaration 0)
          (values #f '())))
    (define args (Call-Site-args call))
    (define anchors
      (and fields
           (= (length fields) (length args))
           (for/list ([field (in-list fields)]
                      [arg (in-list args)]
                      #:unless (equal? field (cdr arg)))
             (Inlay-Hint-Anchor (car arg)
                                InlayHintKind-Parameter
                                (string-append field " ")
                                (format "field ~a of struct ~a"
                                        field
                                        (Struct-Decl-name declaration))))))
    (and (pair? anchors)
         (Inlay-Hint-Group (cons (Call-Site-range call) ranges) anchors)))

  (filter values (map call-group (reverse calls))))

(provide racket-struct-dialect
         typed-racket-struct-dialect
         struct-field-inlay-hints)
