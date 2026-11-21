#lang racket/base

;; JSON-serializable protocol data types and jsexpr utilities.
;;
;; Core concepts:
;;   1. `json-codec` — first-class runtime codec (label, predicate, decoder)
;;      composed via plain functions.
;;   2. Three declaration forms:
;;      - `define-json-struct` — transparent struct with flat-checked constructor,
;;        compile-time keyword calls/defaults, unrolled `->jsexpr`, and `match`.
;;      - `define-json-enum`   — tagged enum struct with `case` encoding and O(1)
;;        hash-table decoding.
;;      - `define-json-union`  — first-class untagged union codec + predicate.
;;   3. Operations and match expanders:
;;      - `(->jsexpr v)`                        encode value to immutable jsexpr
;;      - `(jsexpr-> type-spec js)`             strict decode
;;      - `(jsexpr-> type-spec js default)`     non-throwing decode (lazy `default`)
;;      - `(jsexpr-> type-spec)`                first-class decoder `(js [default] -> any)`
;;      - `(jsexpr type-spec pat)`              general decode match expander
;;      - `(^Name pat ...)`                     decode + destructure (`(jsexpr Name (Name pat ...))`,
;;                                              for structs and enums)
;;
;; Omitted optional fields use the singleton constant `omitted` (`omitted?`).
;; Export types with standard `(struct-out Name)` (for structs) or `(json-out Name ...)`.

(require (for-syntax racket/base
                     racket/list
                     racket/set
                     racket/struct-info
                     racket/syntax
                     syntax/parse
                     racket/provide-transform)
         json
         racket/contract/base
         racket/generic
         racket/match)

(provide define-json-struct
         define-json-enum
         define-json-union
         json-out
         ->jsexpr
         jsexpr->
         jsexpr
         omitted
         omitted?
         gen:jsexpr-struct
         jsexpr-struct?
         struct->jsexpr
         jsexpr-has-key?
         jsexpr-ref
         jsexpr-set
         jsexpr-remove)

;; ============================================================================
;; Sentinels, jsexpr hash helpers, and generic encoding
;; ============================================================================

;; Unique singleton for omitted optional JSON fields (excluded on encode,
;; produced for missing keys on decode). Uses a local struct rather than
;; `gensym` because an uninterned symbol satisfies `symbol?`, which would
;; let missing required `symbol?` fields pass decoding and constructor checks.
(define omitted
  (let ()
    (struct omitted () #:transparent)
    (omitted)))

(define (omitted? v)
  (eq? v omitted))

;; Private uninterned sentinels — never collide with valid JSON or `omitted`.
(define absent-key (gensym 'absent-key))
(define decode-failed (gensym 'decode-failed))

(define (decode-failed? v)
  (eq? v decode-failed))

(define (jsexpr-has-key? jsexpr keys)
  (cond
    [(null? keys) #t]
    [(not (hash? jsexpr)) #f]
    [else
     (define next (hash-ref jsexpr (car keys) absent-key))
     (and (not (eq? next absent-key))
          (jsexpr-has-key? next (cdr keys)))]))

(define (jsexpr-ref jsexpr keys)
  (if (null? keys)
      jsexpr
      (jsexpr-ref (hash-ref jsexpr (car keys)) (cdr keys))))

(define (jsexpr-set jsexpr keys v)
  (cond
    [(null? keys) jsexpr]
    [(null? (cdr keys)) (hash-set jsexpr (car keys) v)]
    [else (hash-set jsexpr (car keys)
                    (jsexpr-set (hash-ref jsexpr (car keys)) (cdr keys) v))]))

(define (jsexpr-remove jsexpr keys)
  (cond
    [(null? keys) jsexpr]
    [(null? (cdr keys)) (hash-remove jsexpr (car keys))]
    [else (hash-set jsexpr (car keys)
                    (jsexpr-remove (hash-ref jsexpr (car keys)) (cdr keys)))]))

(define-generics jsexpr-struct
  (struct->jsexpr jsexpr-struct))

;; Recursively converts structs, hashes, and lists into JSON-compatible
;; immutable hashes. `omitted` values are excluded from hash output.
(define (->jsexpr v)
  (cond
    [(omitted? v) (json-null)]
    [(jsexpr-struct? v) (struct->jsexpr v)]
    [(hash? v)
     (for/hasheq ([(k val) (in-hash v)]
                  #:unless (omitted? val))
       (unless (symbol? k)
         (raise-argument-error '->jsexpr "(hash/c symbol? any/c)" v))
       (values k (->jsexpr val)))]
    [(list? v) (map ->jsexpr v)]
    [else v]))

;; ============================================================================
;; First-class runtime `json-codec` and combinators
;; ============================================================================

;; Runtime codec pairing type diagnostics with validation and JSON decoding:
;; - `label`:  datum (symbol or quoted type form) for error diagnostics
;; - `pred?`:  `(-> any/c boolean?)` checking already-decoded Racket values
;; - `decode`: `(-> any/c boolean? any/c)` decoding `jsexpr`; when `strict?` is
;;             `#f`, returns `decode-failed`, when `#t`, raises `exn:fail:contract`
(struct json-codec (label pred? decode)
  #:transparent)

;; Callable wrapper carrying a type's codec so a bare type identifier (e.g. `Pos`
;; or `FileChangeType`) works both as a procedure (`(map Pos ...)`) and as a
;; codec argument (`(jsexpr-> Pos js)`).
(struct ctor-with-codec (proc codec)
  #:property prop:procedure (struct-field-index proc))

(define (raise-field-contract-error type-name field-name expected given)
  (raise-arguments-error type-name
                         "contract violation"
                         "field" field-name
                         "expected" expected
                         "given" given))

(define (raise-decode-error who expected given)
  (raise-arguments-error who
                         "failed to decode JSON value"
                         "expected" expected
                         "value" given))

(define (decode-struct-field type-name field-name json-key label decode js optional?)
  (define raw (hash-ref js json-key omitted))
  (cond
    [(omitted? raw)
     (if optional?
         raw
         (raise-arguments-error 'jsexpr->
                                "missing required field"
                                "type" type-name
                                "field" field-name
                                "json-key" json-key
                                "expected" label))]
    [else
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (raise-arguments-error 'jsexpr->
                                               "failed to decode field"
                                               "type" type-name
                                               "field" field-name
                                               "expected" label
                                               "value" raw
                                               "cause" (exn-message e)))])
       (decode raw #t))]))

(define (pred-codec label pred?)
  (define (decode x strict?)
    (cond
      [(pred? x) x]
      [strict? (raise-decode-error 'jsexpr-> label x)]
      [else decode-failed]))
  (json-codec label pred? decode))

;; Coerces flat Racket contracts (predicates, literals, regexps, flat or/c, etc.)
;; and guards narrow-domain predicates (e.g. `positive?`) so non-throwing
;; `jsexpr->` and `jsexpr` treat domain mismatches as decode failures instead of throwing.
(define (contract-codec label ctc)
  (unless (flat-contract? ctc)
    (raise-argument-error 'contract "flat-contract?" ctc))
  (define pred? (flat-contract-predicate ctc))
  (define (safe-pred? x)
    (with-handlers ([exn:fail:contract? (lambda (_) #f)])
      (pred? x)))
  (pred-codec label safe-pred?))

(define builtin-codecs
  (hasheq string? (pred-codec 'string? string?)
          boolean? (pred-codec 'boolean? boolean?)
          number? (pred-codec 'number? number?)
          exact-integer? (pred-codec 'exact-integer? exact-integer?)
          exact-nonnegative-integer? (pred-codec 'exact-nonnegative-integer?
                                                 exact-nonnegative-integer?)
          symbol? (pred-codec 'symbol? symbol?)))

(define (->json-codec x)
  (cond
    [(json-codec? x) x]
    [(ctor-with-codec? x) (ctor-with-codec-codec x)]
    [(hash-ref builtin-codecs x #f)]
    [else (raise-argument-error 'jsexpr-> "json-codec?" x)]))

(define (listof-codec elem)
  (define elem-pred? (json-codec-pred? elem))
  (define elem-decode (json-codec-decode elem))
  (define label `(listof ,(json-codec-label elem)))
  (define (pred? xs)
    (and (list? xs) (andmap elem-pred? xs)))
  (define (decode xs strict?)
    (cond
      [(list? xs)
       (let loop ([rest xs] [acc '()])
         (if (null? rest)
             (reverse acc)
             (let ([v (elem-decode (car rest) strict?)])
               (if (decode-failed? v)
                   decode-failed
                   (loop (cdr rest) (cons v acc))))))]
      [strict? (raise-decode-error 'jsexpr-> label xs)]
      [else decode-failed]))
  (json-codec label pred? decode))

(define (optional-codec elem)
  (define elem-pred? (json-codec-pred? elem))
  (define elem-decode (json-codec-decode elem))
  (define label `(optional ,(json-codec-label elem)))
  (define (pred? x)
    (or (omitted? x) (elem-pred? x)))
  (define (decode x strict?)
    (if (omitted? x) x (elem-decode x strict?)))
  (json-codec label pred? decode))

(define (hashof-codec val-codec)
  (define v-pred? (json-codec-pred? val-codec))
  (define v-decode (json-codec-decode val-codec))
  (define label `(hashof ,(json-codec-label val-codec)))
  (define (pred? h)
    (and (hash? h)
         (immutable? h)
         (for/and ([(k v) (in-hash h)])
           (and (symbol? k) (v-pred? v)))))
  (define (decode h strict?)
    (cond
      [(hash? h)
       (for/fold ([acc (hasheq)])
                 ([(k v) (in-hash h)]
                  #:break (decode-failed? acc))
         (cond
           [(symbol? k)
            (let ([dv (v-decode v strict?)])
              (if (decode-failed? dv)
                  decode-failed
                  (hash-set acc k dv)))]
           [strict? (raise-decode-error 'jsexpr-> label h)]
           [else decode-failed]))]
      [strict? (raise-decode-error 'jsexpr-> label h)]
      [else decode-failed]))
  (json-codec label pred? decode))

(define (union-codec . codecs)
  (define preds (map json-codec-pred? codecs))
  (define decoders (map json-codec-decode codecs))
  (define label `(union ,@(map json-codec-label codecs)))
  (define (pred? x)
    (for/or ([p (in-list preds)])
      (p x)))
  (define (decode x strict?)
    (let loop ([ds decoders])
      (cond
        [(null? ds)
         (if strict?
             (raise-decode-error 'jsexpr-> label x)
             decode-failed)]
        [else
         (define r ((car ds) x #f))
         (if (decode-failed? r)
             (loop (cdr ds))
             r)])))
  (json-codec label pred? decode))

;; Defers codec resolution for self-referential and mutually recursive types.
(define (lazy-codec label thunk)
  (define (resolve!)
    (define c (->json-codec (thunk)))
    (set! pred-fn (json-codec-pred? c))
    (set! decode-fn (json-codec-decode c)))
  (define pred-fn (lambda (x) (resolve!) (pred-fn x)))
  (define decode-fn (lambda (x strict?) (resolve!) (decode-fn x strict?)))
  (json-codec label
              (lambda (x) (pred-fn x))
              (lambda (x strict?) (decode-fn x strict?))))

;; ============================================================================
;; Compile-time syntax helpers
;; ============================================================================

(begin-for-syntax
  ;; Struct binding implementing `prop:struct-info` (for standard `struct-out`),
  ;; `prop:match-expander` (positional + keyword patterns), and `prop:procedure`
  ;; (positional + keyword constructors, plus first-class value reference).
  (struct json-struct-transformer (info match expression)
    #:property prop:struct-info (lambda (self) (json-struct-transformer-info self))
    #:property prop:match-expander (struct-field-index match)
    #:property prop:procedure (struct-field-index expression))

  ;; Enum binding implementing `prop:match-expander` and `prop:procedure`.
  (struct json-enum-transformer (match expression)
    #:property prop:match-expander (struct-field-index match)
    #:property prop:procedure (struct-field-index expression))

  (define (symbol->keyword sym)
    (string->keyword (symbol->string sym)))

  (define builtin-predicate-ids
    (syntax->list #'(string? boolean? number? exact-integer?
                             exact-nonnegative-integer? symbol?)))

  (define (builtin-pred-id? id)
    (and (identifier? id)
         (for/or ([b (in-list builtin-predicate-ids)])
           (free-identifier=? id b))))

  (define (ensure-no-duplicates who what stxs)
    (define seen (mutable-seteq))
    (for ([s (in-list stxs)])
      (define key (syntax-e s))
      (when (set-member? seen key)
        (raise-syntax-error who (format "duplicate ~a ~a" what key) s))
      (set-add! seen key)))

  (define (ensure-no-duplicate-datums who what stxs)
    (define seen (mutable-set))
    (for ([s (in-list stxs)])
      (define d (syntax->datum s))
      (when (or (boolean? d) (number? d) (string? d) (symbol? d))
        (when (set-member? seen d)
          (raise-syntax-error who (format "duplicate ~a ~v" what d) s))
        (set-add! seen d))))

  (define (any-keyword? stx)
    (for/or ([x (in-list (syntax->list stx))])
      (keyword? (syntax-e x))))

  (define (gen-exports exports-id export-specs)
    (with-syntax ([exports-id exports-id]
                  [(spec ...) export-specs])
      #'(define-syntax exports-id
          (make-provide-transformer
            (lambda (_stx modes)
              (expand-export #'(combine-out exports-id spec ...) modes))))))

  (define (gen-decode-match-expander decode-match-id name-id)
    (with-syntax ([decode-match-id decode-match-id]
                  [name name-id])
      #'(define-match-expander decode-match-id
          (lambda (inner-stx)
            (syntax-parse inner-stx
              [(_ pat (... ...))
               #'(jsexpr name (name pat (... ...)))])))))

  ;; Parses a type-spec into eager (`codec-expr`, for call sites) and lazy
  ;; (`lazy-codec-expr`, for declarations that may reference recursive types)
  ;; runtime `json-codec` expressions.
  (define-syntax-class type-spec
    #:attributes (codec-expr lazy-codec-expr)
    #:datum-literals (contract listof optional hashof union)
    (pattern tid:id
             #:when (builtin-pred-id? #'tid)
             #:with codec-expr #'(->json-codec tid)
             #:with lazy-codec-expr #'codec-expr)
    (pattern tid:id
             #:with codec-expr #'(->json-codec tid)
             #:with lazy-codec-expr #`(lazy-codec '#,(syntax->datum #'tid)
                                                  (lambda () tid)))
    (pattern (contract pred-expr:expr)
             #:with codec-expr #`(contract-codec '#,(syntax->datum #'(contract pred-expr))
                                                 pred-expr)
             #:with lazy-codec-expr #'codec-expr)
    (pattern (listof elem:type-spec)
             #:with codec-expr #'(listof-codec elem.codec-expr)
             #:with lazy-codec-expr #'(listof-codec elem.lazy-codec-expr))
    (pattern (optional elem:type-spec)
             #:with codec-expr #'(optional-codec elem.codec-expr)
             #:with lazy-codec-expr #'(optional-codec elem.lazy-codec-expr))
    (pattern (hashof v:type-spec)
             #:with codec-expr #'(hashof-codec v.codec-expr)
             #:with lazy-codec-expr #'(hashof-codec v.lazy-codec-expr))
    (pattern (union opt:type-spec ...+)
             #:with codec-expr #'(union-codec opt.codec-expr ...)
             #:with lazy-codec-expr #'(union-codec opt.lazy-codec-expr ...)))

  ;; True when `->jsexpr` is the identity on values matching `stx`
  ;; (so `define-json-struct` can emit the field directly without calling `->jsexpr`).
  (define (identity-enc-type? stx)
    (syntax-parse stx
      #:datum-literals (listof union)
      [tid:id (builtin-pred-id? #'tid)]
      [(listof elem) (identity-enc-type? #'elem)]
      [(union opt ...+) (andmap identity-enc-type? (syntax->list #'(opt ...)))]
      [_ #f]))

  (define-syntax-class field-type-spec
    #:attributes (lazy-elem-codec-expr optional? direct-pred identity-enc?)
    #:datum-literals (optional)
    (pattern (optional elem:type-spec)
             #:with lazy-elem-codec-expr #'elem.lazy-codec-expr
             #:attr optional? #t
             #:with direct-pred #'#f
             #:attr identity-enc? (identity-enc-type? #'elem))
    (pattern ts:type-spec
             #:with lazy-elem-codec-expr #'ts.lazy-codec-expr
             #:attr optional? #f
             #:with direct-pred (or (and (builtin-pred-id? #'ts) #'ts) #'#f)
             #:attr identity-enc? (identity-enc-type? #'ts)))

  (define-syntax-class struct-field-clause
    #:attributes (field json-key keyword lazy-elem-codec-expr optional?
                        direct-pred field-identity-enc? label)
    (pattern [field:id fts:field-type-spec (~optional (~seq #:json custom-key:id))]
             #:with json-key (or (attribute custom-key) #'field)
             #:with keyword (symbol->keyword (syntax-e #'field))
             #:with lazy-elem-codec-expr #'fts.lazy-elem-codec-expr
             #:attr optional? (attribute fts.optional?)
             #:with direct-pred #'fts.direct-pred
             #:attr field-identity-enc? (attribute fts.identity-enc?)
             #:with label #`'#,(syntax->datum #'fts)))

  ;; Reorder keyword constructor arguments in caller evaluation order, filling
  ;; omitted optional / #:rest fields with their defaults.
  (define (expand-kw-ctor-call stx ctor-id keywords default-exprs kw-stxs val-stxs)
    (ensure-no-duplicates 'define-json-struct "keyword argument" kw-stxs)
    (define valid-kws (for/seteq ([kw (in-list keywords)]) (syntax-e kw)))
    (define temps (generate-temporaries val-stxs))
    (define supplied
      (for/hasheq ([k (in-list kw-stxs)]
                   [t (in-list temps)])
        (unless (set-member? valid-kws (syntax-e k))
          (raise-syntax-error 'define-json-struct "unknown keyword argument" stx k))
        (values (syntax-e k) t)))
    (define ordered
      (for/list ([k (in-list keywords)]
                 [def (in-list default-exprs)])
        (hash-ref supplied
                  (syntax-e k)
                  (lambda ()
                    (or def
                        (raise-syntax-error 'define-json-struct
                                            (format "missing keyword argument ~a" (syntax-e k))
                                            stx))))))
    (with-syntax ([ctor-id ctor-id]
                  [(temp ...) temps]
                  [(val ...) val-stxs]
                  [(arg ...) ordered])
      (quasisyntax/loc stx
        (let ([temp val] ...)
          (ctor-id arg ...)))))

  (define (expand-struct-kw-match stx internal-id keywords fields kw-stxs pat-stxs)
    (ensure-no-duplicates 'define-json-struct "keyword argument" kw-stxs)
    (define kw->field
      (for/hasheq ([kw (in-list keywords)]
                   [fld (in-list fields)])
        (values (syntax-e kw) fld)))
    (define pats
      (for/list ([k (in-list kw-stxs)]
                 [p (in-list pat-stxs)])
        (define fld
          (hash-ref kw->field
                    (syntax-e k)
                    (lambda ()
                      (raise-syntax-error 'define-json-struct
                                          (format "unknown keyword ~a" (syntax-e k))
                                          k))))
        #`[#,fld #,p]))
    #`(struct* #,internal-id (#,@pats)))

  (define (expand-struct-pos-match stx internal-id fields pat-stxs)
    (unless (= (length pat-stxs) (length fields))
      (raise-syntax-error
        'define-json-struct
        (format "wrong number of arguments for positional pattern (expected ~a, given ~a)"
                (length fields) (length pat-stxs))
        stx))
    #`(#,internal-id #,@pat-stxs))

  (struct field-info
    (id json-key keyword codec-expr optional? direct-pred identity-enc? label
        internal-acc arg-var codec-var pred-var decode-var))

  (define (build-field-codec-defs field-infos)
    (apply append
           (for/list ([fi (in-list field-infos)])
             (define dp (field-info-direct-pred fi))
             (define ecv (field-info-codec-var fi))
             (define ece (field-info-codec-expr fi))
             (define pv (field-info-pred-var fi))
             (define dv (field-info-decode-var fi))
             (if (syntax-e dp)
                 (list #`(define #,dv (json-codec-decode #,ece)))
                 (list #`(define #,ecv #,ece)
                       (if (field-info-optional? fi)
                           #`(define #,pv
                               (let ([ep (json-codec-pred? #,ecv)])
                                 (lambda (x) (or (omitted? x) (ep x)))))
                           #`(define #,pv (json-codec-pred? #,ecv)))
                       #`(define #,dv (json-codec-decode #,ecv)))))))

  ;; Unrolled serializer: direct `(hasheq ...)` for required fields,
  ;; chaining optional fields in a linear `let*` only when not `omitted`.
  (define (build-serializer-expr self-id field-infos rest-accessor)
    (define (encode-val id-enc? val-stx)
      (if id-enc? val-stx #`(->jsexpr #,val-stx)))
    (define-values (req-fields opt-fields)
      (partition (lambda (fi) (not (field-info-optional? fi))) field-infos))
    (define base-hash-expr
      (if rest-accessor
          (for/fold ([h #`(->jsexpr (#,rest-accessor #,self-id))])
                    ([fi (in-list req-fields)])
            #`(hash-set #,h
                        '#,(field-info-json-key fi)
                        #,(encode-val (field-info-identity-enc? fi)
                                      #`(#,(field-info-internal-acc fi) #,self-id))))
          (with-syntax ([(kv ...)
                         (apply append
                                (for/list ([fi (in-list req-fields)])
                                  (list #`'#,(field-info-json-key fi)
                                        (encode-val (field-info-identity-enc? fi)
                                                    #`(#,(field-info-internal-acc fi) #,self-id)))))])
            #'(hasheq kv ...))))
    (if (null? opt-fields)
        base-hash-expr
        (with-syntax ([base base-hash-expr]
                      [(opt-step ...)
                       (for/list ([fi (in-list opt-fields)])
                         (define jk (field-info-json-key fi))
                         (define acc (field-info-internal-acc fi))
                         (define absent-branch (if rest-accessor #`(hash-remove h '#,jk) #'h))
                         #`[h (let ([v (#,acc #,self-id)])
                                (if (omitted? v)
                                    #,absent-branch
                                    (hash-set h '#,jk #,(encode-val (field-info-identity-enc? fi) #'v))))])])
          #'(let* ([h base]
                   opt-step ...)
              h))))

  (define (build-ctor-checks name-id field-infos rest-field-stx rest-arg-var)
    (append
      (for/list ([fi (in-list field-infos)])
        (define dp (field-info-direct-pred fi))
        (define pred-expr (if (syntax-e dp) dp (field-info-pred-var fi)))
        (define arg (field-info-arg-var fi))
        #`(unless (#,pred-expr #,arg)
            (raise-field-contract-error '#,name-id
                                        '#,(field-info-id fi)
                                        #,(field-info-label fi)
                                        #,arg)))
      (if rest-arg-var
          (list #`(unless (hash? #,rest-arg-var)
                    (raise-field-contract-error '#,name-id '#,rest-field-stx 'hash? #,rest-arg-var)))
          '())))

  (define (build-try-decode-expr js-id field-infos try-construct-expr)
    (for/fold ([next try-construct-expr])
              ([fi (in-list (reverse field-infos))])
      (define jk (field-info-json-key fi))
      (define av (field-info-arg-var fi))
      (define dp (field-info-direct-pred fi))
      (define dv (field-info-decode-var fi))
      (cond
        [(syntax-e dp)
         #`(let ([#,av (hash-ref #,js-id '#,jk omitted)])
             (if (#,dp #,av)
                 #,next
                 decode-failed))]
        [(field-info-optional? fi)
         #`(let* ([raw (hash-ref #,js-id '#,jk omitted)]
                  [#,av (if (omitted? raw) raw (#,dv raw #f))])
             (if (decode-failed? #,av)
                 decode-failed
                 #,next))]
        [else
         #`(let ([raw (hash-ref #,js-id '#,jk omitted)])
             (if (omitted? raw)
                 decode-failed
                 (let ([#,av (#,dv raw #f)])
                   (if (decode-failed? #,av)
                       decode-failed
                       #,next))))]))))

;; ============================================================================
;; Decode / match public forms
;; ============================================================================

(define (make-jsexpr-decoder codec)
  (define decode (json-codec-decode codec))
  (case-lambda
    [(js) (decode js #t)]
    [(js default)
     (define r (decode js #f))
     (if (decode-failed? r) default r)]))

(define-syntax (jsexpr-> stx)
  (syntax-parse stx
    [(_ ts:type-spec)
     #'(make-jsexpr-decoder ts.codec-expr)]
    [(_ ts:type-spec js-expr:expr)
     #'((json-codec-decode ts.codec-expr) js-expr #t)]
    [(_ ts:type-spec js-expr:expr default-expr:expr)
     #'(let ([r ((json-codec-decode ts.codec-expr) js-expr #f)])
         (if (decode-failed? r)
             default-expr
             r))]))

(define-match-expander jsexpr
  (lambda (stx)
    (syntax-parse stx
      [(_ ts:type-spec pat)
       ;; Keep a bare `lambda` so `match`'s `app` beta-reduces it without
       ;; allocating a closure on every match.
       #'(app (lambda (v) ((json-codec-decode ts.codec-expr) v #f))
              (and (not (? decode-failed?))
                   pat))])))

(define-syntax json-out
  (make-provide-transformer
    (lambda (stx modes)
      (syntax-parse stx
        [(_ name:id ...)
         (with-syntax ([(export-call ...)
                        (for/list ([n (in-list (syntax->list #'(name ...)))])
                          #`(#,(format-id n "~a~~exports" n)))])
           (expand-export #'(combine-out export-call ...) modes))]))))

;; ============================================================================
;; define-json-struct
;; ============================================================================

(define-syntax (define-json-struct stx)
  (syntax-parse stx
    [(_ name:id clause:struct-field-clause ...+
        (~optional (~seq #:rest rest-field:id) #:defaults ([rest-field #f])))
     (define fields (syntax->list #'(clause.field ...)))
     (define json-keys (syntax->list #'(clause.json-key ...)))
     (define keywords (syntax->list #'(clause.keyword ...)))
     (define optional-flags (attribute clause.optional?))
     (define rest-field-stx (attribute rest-field))

     (define all-fields
       (if rest-field-stx (append fields (list rest-field-stx)) fields))
     (ensure-no-duplicates 'define-json-struct "field" all-fields)
     (ensure-no-duplicates 'define-json-struct "json key" json-keys)

     (define rest-kw
       (and rest-field-stx (symbol->keyword (syntax-e rest-field-stx))))
     (define all-keywords
       (if rest-field-stx (append keywords (list rest-kw)) keywords))
     (define field-defaults
       (for/list ([opt? (in-list optional-flags)])
         (and opt? #'omitted)))
     (define all-defaults
       (if rest-field-stx (append field-defaults (list #'(hasheq))) field-defaults))

     (define internal-id (format-id #'name "~a~~" #'name))
     (define struct-type-internal (format-id #'name "struct:~a~~" #'name))
     (define struct-type-public (format-id #'name "struct:~a" #'name))
     (define ctor-id (format-id #'name "~a~~ctor" #'name))
     (define val-id (format-id #'name "~a~~val" #'name))
     (define codec-id (format-id #'name "~a~~codec" #'name))
     (define pred-internal (format-id #'name "~a~~?" #'name))
     (define pred-id (format-id #'name "~a?" #'name))
     (define decode-match-id (format-id #'name "^~a" #'name))
     (define exports-id (format-id #'name "~a~~exports" #'name))
     (define internal-accessors
       (for/list ([f (in-list all-fields)])
         (format-id #'name "~a~~-~a" #'name f)))
     (define public-accessors
       (for/list ([f (in-list all-fields)])
         (format-id #'name "~a-~a" #'name f)))
     (define rest-accessor (and rest-field-stx (last internal-accessors)))
     (define arg-vars (generate-temporaries all-fields))
     (define rest-arg-var (and rest-field-stx (last arg-vars)))

     (define field-infos
       (for/list ([fld (in-list fields)]
                  [jk (in-list json-keys)]
                  [kw (in-list keywords)]
                  [ece (in-list (syntax->list #'(clause.lazy-elem-codec-expr ...)))]
                  [opt? (in-list optional-flags)]
                  [dp (in-list (syntax->list #'(clause.direct-pred ...)))]
                  [id-enc? (in-list (attribute clause.field-identity-enc?))]
                  [lbl (in-list (syntax->list #'(clause.label ...)))]
                  [acc (in-list (take internal-accessors (length fields)))]
                  [av (in-list (take arg-vars (length fields)))]
                  [ecv (in-list (generate-temporaries fields))]
                  [pv (in-list (generate-temporaries fields))]
                  [dv (in-list (generate-temporaries fields))])
         (field-info fld jk kw ece opt? dp id-enc? lbl acc av ecv pv dv)))

     (define rest-decode-expr
       (and rest-field-stx
            (with-syntax ([(jk ...) json-keys])
              #'(for/hasheq ([(k v) (in-hash js)]
                             #:unless (memq k '(jk ...)))
                  (values k v)))))
     (define try-construct-expr
       (if rest-arg-var
           #`(let ([#,rest-arg-var #,rest-decode-expr])
               (#,internal-id #,@arg-vars))
           #`(#,internal-id #,@arg-vars)))
     (define strict-field-exprs
       (for/list ([fi (in-list field-infos)])
         #`(decode-struct-field 'name
                                '#,(field-info-id fi)
                                '#,(field-info-json-key fi)
                                #,(field-info-label fi)
                                #,(field-info-decode-var fi)
                                js
                                #,(field-info-optional? fi))))
     (define strict-all-exprs
       (if rest-decode-expr
           (append strict-field-exprs (list rest-decode-expr))
           strict-field-exprs))

     (with-syntax ([internal-id internal-id]
                   [struct-type-internal struct-type-internal]
                   [struct-type-public struct-type-public]
                   [ctor-id ctor-id]
                   [val-id val-id]
                   [codec-id codec-id]
                   [pred-internal pred-internal]
                   [pred-id pred-id]
                   [(all-fld ...) all-fields]
                   [(pub-acc ...) public-accessors]
                   [(int-acc ...) internal-accessors]
                   [(rev-pub-acc ...) (reverse public-accessors)]
                   [(mut-false ...) (for/list ([_ (in-list all-fields)]) #'#f)]
                   [(kw ...) all-keywords]
                   [(def-expr ...) (for/list ([d (in-list all-defaults)])
                                     (if d #`#'#,d #'#f))]
                   [(field-codec-def ...) (build-field-codec-defs field-infos)]
                   [(arg ...) arg-vars]
                   [(ctor-check ...) (build-ctor-checks #'name field-infos rest-field-stx rest-arg-var)]
                   [serializer (build-serializer-expr #'self field-infos rest-accessor)]
                   [try-body (build-try-decode-expr #'js field-infos try-construct-expr)]
                   [(strict-expr ...) strict-all-exprs]
                   [decode-match-def (gen-decode-match-expander decode-match-id #'name)]
                   [exports-def
                    (gen-exports exports-id
                                 (append (list struct-type-public #'name pred-id
                                               decode-match-id)
                                         public-accessors))])
       #'(begin
           (struct internal-id (all-fld ...)
             #:transparent
             #:reflection-name 'name
             #:methods gen:jsexpr-struct
             [(define (struct->jsexpr self)
                serializer)])

           (define struct-type-public struct-type-internal)
           (define pred-id pred-internal)
           (define pub-acc int-acc) ...

           field-codec-def ...

           (define ctor-id
             (procedure-rename
               (lambda (arg ...)
                 ctor-check ...
                 (internal-id arg ...))
               'name))

           (define (decode-fn js strict?)
             (define result
               (if (hash? js)
                   try-body
                   decode-failed))
             (cond
               [(not (decode-failed? result)) result]
               [(not strict?) decode-failed]
               [else
                (unless (hash? js)
                  (raise-decode-error 'jsexpr-> 'name js))
                (let ([arg strict-expr] ...)
                  (internal-id arg ...))]))

           decode-match-def

           (define codec-id
             (json-codec 'name pred-id decode-fn))
           (define val-id (ctor-with-codec ctor-id codec-id))

           (define-syntax name
             (json-struct-transformer
               (list #'struct-type-public
                     #'name
                     #'pred-id
                     (list #'rev-pub-acc ...)
                     (list mut-false ...)
                     #t)
               (lambda (inner-stx)
                 (syntax-parse inner-stx
                   [(_ (~seq k:keyword p) (... ...+))
                    (expand-struct-kw-match inner-stx
                                            #'internal-id
                                            (list #'kw ...)
                                            (list #'all-fld ...)
                                            (syntax->list #'(k (... ...)))
                                            (syntax->list #'(p (... ...))))]
                   [(_ p (... ...))
                    #:fail-when (any-keyword? #'(p (... ...)))
                    "mixed or malformed keyword arguments are not allowed in struct patterns"
                    (expand-struct-pos-match inner-stx
                                             #'internal-id
                                             (list #'all-fld ...)
                                             (syntax->list #'(p (... ...))))]))
               (lambda (inner-stx)
                 (syntax-parse inner-stx
                   [(_ (~seq k:keyword v) (... ...))
                    (expand-kw-ctor-call inner-stx
                                         #'ctor-id
                                         (list #'kw ...)
                                         (list def-expr ...)
                                         (syntax->list #'(k (... ...)))
                                         (syntax->list #'(v (... ...))))]
                   [(_ a1 a (... ...))
                    #:fail-when (any-keyword? #'(a1 a (... ...)))
                    "mixed or malformed keyword arguments are not allowed"
                    (syntax/loc inner-stx (ctor-id a1 a (... ...)))]
                   [_:id (syntax/loc inner-stx val-id)]))))

           exports-def))]))

;; ============================================================================
;; define-json-enum
;; ============================================================================

(define-syntax (define-json-enum stx)
  (syntax-parse stx
    [(_ name:id [sym:id val:expr] ...+)
     (define symbols (syntax->list #'(sym ...)))
     (define values-stx (syntax->list #'(val ...)))
     (ensure-no-duplicates 'define-json-enum "variant" symbols)
     (for ([s (in-list symbols)])
       (when (eq? (syntax-e s) 'v)
         (raise-syntax-error 'define-json-enum
                             "variant name `v` is reserved for the enum value accessor"
                             s)))
     (ensure-no-duplicate-datums 'define-json-enum "JSON value" values-stx)

     (define internal-id (format-id #'name "~a~~" #'name))
     (define pred-internal (format-id #'name "~a~~?" #'name))
     (define pred-id (format-id #'name "~a?" #'name))
     (define acc-internal (format-id #'name "~a~~-v" #'name))
     (define acc-id (format-id #'name "~a-v" #'name))
     (define ctor-id (format-id #'name "~a~~ctor" #'name))
     (define val-id (format-id #'name "~a~~val" #'name))
     (define codec-id (format-id #'name "~a~~codec" #'name))
     (define decode-match-id (format-id #'name "^~a" #'name))
     (define exports-id (format-id #'name "~a~~exports" #'name))
     (define aliases
       (for/list ([s (in-list symbols)])
         (format-id #'name "~a-~a" #'name s)))
     (define alias-defs
       (for/list ([alias (in-list aliases)]
                  [s (in-list symbols)])
         #`(define #,alias (#,internal-id '#,s))))
     (define val-vars (generate-temporaries symbols))
     (define kv-pairs
       (apply append
              (for/list ([vv (in-list val-vars)]
                         [a (in-list aliases)])
                (list vv a))))

     (with-syntax ([internal-id internal-id]
                   [pred-internal pred-internal]
                   [pred-id pred-id]
                   [acc-internal acc-internal]
                   [acc-id acc-id]
                   [ctor-id ctor-id]
                   [val-id val-id]
                   [codec-id codec-id]
                   [variant-count (length aliases)]
                   [(alias ...) aliases]
                   [(alias-def ...) alias-defs]
                   [(val-var ...) val-vars]
                   [(kv ...) kv-pairs]
                   [decode-match-def (gen-decode-match-expander decode-match-id #'name)]
                   [exports-def
                    (gen-exports exports-id
                                 (append (list #'name pred-id acc-id decode-match-id)
                                         aliases))])
       #'(begin
           (define val-var val) ...

           (struct internal-id (v)
             #:transparent
             #:reflection-name 'name
             #:methods gen:jsexpr-struct
             [(define (struct->jsexpr self)
                (case (acc-internal self)
                  [(sym) val-var] ...))])

           (define pred-id pred-internal)
           (define acc-id acc-internal)
           alias-def ...

           (define ctor-id
             (procedure-rename
               (lambda (tag)
                 (case tag
                   [(sym) alias] ...
                   [else (raise-arguments-error 'name
                                                "invalid enum variant"
                                                "type" 'name
                                                "given" tag)]))
               'name))

           (define json->alias (hash kv ...))
           (unless (= (hash-count json->alias) variant-count)
             (error 'define-json-enum "duplicate JSON value in enum ~a" 'name))

           (define (decode-fn x strict?)
             (define v (hash-ref json->alias x decode-failed))
             (if (and (decode-failed? v) strict?)
                 (raise-decode-error 'jsexpr-> 'name x)
                 v))

           decode-match-def

           (define codec-id
             (json-codec 'name pred-id decode-fn))
           (define val-id (ctor-with-codec ctor-id codec-id))

           (define-syntax name
             (json-enum-transformer
               (lambda (inner-stx)
                 (syntax-parse inner-stx
                   [(_ pat) #'(internal-id pat)]))
               (lambda (inner-stx)
                 (syntax-parse inner-stx
                   [(_ tag-expr) (syntax/loc inner-stx (ctor-id tag-expr))]
                   [_:id (syntax/loc inner-stx val-id)]))))

           exports-def))]))

;; ============================================================================
;; define-json-union
;; ============================================================================

(define-syntax (define-json-union stx)
  (syntax-parse stx
    [(_ name:id variant:type-spec ...+)
     (define pred-id (format-id #'name "~a?" #'name))
     (define exports-id (format-id #'name "~a~~exports" #'name))
     (with-syntax ([pred-id pred-id]
                   [exports-def
                    (gen-exports exports-id (list #'name pred-id))])
       #'(begin
           (define name (union-codec variant.lazy-codec-expr ...))
           (define pred-id (json-codec-pred? name))
           exports-def))]))
