#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/set
                     racket/syntax
                     syntax/parse
                     racket/provide-transform
                     racket/bool)
         json
         racket/generic
         racket/match
         racket/contract
         syntax/parse)

;; define-json-expander — generates a match expander for mutable hasheq.
;; (Legacy: being replaced by define-json-struct)
(define-syntax (define-json-expander stx)
  (syntax-parse stx
    [(_ name:id [key:id ctc:expr] ...+)
     (with-syntax ([(key_ ...) (generate-temporaries #'(key ...))]
                   [(keyword ...)
                    (for/list ([k (syntax->datum #'(key ...))])
                      (symbol->keyword k))]
                   [~?-id (quote-syntax ~?)])
       (syntax/loc stx
         (define-match-expander name
           (λ (stx)
             (syntax-parse stx
               [(_ (~optional (~seq keyword key_)) ...)
                (quasisyntax/loc stx (hash-table (~?-id ['key (? ctc key_)]) ...))]))
           (λ (stx)
             (syntax-parse stx
               [(_ (~optional (~seq keyword key_)) ...)
                (syntax/loc stx
                  (make-hasheq (list (cons 'key key_) ...)))])))))]))

;; jsexpr utilities — helpers for manipulating nested JS-expression hashes.
(define (jsexpr-has-key? jsexpr keys)
  (cond [(null? keys) #t]
        [else (and (hash-has-key? jsexpr (car keys))
                   (jsexpr-has-key? (hash-ref jsexpr (car keys)) (cdr keys)))]))

(define (jsexpr-ref jsexpr keys)
  (cond [(null? keys) jsexpr]
        [else (jsexpr-ref (hash-ref jsexpr (car keys)) (cdr keys))]))

(define (jsexpr-set jsexpr keys v)
  (cond [(null? keys) jsexpr]
        [(null? (cdr keys)) (hash-set jsexpr (car keys) v)]
        [else (hash-set jsexpr (car keys)
                        (jsexpr-set (hash-ref jsexpr (car keys)) (cdr keys) v))]))

(define (jsexpr-remove jsexpr keys)
  (cond [(null? keys) jsexpr]
        [(null? (cdr keys)) (hash-remove jsexpr (car keys))]
        [else (hash-set jsexpr (car keys)
                        (jsexpr-remove (hash-ref jsexpr (car keys)) (cdr keys)))]))

;; gen:jsexpr-struct — generic interface for JSON-serializable structs
(define-generics jsexpr-struct
  (struct->jsexpr jsexpr-struct))

;; Nothing — singleton for absent/optional JSON fields.
;; Encodes to nothing (omitted from hashes).  Decodes from missing keys.
(struct Nothing ()
  #:transparent)

;; (optional/c pred?) — contract that accepts Nothing or values satisfying pred?.
(define (optional/c pred?)
  (or/c Nothing? pred?))

;; Recursively converts data (structs, hashes, lists) into JSON-compatible immutable hashes.
;; Nothing values are omitted from hash output.
(define (->jsexpr v)
  (cond
    [(Nothing? v) (json-null)]
    [(jsexpr-struct? v) (struct->jsexpr v)]
    [(hash? v)
     (for/hasheq ([(k val) (in-hash v)]
                  #:unless (Nothing? val))
       (values k (->jsexpr val)))]
    [(list? v) (map ->jsexpr v)]
    [else v]))

;; define-json-struct — generates a transparent struct with JSON support,
;; keyword constructors, and split pattern matching.
(begin-for-syntax
  (define (symbol->keyword sym)
    (string->keyword (symbol->string sym)))

  (define (id-bound? id)
    (and (identifier? id)
         (not (false? (identifier-binding id)))))

  (define ((name-id fmt) name-stx)
    (format-id name-stx fmt name-stx))

  (define ((name+field-id fmt) name-stx field-stx)
    (format-id name-stx fmt name-stx field-stx))

  (define name->pred-id (name-id "~a?"))
  (define name->js-pred-id (name-id "~a-js?"))
  (define name->exports-id (name-id "~a-exports"))
  (define name->decoder-id (name-id "jsexpr->~a"))
  (define name->js-match-id (name-id "~a-js"))
  (define name->as-match-id (name-id "as-~a"))
  (define name->decode-match-id (name-id "^~a"))
  (define name->try-decoder-id (name-id "try-jsexpr->~a"))
  (define json-struct-internal-id (name-id "~a~~"))
  (define json-struct-keyword-constructor-id (name-id "~a~~kw"))
  (define json-struct-predicate-internal-id (name-id "~a~~?"))

  (define json-struct-accessor-id (name+field-id "~a~~-~a"))
  (define json-public-accessor-id (name+field-id "~a-~a"))

  (define (decodable-type-id? id)
    (id-bound? (name->decoder-id id)))

  (define (type-id-ctc id)
    (match* ((decodable-type-id? id)
             (id-bound? (name->pred-id id)))
      [(#t #t) (name->pred-id id)]
      ;; Decodable types must provide a runtime predicate.
      [(#t #f)
       (raise-syntax-error
         'type-spec
         (format "decodable type ~a is missing a runtime predicate ~a"
                 (syntax->datum id)
                 (name->pred-id id))
         id)]
      ;; Non-decodable ids are usually contracts/predicates.
      [(#f _) id]))

  (define (type-id-decoder id)
    (if (decodable-type-id? id)
        (name->decoder-id id)
        #'values))

  (define (type-id-json-predicate id)
    (match* ((decodable-type-id? id)
             (id-bound? (name->js-pred-id id)))
      [(#t #t) (name->js-pred-id id)]
      ;; Missing JSON predicate but provided a decoder: skip JSON validation.
      [(#t #f) #'(lambda (_) #t)]
      [(#f _) id]))

  ;; type-spec normalizes the DSL type annotation into four compile-time
  ;; attributes used by define-json-struct expansion:
  ;; - ctc: contract for the generated struct field
  ;; - decoder: function to decode JSON values into Racket values
  ;; - json-pred: predicate used by the generated `*-js?` validator
  ;; - optional?: whether the field can be omitted (for optional ...)
  (define-syntax-class type-spec
    #:attributes (ctc decoder json-pred optional?)
    #:datum-literals (listof optional hash/c or/c)
    ;; Bare identifier type. If a matching decoder exists (jsexpr->T),
    ;; treat it as a decodable domain type; otherwise use the identifier
    ;; directly and leave decoding as identity.
    (pattern tid:id
             #:with ctc (type-id-ctc #'tid)
             #:with decoder (type-id-decoder #'tid)
             #:with json-pred (type-id-json-predicate #'tid)
             #:attr optional? #f)
    ;; Homogeneous lists recursively decode/validate each element.
    (pattern (listof elem:type-spec)
             #:with ctc #'(listof elem.ctc)
             #:with decoder #'(lambda (xs) (map elem.decoder xs))
             #:with json-pred #'(lambda (xs)
                                  (and (list? xs)
                                       (andmap elem.json-pred xs)))
             #:attr optional? #f)
    ;; Optional values propagate Nothing unchanged and decode present values.
    (pattern (optional elem:type-spec)
             #:with ctc #'(optional/c elem.ctc)
             #:with decoder #'(lambda (x)
                                (if (Nothing? x)
                                    x
                                    (elem.decoder x)))
             #:with json-pred #'(optional/c elem.json-pred)
             #:attr optional? #t)
    (pattern (hash/c key:type-spec val:type-spec)
             #:with ctc #'(hash/c key.ctc val.ctc)
             #:with decoder #'(lambda (h)
                                (for/hasheq ([(k v) h])
                                  (values (key.decoder k)
                                          (val.decoder v))))
             #:with json-pred #'(hash/c key.json-pred val.json-pred)
             #:attr optional? #f)
    (pattern (or/c opt:type-spec ...+)
             #:with ctc #'(or/c opt.ctc ...)
             #:with decoder #'(lambda (x)
                                (let/ec return
                                  (with-handlers ([exn:fail? void])
                                    (define decoded (opt.decoder x))
                                    (when (opt.ctc decoded)
                                      (return decoded)))
                                  ...
                                  (error 'or/c "no variant matched value: ~v" x)))
             #:with json-pred #'(or/c opt.json-pred ...)
             #:attr optional? #f)
    (pattern other:expr
             #:with ctc #'other
             #:with decoder #'values
             #:with json-pred #'other
             #:attr optional? #f))

  ;; Syntax class to parse a field clause.
  ;; Supports:
  ;;   - [field type-spec]            -> json key is same as field name
  ;;   - [field type-spec #:json key] -> custom json key
  (define-syntax-class json-struct-clause
    #:attributes (field type-pred type-decoder type-json-pred optional? json-key keyword)
    (pattern [field:id ts:type-spec]
             #:with json-key #'field
             #:with keyword (symbol->keyword (syntax-e #'field))
             #:with type-pred #'ts.ctc
             #:with type-decoder #'ts.decoder
             #:with type-json-pred #'ts.json-pred
             #:attr optional? (attribute ts.optional?))
    (pattern [field:id ts:type-spec #:json json-key:id]
             #:with keyword (symbol->keyword (syntax-e #'field))
             #:with type-pred #'ts.ctc
             #:with type-decoder #'ts.decoder
             #:with type-json-pred #'ts.json-pred
             #:attr optional? (attribute ts.optional?)))

  (define (any-keyword? stx)
    (for/or ([x (in-list (syntax->list stx))])
      (keyword? (syntax-e x))))

  (define (ensure-no-duplicate-keywords who k-list)
    (define seen (mutable-seteq))
    (for ([k k-list])
      (define key (syntax-e k))
      (when (set-member? seen key)
        (raise-syntax-error who
                            (format "duplicate keyword argument ~a" key)
                            k))
      (set-add! seen key)))

  ;; Generates the internal struct definition with JSON serialization method and contracts.
  (define (gen-contracted-struct stx iname fields contracts json-keys accessors)
    (with-syntax ([iname iname]
                  [(fld ...) fields]
                  [(ctc ...) contracts]
                  [(jk ...) json-keys]
                  [(acc ...) accessors])
      #'(struct iname (fld ...)
          #:transparent
          #:guard (struct-guard/c ctc ...)
          #:methods gen:jsexpr-struct
          [(define (struct->jsexpr self)
             (for/hasheq ([k (list 'jk ...)]
                          [v (list (acc self) ...)]
                          #:unless (Nothing? v))
               (values k (->jsexpr v))))])))

  ;; Public aliases for the accessors.
  (define (gen-public-accessors public-accessors accessors)
    (for/list ([pub public-accessors]
               [acc accessors])
      #`(define #,pub #,acc)))

  ;; Generates Name-js? predicate.
  ;; Checks JSON hash shape and validates each field contract.
  (define (gen-json-predicate stx js-pred-name json-keys json-preds optional-flags)
    (define checks
      (for/list ([jk json-keys]
                 [jp json-preds]
                 [opt? optional-flags])
        (if opt?
            #`(or (not (hash-has-key? x '#,jk))
                  (#,jp (hash-ref x '#,jk)))
            #`(and (hash-has-key? x '#,jk)
                   (#,jp (hash-ref x '#,jk))))))
    (with-syntax ([js-pred-name js-pred-name]
                  [(check ...) checks])
      #'(define (js-pred-name x)
          (and (hash? x)
               check
               ...))))

  ;; Generates jsexpr->Name decoder.
  ;; Extracts each field from a JSON hash by its JSON key and constructs
  ;; the struct via the keyword constructor.  The struct guard validates.
  (define (gen-json-decoder stx decoder-name keyword-constructor-id json-keys keywords decoders)
    (define input-id (datum->syntax stx 'js))
    (define field-values
      (for/list ([jk json-keys]
                 [dec decoders])
        #`(let ([v (hash-ref #,input-id '#,jk (Nothing))])
            (if (Nothing? v) v (#,dec v)))))
    (with-syntax ([decoder-name decoder-name]
                  [input input-id]
                  [keyword-constructor keyword-constructor-id]
                  [(kw ...) keywords]
                  [(fv ...) field-values])
      #'(define (decoder-name input)
          (unless (hash? input)
            (error 'decoder-name "expected hash, got ~v" input))
          (keyword-constructor (~@ kw fv) ...))))

  ;; Generates as-Name / ^Name match expanders and helper decoder wrapper.
  ;; as-Name matches JSON hashes and binds decoded structs.
  ;; as-Name is strict, Name-js is loose: allow some fields not exists, while as-Name
  ;; always build a Name struct instance.
  ;; ^Name aliases (as-Name (Name ...)).
  (define (gen-as-match-expanders stx as-name decode-name try-decoder decoder-name name)
    (with-syntax ([as-name as-name]
                  [decode-name decode-name]
                  [try-decoder try-decoder]
                  [decoder-name decoder-name]
                  [name name])
      #'(begin
          (define (try-decoder x)
            (with-handlers ([exn:fail? (lambda (_) (Nothing))])
              (decoder-name x)))
          (define-match-expander as-name
            (lambda (inner-stx)
              (syntax-parse inner-stx
                [(_ name)
                 #'(and (? hash?)
                        (app try-decoder (and name (not (? Nothing?)))))])))
          (define-match-expander decode-name
            (lambda (inner-stx)
              (syntax-parse inner-stx
                [(_ p (... ...))
                 #'(as-name (name p (... ...)))]))))))

  ;; Generates the export bundle macro.
  (define (gen-exports name-exports name pred public-accessors name-js name-js-pred decoder-name as-name decode-name)
    (with-syntax ([name-exports name-exports]
                  [name name]
                  [pred pred]
                  [(pub ...) public-accessors]
                  [name-js name-js]
                  [name-js-pred name-js-pred]
                  [decoder decoder-name]
                  [as-name as-name]
                  [decode-name decode-name])
      #'(define-syntax name-exports
          (make-provide-transformer
            (lambda (stx modes)
              (expand-export
                #'(combine-out name pred pub ... name-js name-js-pred decoder as-name decode-name)
                modes))))))

  ;; Generates the public match expander.
  ;; 1. Patterns: matches internal struct.
  ;; 2. Expressions: constructor with contract checks.

  ;; keyword-argument constructor with contract enforcement.
  (define (gen-kw-constructor keyword-constructor-id iname fields keywords contracts struct-pred)
    (with-syntax ([keyword-constructor keyword-constructor-id]
                  [iname iname]
                  [struct-pred struct-pred]
                  [(kw ...) keywords]
                  [(fld ...) fields]
                  [(ctc ...) contracts])
      #'(define/contract (keyword-constructor (~@ kw fld) ...)
          (-> (~@ kw ctc) ... struct-pred)
          (iname fld ...))))

  ;; Generates the match pattern for keyword-based matching.
  ;; Supports both internal structs (via struct*) and hashes (via hash-table).
  ;; Mode determines output: 'struct (struct pattern) or 'json (hash pattern).
  (define (gen-match-kw-pattern stx k-list v-list keywords fields json-keys iname mode)
    (ensure-no-duplicate-keywords 'define-json-struct k-list)

    (define keyword->idx
      (for/hasheq ([kw keywords]
                   [i (in-naturals)])
        (values (syntax-e kw) i)))

    (define (unknown-keyword-failure k)
      (lambda ()
        (raise-syntax-error 'define-json-struct
                            (format "unknown keyword ~a" (syntax-e k))
                            k)))

    (define-values (s-pats h-pats)
      (for/lists (s h)
                 ([k k-list]
                  [v v-list])
        (define idx (hash-ref keyword->idx (syntax-e k) (unknown-keyword-failure k)))
        (define fld (list-ref fields idx))
        (define jk (list-ref json-keys idx))
        (with-syntax ([fld fld]
                      [v v]
                      [jk jk])
          (values #'[fld v] #'['jk v]))))

    (if (eq? mode 'struct)
        #`(struct* #,iname (#,@s-pats))
        #`(hash-table #,@h-pats)))

  ;; Generates the match pattern for positional matching.
  ;; Enforces full arity.
  ;; Mode determines output: 'struct (struct pattern) or 'json (hash pattern).
  (define (gen-match-pos-pattern stx v-list fields json-keys iname mode)
    (unless (= (length v-list) (length fields))
      (raise-syntax-error
        'define-json-struct
        (format "wrong number of arguments for positional pattern (expected ~a, given ~a)"
                (length fields) (length v-list))
        stx))

    (with-syntax ([iname iname]
                  [(v ...) v-list]
                  [(jk ...) json-keys])
      (cond [(eq? mode 'struct)
             #'(iname v ...)]
            [else
             #'(hash-table ['jk v] ...)])))

  ;; Generates the public Name match/constructor wrapper.
  ;; Pattern mode matches struct values; expression mode constructs structs.
  (define (gen-struct-match-expander stx name iname keyword-constructor-id keywords fields json-keys)
    (with-syntax ([iname iname]
                  [keyword-constructor keyword-constructor-id]
                  [name name]
                  [(kw ...) keywords]
                  [(fld ...) fields]
                  [(jk ...) json-keys])
      #`(define-match-expander name
          ;; Pattern transformer
          (λ (inner-stx)
            (syntax-parse inner-stx
              ;; Keyword pattern: (Name #:field1 val1 ...)
              [(_ (~seq k:keyword v) (... ...+))
               (gen-match-kw-pattern inner-stx (syntax->list #'(k (... ...))) (syntax->list #'(v (... ...)))
                                     (list #'kw ...) (list #'fld ...) (list #'jk ...)
                                     #'iname 'struct)]
              ;; Positional pattern: (Name val1 ...)
              ;; Dispatch to internal struct for pattern matching
              [(_ v (... ...))
               (gen-match-pos-pattern inner-stx (syntax->list #'(v (... ...)))
                                      (list #'fld ...) (list #'jk ...)
                                      #'iname 'struct)]))
          ;; Expression transformer (constructor)
          (λ (inner-stx)
            (syntax-parse inner-stx
              [(_ (~seq k:keyword v) (... ...+))
               (begin
                 (define k-list (syntax->list #'(k (... ...))))
                 (define v-list (syntax->list #'(v (... ...))))
                 (ensure-no-duplicate-keywords 'define-json-struct k-list)
                 (define kv-args
                   (append*
                     (for/list ([kw-arg k-list] [val-arg v-list])
                       (list kw-arg val-arg))))
                 (with-syntax ([(arg (... ...)) kv-args])
                   (syntax/loc inner-stx
                     (keyword-constructor arg (... ...)))))]
              [(_ a (... ...))
               #:fail-when (any-keyword? #'(a (... ...)))
               "mixed positional and keyword arguments are not allowed"
               (syntax/loc inner-stx (iname a (... ...)))])))))

  ;; Generates the Name-js match expander (matches JSON hashes only).
  (define (gen-json-match-expander stx name-js keywords fields json-keys)
    (with-syntax ([name-js name-js]
                  [(kw ...) keywords]
                  [(fld ...) fields]
                  [(jk ...) json-keys])
      #`(define-match-expander name-js
          ;; Pattern transformer
          (λ (inner-stx)
            (syntax-parse inner-stx
              ;; Keyword pattern: (Name-js #:field1 val1 ...)
              [(_ (~seq k:keyword v) (... ...+))
               (gen-match-kw-pattern inner-stx (syntax->list #'(k (... ...))) (syntax->list #'(v (... ...)))
                                     (list #'kw ...) (list #'fld ...) (list #'jk ...)
                                     #f 'json)]
              ;; Positional pattern: (Name-js val1 ...)
              [(_ v (... ...))
               (gen-match-pos-pattern inner-stx (syntax->list #'(v (... ...)))
                                      (list #'fld ...) (list #'jk ...)
                                      #f 'json)]))))))


;; define-json-struct
;;
;; Defines a data type with:
;; - an internal transparent struct (with field contracts),
;; - a public constructor/match expander `Name`,
;; - JSON helpers: `Name-js`, `Name-js?`, `jsexpr->Name`, `as-Name`, and `^Name`.
;;
;; Clause syntax:
;;   (define-json-struct Name
;;     [field type-spec]                  ; JSON key defaults to `field`
;;     [field type-spec #:json json-key]) ; custom JSON key
;;
;; `Name` in expression position:
;; - (Name v1 v2 ...)
;; - (Name #:f1 v1 #:f2 v2 ...) ; keyword order independent
;;
;; `Name` in match position:
;; - (Name p1 p2 ...)            ; full positional match
;; - (Name #:f1 p1 ...)          ; partial keyword match
;;
;; `Name-js` matches JSON hash shapes; `Name` matches struct instances.
;; `jsexpr->Name` is strict (raises on invalid input).
;; `as-Name` is tolerant in match contexts (failed decode => `Nothing`).
;; `^Name` aliases `(as-Name (Name ...))`.
(define-syntax (define-json-struct stx)
  (syntax-parse stx
    [(_ name:id clause:json-struct-clause ...+)
     ;; Extract normalized per-field metadata from `json-struct-clause`.
     (define fields (syntax->list #'(clause.field ...)))
     (define contracts (syntax->list #'(clause.type-pred ...)))
     (define decoders (syntax->list #'(clause.type-decoder ...)))
     (define json-preds (syntax->list #'(clause.type-json-pred ...)))
     (define optional-flags (attribute clause.optional?))
     (define json-keys (syntax->list #'(clause.json-key ...)))
     (define keywords (syntax->list #'(clause.keyword ...)))

     ;; Generate identifiers once and reuse to avoid drift between definitions.
     (define iname (json-struct-internal-id #'name))
     (define keyword-constructor-id (json-struct-keyword-constructor-id #'name))
     (define accessors
       (for/list ([f fields])
         (json-struct-accessor-id #'name f)))
     (define public-accessors
       (for/list ([f fields])
         (json-public-accessor-id #'name f)))
     (define struct-pred-internal (json-struct-predicate-internal-id #'name))
     (define pred (name->pred-id #'name))
     (define name-js-pred (name->js-pred-id #'name))
     (define name-js (name->js-match-id #'name))
     (define as-name (name->as-match-id #'name))
     (define decode-name (name->decode-match-id #'name))
     (define try-decoder (name->try-decoder-id #'name))
     (define name-exports (name->exports-id #'name))
     (define decoder-name (name->decoder-id #'name))

     ;; Splice all generated pieces into one expansion unit.
     (datum->syntax
       stx
       `(begin
          ,(gen-contracted-struct stx iname fields contracts json-keys accessors)
          ,(gen-kw-constructor keyword-constructor-id iname fields keywords contracts struct-pred-internal)
          ,@(gen-public-accessors public-accessors accessors)
          (define ,pred ,struct-pred-internal)

          ,(gen-struct-match-expander stx #'name iname keyword-constructor-id keywords fields json-keys)
          ,(gen-json-match-expander stx name-js keywords fields json-keys)
          ,(gen-json-predicate stx name-js-pred json-keys json-preds optional-flags)
          ,(gen-json-decoder stx decoder-name keyword-constructor-id json-keys keywords decoders)
          ,(gen-as-match-expanders stx as-name decode-name try-decoder decoder-name #'name)

          ,(gen-exports name-exports #'name pred public-accessors name-js name-js-pred decoder-name as-name decode-name))
       stx)]))

(define-syntax json-type-out
  (make-provide-transformer
    (lambda (stx modes)
      (syntax-parse stx
        [(_ name:id ...)
         (with-syntax ([(export-macro-call ...)
                        (for/list ([n (in-list (syntax->list #'(name ...)))])
                          #`(#,(format-id n "~a-exports" n)))])
           (expand-export
             #'(combine-out export-macro-call ...)
             modes))]))))

;; define-json-enum — Generates an enum type that maps Racket symbols to JSON values.
;; Wraps symbols in a transparent struct with gen:jsexpr-struct encoding.
;;
;; Syntax:
;;   (define-json-enum Name [sym json-val] ...)
;;
;; Generated:
;;   Name          — struct (Name v), constructor & predicate
;;   jsexpr->Name  — JSON value → Name struct
;;   Name-js?      — JSON-side predicate
;;   Name-js       — match expander for JSON values
;;   as-Name       — tolerant match expander via decoder (failure => Nothing)
;;   ^Name         — alias for `(as-Name (Name ...))`
;;   Name-exports  — provide transformer
(define-syntax (define-json-enum stx)
  (syntax-parse stx
    [(_ name:id [sym:id val:expr] ...+)
     (define name-js? (name->js-pred-id #'name))
     (define exports (name->exports-id #'name))
     (define jsexpr->name (name->decoder-id #'name))
     (define name-js (name->js-match-id #'name))
     (define as-name (name->as-match-id #'name))
     (define decode-name (name->decode-match-id #'name))
     (define try-decoder (name->try-decoder-id #'name))
     (define symbols (syntax->list #'(sym ...)))
     (define aliases
       (for/list ([s (in-list symbols)])
         (format-id #'name "~a-~a" #'name s)))
     (define alias-defs
       (for/list ([alias (in-list aliases)]
                  [symbol-id (in-list symbols)])
         #`(define #,alias (name '#,symbol-id))))
     (with-syntax ([name-js? name-js?]
                   [exports exports]
                   [jsexpr->name jsexpr->name]
                   [name-js name-js]
                   [as-name as-name]
                   [decode-name decode-name]
                   [try-decoder try-decoder]
                   [(alias ...) aliases]
                   [(alias-def ...) alias-defs])
       #'(begin
           (struct name (v)
             #:transparent
             #:guard
             (λ (tag _struct-name)
               (unless (or (eq? tag 'sym) ...)
                 (error 'name "invalid ~a variant: ~v" 'name tag))
               tag)
             #:methods gen:jsexpr-struct
             [(define (struct->jsexpr self)
                (match self
                  [(name 'sym) val] ...))])

           alias-def ...

           (define (name-js? x)
             (or (equal? x val) ...))

           (define (jsexpr->name x)
             (cond
               [(equal? x val) (name 'sym)] ...
               [else (error 'jsexpr->name "invalid ~a JSON value: ~v" 'name x)]))

           (define-match-expander name-js
             (λ (inner-stx)
               (syntax-parse inner-stx
                 [(_ p)
                  #'(app jsexpr->name (name p))])))

           (define (try-decoder x)
             (with-handlers ([exn:fail? (lambda (_) (Nothing))])
               (jsexpr->name x)))
           (define-match-expander as-name
             (lambda (inner-stx)
               (syntax-parse inner-stx
                 [(_ p)
                  #'(app try-decoder (and p (not (? Nothing?))))])))
           (define-match-expander decode-name
             (lambda (inner-stx)
               (syntax-parse inner-stx
                 [(_ p (... ...))
                  #'(as-name (name p (... ...)))])))

           (define-syntax exports
             (make-provide-transformer
               (lambda (inner-stx modes)
                 (expand-export
                   #'(combine-out name (struct-out name) alias ... name-js? jsexpr->name name-js as-name decode-name)
                   modes))))))]))

;; define-json-union — Generates an untagged union type.
;; No wrapper struct; this is a named alias around an `(or/c ...)` type-spec.
;;
;; Syntax:
;;   (define-json-union Name type-spec ...)
;;
;; Generated:
;;   Name?         — runtime predicate
;;   jsexpr->Name  — JSON decoder (from combined `(or/c ...)` type-spec)
;;   Name-js?      — JSON-side predicate
;;   Name-js       — match expander for JSON values
;;   as-Name       — tolerant match expander via decoder (failure => Nothing)
;;   ^Name         — alias for `(as-Name ...)`
;;   Name-exports  — provide transformer
(define-syntax (define-json-union stx)
  (syntax-parse stx
    [(_ name:id variant:type-spec ...+)
     (define name-pred (name->pred-id #'name))
     (define name-js-pred (name->js-pred-id #'name))
     (define name-exports (name->exports-id #'name))
     (define jsexpr->name (name->decoder-id #'name))
     (define name-js (name->js-match-id #'name))
     (define as-name (name->as-match-id #'name))
     (define decode-name (name->decode-match-id #'name))
     (define try-decoder (name->try-decoder-id #'name))

     ;; Collapse the union variants into one `(or/c ...)` type-spec and reuse
     ;; its normalized predicate/decoder/json-predicate attributes.
     (syntax-parse #'(or/c variant ...)
       [ts:type-spec
        (with-syntax ([pred name-pred]
                      [js-pred name-js-pred]
                      [exports name-exports]
                      [js->name jsexpr->name]
                      [js-match name-js]
                      [as-name as-name]
                      [decode-name decode-name]
                      [try-decoder try-decoder]
                      [runtime-pred #'ts.ctc]
                      [decoder #'ts.decoder]
                      [json-pred #'ts.json-pred])
          #'(begin
              (define (pred x)
                (runtime-pred x))
              (define (js-pred x)
                (json-pred x))
              (define (js->name x)
                (decoder x))
              (define-match-expander js-match
                (λ (inner-stx)
                  (syntax-parse inner-stx
                    [(_ p)
                     #'(app js->name p)])))
              (define (try-decoder x)
                (with-handlers ([exn:fail? (lambda (_) (Nothing))])
                  (js->name x)))
              (define-match-expander as-name
                (lambda (inner-stx)
                  (syntax-parse inner-stx
                    [(_ p)
                     #'(app try-decoder (and p (not (? Nothing?))))])))
              (define-match-expander decode-name
                (lambda (inner-stx)
                  (syntax-parse inner-stx
                    [(_ p)
                     #'(as-name p)])))
              (define-syntax exports
                (make-provide-transformer
                  (lambda (inner-stx modes)
                    (expand-export
                      #'(combine-out pred js-pred js->name js-match as-name decode-name)
                      modes))))))])]))

(provide define-json-expander
         define-json-struct
         define-json-enum
         define-json-union
         json-type-out
         Nothing
         Nothing?
         optional/c
         gen:jsexpr-struct
         jsexpr-struct?
         struct->jsexpr
         ->jsexpr
         jsexpr-has-key?
         jsexpr-ref
         jsexpr-set
         jsexpr-remove)

