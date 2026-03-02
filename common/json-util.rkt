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

;; ============================================================================
;; JSON type macros — reference
;; ============================================================================
;;
;; Three macros for declaring protocol-facing data types with automatic
;; JSON encoding, decoding, contracts, and pattern matching:
;;
;;   define-json-struct  — structured objects with named fields.
;;   define-json-enum    — tagged enum: symbolic variants <-> jsexpr.
;;   define-json-union   — untagged union: alias over alternative type specs.
;;
;; Enums are tagged: each value is wrapped in a `(Name 'variant)` struct with
;; a stable symbolic tag.  Unions are untagged: no wrapper struct exists, and
;; decoding tries each alternative in order until one succeeds.
;;
;; ----------------------------------------------------------------------------
;; 1. Syntax
;; ----------------------------------------------------------------------------
;;
;;   (define-json-struct Name
;;     [field type-spec]
;;     [field type-spec #:json json-key] ...)
;;
;;   (define-json-enum Name
;;     [variant jsexpr] ...)
;;
;;   (define-json-union Name type-spec ...)
;;
;; ----------------------------------------------------------------------------
;; 2. type-spec DSL
;; ----------------------------------------------------------------------------
;;
;; Used in struct field declarations and union variant lists.
;;
;;   identifier          decodable type if jsexpr->T is bound; otherwise contract
;;   (listof T)          homogeneous list, each element decoded via T
;;   (optional T)        T | Nothing; omitted JSON keys map to Nothing
;;   (hash/c K V)        hash map, K and V are type-specs (see above)
;;   (or/c T1 T2 ...)    first successful decode wins
;;   <expr>              arbitrary contract/predicate expression
;;
;; When an identifier Name has a jsexpr->Name decoder in scope, the macro
;; uses it for recursive decoding and picks Name? as the runtime contract.
;;
;; ----------------------------------------------------------------------------
;; 3. Generated bindings
;; ----------------------------------------------------------------------------
;;
;; All three macros produce a Name-exports provide transformer so types can be
;; exported with `(json-type-out Name ...)`.
;;
;; define-json-struct Name:
;;   Name              constructor + struct match expander
;;   Name?             runtime predicate
;;   Name-field        public accessor for each field
;;   Name-js           JSON-hash match expander
;;   Name-js?          JSON-hash predicate
;;   jsexpr->Name      strict decoder (raises on bad input)
;;   as-Name           tolerant decode match expander (bad input → no match)
;;   ^Name             shorthand: (as-Name (Name ...))
;;
;; define-json-enum Name:
;;   Name / Name?      struct wrapper + predicate
;;   Name-v            field accessor for the symbolic tag
;;   Name-variant      pre-built alias constant per variant
;;   Name-js / Name-js? / jsexpr->Name / as-Name / ^Name
;;
;; define-json-union Name:
;;   Name?             runtime predicate
;;   Name-js / Name-js? / jsexpr->Name / as-Name / ^Name
;;
;; ----------------------------------------------------------------------------
;; 4. Match expanders
;; ----------------------------------------------------------------------------
;;
;; Struct:
;;   (Name v ...)             positional struct pattern or constructor
;;   (Name #:f v ...)         keyword struct pattern or constructor
;;   (Name-js ...)            JSON-hash shape match (keyword or positional)
;;   (as-Name pat)            decode JSON hash, then match decoded struct
;;   (^Name pat ...)          shorthand for (as-Name (Name pat ...))
;;
;; Name matches struct instances only.
;; Name-js is a shallow shape match on raw JSON hashes. It checks that expected
;;   keys are present but does not decode nested values.  Partial keyword matches
;;   are allowed.
;; as-Name decodes JSON into a struct first. All required fields must be present
;;   and decodable or the pattern fails (no exception, just no match).
;; ^Name combines as-Name + Name in one step for the common case.
;;
;; Constructors accept positional or keyword arguments; mixing both is an error.
;; Duplicate or unknown keywords are rejected at compile time.
;;
;; Enum:
;;   (Name 'variant)          constructor
;;   Name-variant              alias constant
;;   (Name-js pat)            decode JSON literal, match inner symbolic tag
;;   (as-Name pat)            tolerant decode match
;;   (^Name pat ...)          shorthand: (as-Name (Name pat ...))
;;
;; Union:
;;   No constructor — unions are aliases.  Use Name?, jsexpr->Name, or the
;;   match expanders (Name-js, as-Name, ^Name) to work with values.
;;
;; ----------------------------------------------------------------------------
;; 5. Encoding
;; ----------------------------------------------------------------------------
;;
;; Use ->jsexpr to convert any struct/enum/union value (or nested combination)
;; into a JSON-compatible immutable hash.  Nothing values are omitted from the
;; output hash (they represent absent optional fields).
;;
;; ----------------------------------------------------------------------------
;; 6. Quick examples
;; ----------------------------------------------------------------------------
;;
;;   ;; struct: define, construct, decode+match
;;   (define-json-struct Pos
;;     [line exact-nonnegative-integer?]
;;     [char exact-nonnegative-integer? #:json character])
;;   (Pos #:line 1 #:char 2)
;;   (match (hasheq 'line 1 'character 2)
;;     [(^Pos #:line l #:char c) (list l c)])
;;
;;   ;; enum: define, use alias, decode
;;   (define-json-enum FileChangeType [created 1] [changed 2] [deleted 3])
;;   FileChangeType-created              ; => (FileChangeType 'created)
;;   (jsexpr->FileChangeType 2)          ; => (FileChangeType 'changed)
;;
;;   ;; union: define, predicate, match
;;   (define-json-union DocContent string? Markup)
;;   (DocContent? "hello")               ; => #t
;;   (match json [(^DocContent v) v])
;;
;; ----------------------------------------------------------------------------
;; 7. When to use which
;; ----------------------------------------------------------------------------
;;
;;   Name-js             — matching raw JSON hashes.
;;   as-Name / ^Name     — decoding untrusted JSON input before matching.
;;   json-type-out       — exporting types and their generated bindings.
;;
;; For full usage examples see test files:
;;   tests/json-struct-test.rkt
;;   tests/json-enum-union-test.rkt
;;
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

  ;; Generates keyword-argument constructor with contract enforcement.
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
  (define (gen-struct-match-expander stx name iname keyword-constructor-id keywords fields json-keys)
    (with-syntax ([iname iname]
                  [keyword-constructor keyword-constructor-id]
                  [name name]
                  [(kw ...) keywords]
                  [(fld ...) fields]
                  [(jk ...) json-keys])
      #`(define-match-expander name
          (λ (inner-stx)
            (syntax-parse inner-stx
              [(_ (~seq k:keyword v) (... ...+))
               (gen-match-kw-pattern inner-stx (syntax->list #'(k (... ...))) (syntax->list #'(v (... ...)))
                                     (list #'kw ...) (list #'fld ...) (list #'jk ...)
                                     #'iname 'struct)]
              [(_ v (... ...))
               (gen-match-pos-pattern inner-stx (syntax->list #'(v (... ...)))
                                      (list #'fld ...) (list #'jk ...)
                                      #'iname 'struct)]))
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

  ;; Generates the Name-js match expander.
  (define (gen-json-match-expander stx name-js keywords fields json-keys)
    (with-syntax ([name-js name-js]
                  [(kw ...) keywords]
                  [(fld ...) fields]
                  [(jk ...) json-keys])
      #`(define-match-expander name-js
          (λ (inner-stx)
            (syntax-parse inner-stx
              [(_ (~seq k:keyword v) (... ...+))
               (gen-match-kw-pattern inner-stx (syntax->list #'(k (... ...))) (syntax->list #'(v (... ...)))
                                     (list #'kw ...) (list #'fld ...) (list #'jk ...)
                                     #f 'json)]
              [(_ v (... ...))
               (gen-match-pos-pattern inner-stx (syntax->list #'(v (... ...)))
                                      (list #'fld ...) (list #'jk ...)
                                      #f 'json)]))))))


;; define-json-struct
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

;; define-json-enum
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

;; define-json-union
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

(provide define-json-struct
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

