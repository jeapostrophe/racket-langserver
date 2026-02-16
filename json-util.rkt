#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/set
                     racket/syntax
                     syntax/parse
                     racket/provide-transform)
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
  (->jsexpr jsexpr-struct))

;; Recursively converts data (structs, hashes, lists) into JSON-compatible immutable hashes.
(define (jsexpr-encode v)
  (cond
    [(jsexpr-struct? v) (->jsexpr v)]
    [(hash? v)
     (for/hasheq ([(k val) (in-hash v)])
       (values k (jsexpr-encode val)))]
    [(list? v) (map jsexpr-encode v)]
    [else v]))

;; define-json-struct — generates a transparent struct with JSON support,
;; keyword constructors, and split pattern matching.
(begin-for-syntax
  (define (symbol->keyword sym)
    (string->keyword (symbol->string sym)))

  ;; Syntax class to parse a field clause.
  ;; Supports:
  ;;   - [field ctc]            -> json key is same as field name
  ;;   - [field ctc #:json key] -> custom json key
  (define-syntax-class json-struct-clause
    #:attributes (field ctc json-key keyword)
    (pattern [field:id ctc:expr #:json json-key:id]
             #:with keyword (symbol->keyword (syntax-e #'field)))
    (pattern [field:id ctc:expr]
             #:with json-key #'field
             #:with keyword (symbol->keyword (syntax-e #'field))))

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
          [(define (->jsexpr self)
             (hasheq (~@ 'jk (jsexpr-encode (acc self))) ...))])))

  ;; Public aliases for the accessors.
  (define (gen-public-accessors public-accessors accessors)
    (for/list ([pub public-accessors]
               [acc accessors])
      #`(define #,pub #,acc)))

  ;; Generates Name-js? predicate.
  ;; Checks JSON hash shape and validates each field contract.
  (define (gen-json-predicate stx js-pred-name json-keys contracts)
    (with-syntax ([js-pred-name js-pred-name]
                  [(jk ...) json-keys]
                  [(ctc ...) contracts])
      #'(define (js-pred-name x)
          (and (hash? x)
               (and (hash-has-key? x 'jk)
                    (ctc (hash-ref x 'jk)))
               ...))))

  ;; Generates the export bundle macro.
  (define (gen-exports name-exports name pred public-accessors name-js name-js-pred)
    (with-syntax ([name-exports name-exports]
                  [name name]
                  [pred pred]
                  [(pub ...) public-accessors]
                  [name-js name-js]
                  [name-js-pred name-js-pred])
      #'(define-syntax name-exports
          (make-provide-transformer
            (lambda (stx modes)
              (expand-export
                #'(combine-out name pred pub ... name-js name-js-pred)
                modes))))))

  ;; Generates the public match expander.
  ;; 1. Patterns: matches internal struct.
  ;; 2. Expressions: constructor with contract checks.

  ;; keyword-argument constructor with contract enforcement.
  (define (gen-kw-constructor iname-kw-proc iname fields keywords contracts struct-pred)
    (with-syntax ([iname-kw-proc iname-kw-proc]
                  [iname iname]
                  [struct-pred struct-pred]
                  [(kw ...) keywords]
                  [(fld ...) fields]
                  [(ctc ...) contracts])
      #'(define/contract (iname-kw-proc (~@ kw fld) ...)
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
  (define (gen-struct-match-expander stx name iname iname-kw keywords fields json-keys)
    (with-syntax ([iname iname]
                  [iname-kw iname-kw]
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
                     (iname-kw arg (... ...)))))]
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


;; define-json-struct — Generates a transparent struct with recursive JSON encoding
;; and split pattern matching (struct via Name, JSON via Name-js).
;;
;; Syntax:
;;   (define-json-struct Name
;;     [field contract]                  ; JSON key matches field name
;;     [field contract #:json json-key]) ; Custom JSON key
;;
;; Constructors (Expression context):
;;   - Positional: (Name v1 v2 ...)
;;   - Keyword:    (Name #:f1 v1 #:f2 v2 ...) [order-independent]
;;
;; Pattern Matching (Match context):
;;   - Positional: (match x [(Name p1 p2 ...) ...]) [requires all fields]
;;   - Keyword:    (match x [(Name #:f1 p1) ...])    [partial & order-independent]
;;
;; Name patterns match instances of Name only.
;; Name-js patterns match hasheq tables containing the JSON keys.
;; Field contracts are enforced on construction and by Name-js?.
(define-syntax (define-json-struct stx)
  (syntax-parse stx
    [(_ name:id clause:json-struct-clause ...+)
     ;; Extract field, contract, and key data from syntax class attributes
     (define fields (syntax->list #'(clause.field ...)))
     (define contracts (syntax->list #'(clause.ctc ...)))
     (define json-keys (syntax->list #'(clause.json-key ...)))
     (define keywords (syntax->list #'(clause.keyword ...)))

     ;; Generate names for internal parts
     (define iname (format-id stx "~a~~" #'name)) ; Internal struct
     (define iname-kw (format-id stx "~a~~kw" #'name)) ; Internal keyword constructor
     (define accessors
       (for/list ([f fields])
         (format-id stx "~a~~-~a" #'name f)))
     (define public-accessors
       (for/list ([f fields])
         (format-id stx "~a-~a" #'name f)))

     ;; struct-pred-internal aliases the generated internal struct predicate.
     (define struct-pred-internal (format-id stx "~a~~?" #'name))
     (define pred (format-id stx "~a?" #'name))

     (define name-js-pred (format-id stx "~a-js?" #'name))
     (define name-js (format-id stx "~a-js" #'name))
     (define name-exports (format-id stx "~a-exports" #'name))

     ;; Combine generated parts into a single begin block
     (datum->syntax
       stx
       `(begin
          ,(gen-contracted-struct stx iname fields contracts json-keys accessors)
          ,(gen-kw-constructor iname-kw iname fields keywords contracts struct-pred-internal)
          ,@(gen-public-accessors public-accessors accessors)
          (define ,pred ,struct-pred-internal)

          ,(gen-struct-match-expander stx #'name iname iname-kw keywords fields json-keys)
          ,(gen-json-match-expander stx name-js keywords fields json-keys)
          ,(gen-json-predicate stx name-js-pred json-keys contracts)

          ,(gen-exports name-exports #'name pred public-accessors name-js name-js-pred))
       stx)]))

(define-syntax json-struct-out
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

(provide define-json-expander
         define-json-struct
         json-struct-out
         gen:jsexpr-struct
         jsexpr-struct?
         ->jsexpr
         jsexpr-encode
         jsexpr-has-key?
         jsexpr-ref
         jsexpr-set
         jsexpr-remove)

