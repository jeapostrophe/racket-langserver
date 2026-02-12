#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse)
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
;; keyword constructors, and polymorphic pattern matching.
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

  ;; Generates the internal struct definition with JSON serialization method.
  (define (gen-struct-def stx iname fields json-keys accessors)
    (with-syntax ([(fld ...) fields])
      (define jsexpr-pairs
        (append*
          (for/list ([jk json-keys]
                     [acc accessors])
            (list #`'#,(datum->syntax stx (syntax-e jk))
                  #`(jsexpr-encode (#,acc self))))))
      #`(struct #,iname (fld ...)
          #:transparent
          #:methods gen:jsexpr-struct
          [(define (->jsexpr self)
             (hasheq #,@jsexpr-pairs))])))

  ;; Positional constructor with contract enforcement.
  (define (gen-checked-constructor iname-checked fields contracts ipred iname)
    #`(define/contract (#,iname-checked #,@fields)
        (-> #,@contracts #,ipred)
        (#,iname #,@fields)))

  ;; Public aliases for the predicate and contracted accessors.
  (define (gen-alias-defs pred ipred public-accessors accessors contracts)
    (cons #`(define #,pred #,ipred)
          (for/list ([pub public-accessors]
                     [acc accessors]
                     [ctc contracts])
            #`(define/contract #,pub
                (-> #,ipred #,ctc)
                #,acc))))

  ;; Generates the public match expander.
  ;; 1. Patterns: matches internal struct OR hasheq (for incoming JSON).
  ;; 2. Expressions: constructor with contract checks.

  ;; keyword-argument constructor with contract enforcement.
  (define (gen-kw-constructor iname-kw iname fields keywords contracts ipred)
    (define kw-args
      (append*
        (for/list ([kw keywords] [f fields])
          (list kw f))))
    (define kw-ctcs
      (append*
        (for/list ([kw keywords] [ctc contracts])
          (list kw ctc))))
    #`(define/contract (#,iname-kw #,@kw-args)
        (-> #,@kw-ctcs #,ipred)
        (#,iname #,@fields)))
  ;; Generates the match pattern for keyword-based matching.
  ;; Supports both internal structs (via struct*) and hashes (via hash-table).
  ;; This allows out-of-order and partial matching.
  (define (gen-match-kw-pattern stx k-list v-list keywords fields json-keys contracts iname)
    (define (find-index target-kw)
      (for/first ([kw keywords] [i (in-naturals)]
                  #:when (eq? (syntax-e target-kw) (syntax-e kw)))
        i))

    (define-values (s-pats h-pats)
      (for/lists (s h) ([k k-list] [v v-list])
        (define idx (find-index k))
        (unless idx
          (raise-syntax-error 'define-json-struct (format "unknown keyword ~a" (syntax-e k)) k))
        (define fld (list-ref fields idx))
        (define jk (list-ref json-keys idx))
        (define ctc (list-ref contracts idx))
        (values #`[#,fld #,v]
                #`['#,(datum->syntax stx (syntax-e jk)) (? #,ctc #,v)])))

    #`(or (struct* #,iname (#,@s-pats))
          (hash-table #,@h-pats)))

  ;; Generates the match pattern for positional matching.
  ;; Enforces full arity to avoid ambiguity with hashes.
  (define (gen-match-pos-pattern stx v-list fields json-keys contracts iname)
    (unless (= (length v-list) (length fields))
      (raise-syntax-error 'define-json-struct
                          (format "wrong number of arguments for positional pattern (expected ~a, given ~a)"
                                  (length fields) (length v-list))
                          stx))
    (define h-pats
      (for/list ([jk json-keys] [ctc contracts] [v v-list])
        #`['#,(datum->syntax stx (syntax-e jk)) (? #,ctc #,v)]))
    #`(or (#,iname #,@v-list)
          (hash-table #,@h-pats)))

  ;; Generates the public match expander.
  (define (gen-match-expander stx name iname iname-kw iname-checked keywords fields contracts json-keys)
    (with-syntax ([iname iname]
                  [iname-kw iname-kw]
                  [iname-checked iname-checked]
                  [name name]
                  [(kw ...) keywords]
                  [(fld ...) fields]
                  [(jk ...) json-keys]
                  [(ctc ...) contracts])
      #`(define-match-expander name
          ;; Pattern transformer
          (λ (inner-stx)
            (syntax-parse inner-stx
              ;; Keyword pattern: (Name #:field1 val1 ...)
              [(_ (~seq k:keyword v) (... ...+))
               (gen-match-kw-pattern inner-stx (syntax->list #'(k (... ...))) (syntax->list #'(v (... ...)))
                                     (list #'kw ...) (list #'fld ...) (list #'jk ...) (list #'ctc ...)
                                     #'iname)]
              ;; Positional pattern: (Name val1 ...)
              [(_ v (... ...))
               (gen-match-pos-pattern inner-stx (syntax->list #'(v (... ...)))
                                      (list #'fld ...) (list #'jk ...) (list #'ctc ...)
                                      #'iname)]))
          ;; Expression transformer (constructor)
          (λ (inner-stx)
            (syntax-parse inner-stx
              [(_ . args)
               (if (any-keyword? #'args)
                   (syntax/loc inner-stx (iname-kw . args))
                   (syntax/loc inner-stx (iname-checked . args)))]))))))

;; define-json-struct — Generates a transparent struct with recursive JSON encoding
;; and polymorphic pattern matching (matching both structs and JSON hashes).
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
;; Patterns match both instances of Name and hasheq tables containing the keys.
;; Field contracts are enforced on construction and during pattern matching.
(define-syntax (define-json-struct stx)
  (syntax-parse stx
    [(_ name:id clause:json-struct-clause ...+)
     ;; Extract field, contract, and key data from syntax class attributes
     (define fields (syntax->list #'(clause.field ...)))
     (define contracts (syntax->list #'(clause.ctc ...)))
     (define json-keys (syntax->list #'(clause.json-key ...)))
     (define keywords (syntax->list #'(clause.keyword ...)))

     ;; Generate names for internal parts
     (define iname (format-id stx "~a:struct" #'name))
     (define iname-checked (format-id stx "~a:checked" #'name))
     (define iname-kw (format-id stx "~a:struct-kw" #'name))
     (define accessors
       (for/list ([f fields])
         (format-id stx "~a:struct-~a" #'name f)))
     (define public-accessors
       (for/list ([f fields])
         (format-id stx "~a-~a" #'name f)))
     (define ipred (format-id stx "~a:struct?" #'name))
     (define pred (format-id stx "~a?" #'name))

     ;; Combine generated parts into a single begin block
     (datum->syntax
       stx
       `(begin
          ,(gen-struct-def stx iname fields json-keys accessors)
          ,(gen-checked-constructor iname-checked fields contracts ipred iname)
          ,(gen-kw-constructor iname-kw iname fields keywords contracts ipred)
          ,@(gen-alias-defs pred ipred public-accessors accessors contracts)
          ,(gen-match-expander stx #'name iname iname-kw iname-checked keywords fields contracts json-keys))
       stx)]))
(provide define-json-expander
         define-json-struct
         gen:jsexpr-struct
         jsexpr-struct?
         ->jsexpr
         jsexpr-encode
         jsexpr-has-key?
         jsexpr-ref
         jsexpr-set
         jsexpr-remove)

