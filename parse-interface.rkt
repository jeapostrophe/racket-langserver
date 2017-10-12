#lang racket/base
(require (for-syntax racket/base
                     compiler/cm-accomplice
                     racket/match
                     racket/set
                     racket/string
                     racket/syntax
                     syntax/parse
                     syntax/parse/experimental/template
                     syntax/location
                     parser-tools/yacc
                     parser-tools/lex
                     (prefix-in : parser-tools/lex-sre))
         racket/match)

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ f:str)
     (quasisyntax/loc stx
       (#%module-begin
        #,@(compile-interface
            stx
            (parse-interface
             (build-path (syntax-source-directory #'f)
                         (syntax-e #'f))))))]))

(begin-for-syntax
  (define-tokens program-tokens
    (NAME))
  (define-empty-tokens program-punc
    (EXPORT INTERFACE EXTENDS LBRACE RBRACE SEMI COLON OR EOF))

  (define-lex-abbrevs
    [id-chars (char-complement (char-set "(,)/*=;:.~?\"% \n"))]
    [variable-re (:: id-chars (:* id-chars) (:? "?"))] ;; TODO: match '?' without saving it?
    [comment-re (:or (:: "//" any-string (:or "\n" "\r" "")) ;; TODO: broken, too general
                     (:: "/*" (complement (:: any-string "*/" any-string)) "*/"))]
    [ignore-re (:or "'use strict';")])    

  (define program-lexer
    (lexer-src-pos
     [(union whitespace comment-re ignore-re)
      (return-without-pos (program-lexer input-port))]
     ["export" (token-EXPORT)]
     ["interface" (token-INTERFACE)]
     ["extends" (token-EXTENDS)]
     ["{" (token-LBRACE)]
     ["}" (token-RBRACE)]
     [";" (token-SEMI)]
     [":" (token-COLON)]
     ["|" (token-OR)]
     [(eof) (token-EOF)]
     [variable-re
      (token-NAME (string->symbol (string-normalize-spaces lexeme)))]))

  (define (make-srcloc start-pos end-pos)
    (list (file-path)
          (position-line start-pos)
          (position-col start-pos)
          (position-offset start-pos)
          (- (position-offset end-pos) (position-offset start-pos))))

  (define program-parser
    (parser (start program)
            (end EOF)
            (tokens program-tokens program-punc)
            (src-pos)
            (error
             (λ (tok-ok? tok-name tok-value start-pos end-pos)
               (raise-syntax-error
                'interface
                (if tok-ok?
                    (format "Unexpected token ~S" tok-name)
                    (format "Invalid token ~S" tok-name))
                (datum->syntax #f tok-value (make-srcloc start-pos end-pos)))))
            (grammar
             (program [(interfaces) $1])
             (interfaces [() (hasheq)]
                         [(EXPORT INTERFACE NAME LBRACE fields RBRACE interfaces)
                          (hash-set $7 $3 $5)]
                         [(EXPORT INTERFACE NAME EXTENDS NAME LBRACE fields RBRACE interfaces)
                          (hash-set $9 $3 $7)])
             (fields [() (hasheq)]
                     [(NAME COLON types fields)
                      (hash-set $4 $1 $3)])
             (types [(NAME OR types)
                     (set-add $3 $1)]
                    [(NAME SEMI)
                     (set $1)])
             )))

  (define (parse-interface f)
    (parameterize ([file-path f])
      (register-external-file f)
      (call-with-input-file f
        (λ (ip)
          (port-count-lines! ip)
          (program-parser (λ ()
                            (define r (program-lexer ip))
                            (eprintf "~v\n" r)
                            r))))))

  (define (type-name->expander stx ty field_)
    (match ty
      ['boolean (quasisyntax/loc stx (? boolean? #,field_))]
      ['number (quasisyntax/loc stx (? number? #,field_))]
      ['string (quasisyntax/loc stx (? string? #,field_))]
      [_ field_]))

  #;
  (define (type-name->contract stx ty)
    (match ty
      ['boolean (syntax/loc stx boolean?)]
      ['number (syntax/loc stx number?)]
      ['string (syntax/loc stx string?)]
      [(? set?)
       (quasisyntax/loc stx
         (or/c #,@(for/list ([t (in-set ty)])
                    (type-name->contract stx t))))]
      [_ (syntax/loc stx any/c)]))

  (define (type-name->contract ty)
    (match ty
      ['boolean #'boolean?]
      ['number #'number?]
      ['string #'string?]
      [(? set?)
       #`(or/c #,@(set-map ty type-name->contract))]
      [_ #'any/c]))
  
  (define (compile-interface stx ifaces)
    (eprintf "~v\n" ifaces)
    (for/list ([(type-name type-def)
                (in-hash ifaces)])
      (with-syntax ([type-name-stx (datum->syntax stx type-name)]
                    [([field field-ctc field_ field-kw] ...)
                     (for/list ([(field-name field-type)
                                 (in-hash type-def)])
                       (list
                        field-name
                        (type-name->contract stx field-type)
                        (format-id stx "~a_" field-name)
                        (string->keyword (symbol->string field-name))))])
        (syntax/loc stx
          (begin
            (define-match-expander type-name-stx
              (λ (stx)
                (syntax-parse stx
                  [(_ (~optional (~seq field-kw field_)) ...)
                   (template/loc
                    stx (hash-table (?? ['field (? field-ctc field_)]) ...))]))
              (λ (stx)
                (syntax-parse stx
                  ([_ field_ ...]
                   (syntax/loc stx
                     (make-hash (list (cons 'field field_) ...)))))))
            (provide type-name-stx)))))))

(define-syntax (define-json-expander stx)
  (syntax-parse stx
    [(_ name:id [key:id ctc:expr] ...+)
     (with-syntax ([(key_ ...) (generate-temporaries #'(key ...))]
                   [(keyword ...)
                    (for/list ([k (syntax->datum #'(key ...))])
                      (string->keyword (symbol->string k)))])
       (syntax/loc stx
         (define-match-expander name
           (λ (stx)
             (syntax-parse stx
               [(_ (~optional (~seq keyword key_)) ...)
                (template/loc stx (hash-table (?? ['key (? ctc key_)]) ...))]))
           (λ (stx)
             (syntax-parse stx
               [(_ (~optional (~seq keyword key_)) ...)
                (syntax/loc stx
                  (make-hasheq (list (cons 'key key_) ...)))])))))]))

(define-json-expander Pos
  [character number?]
  [line number?])

(match #hasheq([character . 5] [line . 3])
  [(Pos #:line y #:character x)
   #t])

#|
(define-syntax (define-json-expander stx)
  (syntax-parse stx
    [(_ name:id [key:id (~optional ctc:expr #:defaults ([ctc #'any/c]))] ...)
     (with-syntax ([(key_ ...) (generate-temporaries #'(key ...))]
                   [(keyword ...)
                    (for/list ([k (syntax->datum #'(key ...))])
                      (string->keyword (symbol->string k)))])
       (syntax/loc stx
         (define-match-expander name
           (λ (stx)
             (syntax-parse stx
               [(_ (~optional (~seq keyword key_)) ...)
                (template (hash-table (?? ['key (? ctc key_)]) ...))]))
           (λ (stx)
             (syntax-parse stx
               [(_ (~optional (~seq keyword key_)) ...)
                (syntax/loc stx
                  (make-hasheq (list (cons 'key key_) ...)))])))))]))|#
(provide
 (rename-out [module-begin #%module-begin]))