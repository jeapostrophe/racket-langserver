#lang racket/base
(require (for-syntax racket/base
                     compiler/cm-accomplice
                     racket/match
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
    (INTERFACE LBRACE RBRACE SEMI COLON EOF))

  (define-lex-abbrevs
    [id-chars (char-complement (char-set "(,)/*=;:.~?\"% \n"))]
    [variable-re (:: id-chars (:* id-chars) #;(:? "?"))] ;; TODO: match '?' without saving it?
    [comment-re (:or (:: "//" any-string (:or "\n" "\r" ""))
                     (:: "/*" (complement (:: any-string "*/" any-string)) "*/"))]
    [ignore-re (:or "'use strict';")]
    [namespace-re
     (:: (:? "export") whitespace "namespace" whitespace variable-re whitespace "{" any-string "}")])

  (define program-lexer
    (lexer-src-pos
     [(union whitespace comment-re ignore-re #;namespace-re)
      (return-without-pos (program-lexer input-port))]
     ["export interface" (token-INTERFACE)]
     ["{" (token-LBRACE)]
     ["}" (token-RBRACE)]
     [";" (token-SEMI)]
     [":" (token-COLON)]
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
                         [(INTERFACE NAME LBRACE fields RBRACE interfaces)
                          (hash-set $6 $2 $4)])
             (fields [() (hasheq)]
                     [(NAME COLON NAME SEMI fields)
                      (hash-set $5 $1 $3)]))))

  (define (parse-interface f)
    (parameterize ([file-path f])
      (register-external-file f)
      (call-with-input-file f
        (λ (ip)
          (port-count-lines! ip)
          (program-parser (λ ()
                            (define r (program-lexer ip))
                            (eprintf "lex ~v\n" r)
                            r))))))

  (define (type-name->expander stx ty field_)
    (match ty
      ['boolean (quasisyntax/loc stx (? boolean? #,field_))]
      ['number (quasisyntax/loc stx (? number? #,field_))]
      ['string (quasisyntax/loc stx (? string? #,field_))]
      [_ field_]))
  
  (define (compile-interface stx ifaces)
    (eprintf "~v\n" ifaces)
    (for/list ([(type-name type-def)
                (in-hash ifaces)])
      (with-syntax ([type-name-stx (datum->syntax stx type-name)]
                    [([field field-pat field_ field-kw] ...)
                     (for/list ([(field-name field-type)
                                 (in-hash type-def)])
                       (define field_ (format-id stx "~a_" field-name))
                       (list
                        field-name
                        (type-name->expander stx field-type field_)
                        field_
                        (string->keyword (symbol->string field-name))))])
        (syntax/loc stx
          (begin
            (define-match-expander type-name-stx
              (λ (stx)
                (syntax-parse stx
                  [(_ (~optional (~seq field-kw field_)) ...)
                   (template (hash-table (?? ['field field-pat]) ...))]))
              (λ (stx)
                (syntax-parse stx
                  ([_ field_ ...]
                   (syntax/loc stx
                     (make-hash (list (cons 'field field_) ...)))))))
            (provide type-name-stx)))))))

(provide
 (rename-out [module-begin #%module-begin]))