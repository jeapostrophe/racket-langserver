#lang racket/base
(require
 (for-syntax racket/base
             racket/list
             racket/syntax
             syntax/parse
             syntax/location
             parser-tools/yacc
             parser-tools/lex
             (prefix-in : parser-tools/lex-sre))
 racket/format)

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

  (define-lex-abbrev id-chars (char-complement (char-set "(,)=;:.~?\"% \n")))
  (define-lex-abbrev variable-re (:: id-chars (:* id-chars)))

  (define program-lexer
    (lexer-src-pos
     [whitespace
      (return-without-pos (program-lexer input-port))]
     ["interface" (token-INTERFACE)]
     ["{" (token-LBRACE)]
     ["}" (token-RBRACE)]
     [";" (token-SEMI)]
     [":" (token-COLON)]
     [(eof) (token-EOF)]
     [variable-re
      (token-NAME (string->symbol lexeme))]))

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
             (lambda (tok-ok? tok-name tok-value start-pos end-pos)
               (raise-syntax-error
                'interface
                (if tok-ok?
                  (format "Unexpected token ~S" tok-name)
                  (format "Invalid token ~S" tok-name))
                (datum->syntax #f tok-value (make-srcloc start-pos end-pos)))))
            (grammar
             (program [(interfaces) $1])
             (interfaces [() (hasheq)]
                         [(INTERFACE NAME LBRACE fields RBRACE SEMI interfaces)
                          (hash-set $7 $2 $4)])
             (fields [() (hasheq)]
                     [(NAME COLON NAME SEMI fields)
                      (hash-set $5 $1 $3)]))))

  (define (parse-interface f)
    (parameterize ([file-path f])
      (call-with-input-file f
        (λ (ip)
          (port-count-lines! ip)
          (program-parser (λ () (program-lexer ip)))))))

  (define (compile-interface stx ifaces)
    (eprintf "~v\n" ifaces)
    (for/list ([(type-name type-def)
                (in-hash ifaces)])
      (with-syntax ([type-name-stx (datum->syntax stx type-name)]
                    [format-type-name (format-id stx "format-~a" type-name)]
                    [([format-field-type field type-name-field] ...)
                     (for/list ([(field-name field-type)
                                 (in-hash type-def)])
                       (list
                        (format-id stx "format-~a" field-type)
                        field-name
                        (format-id stx "~a-~a" type-name field-name)))])
        (syntax/loc stx
          (begin
            ;; XXX You put match-expander here
            (struct type-name-stx (field ...))
            (define (format-type-name it)
              (~a 'type-name-stx "("
                  (list (format-field-type (type-name-field it)) ...)
                  ")"))
            (provide (struct-out type-name-stx)
                     format-type-name)))))))

(define (format-int x)
  (number->string x))
(define (format-bool x)
  (if x "T" "F"))

(provide
 format-int format-bool
 (rename-out
  [module-begin #%module-begin]))
