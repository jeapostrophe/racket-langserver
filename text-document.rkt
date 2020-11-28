#lang racket/base
(require (for-syntax racket/base)
         data/interval-map
         framework
         json
         racket/class
         racket/contract/base
         racket/list
         racket/match
         racket/string
         racket/set
         syntax-color/module-lexer
         syntax-color/racket-lexer
         "append-message.rkt"
         "check-syntax.rkt"
         "error-codes.rkt"
         "interfaces.rkt"
         "json-util.rkt"
         "responses.rkt"
         "symbol-kinds.rkt")

(struct doc (text trace) #:transparent #:mutable)

(define (uri-is-path? str)
  (string-prefix? str "file://"))

(define (uri->path uri)
  (cond
    [(eq? (system-type 'os) 'windows)
     ;; If a file URI begins with file:// or file:////, Windows translates it
     ;; as a UNC path. If it begins with file:///, it's translated to an MS-DOS
     ;; path. (https://en.wikipedia.org/wiki/File_URI_scheme#Windows_2)
     (cond
       [(string-prefix? uri "file:////") (substring uri 7)]
       [(string-prefix? uri "file:///") (substring uri 8)]
       [else (string-append "//" (substring uri 7))])]
    [else (substring uri 7)]))

;;
;; Match Expanders
;;;;;;;;;;;;;;;;;;;;

(define-json-expander Location
  [uri string?]
  [range any/c])

(define-json-expander ContentChangeEvent
  [range any/c]
  [rangeLength exact-nonnegative-integer?]
  [text string?])

;; VersionedTextDocumentIdentifier
(define-json-expander DocIdentifier
  [version exact-nonnegative-integer?]
  [uri string?])

;; TextDocumentItem
(define-json-expander DocItem
  [uri string?]
  [languageId string?]
  [version exact-nonnegative-integer?]
  [text string?])

(define-json-expander DocHighlight
  [range any/c])

(define-json-expander SymbolInfo
  [name string?]
  [kind exact-positive-integer?]
  [location any/c])

(define-json-expander TextEdit
  [range any/c]
  [newText string?])

;;
;; Methods
;;;;;;;;;;;;

(define open-docs (make-hasheq))

(define (did-open! params)
  (match-define (hash-table ['textDocument (DocItem #:uri uri #:text text)]) params)
  (unless (uri-is-path? uri)
    ;; TODO: send user diagnostic or something
    (error 'did-open "uri is not a path."))
  (define path (uri->path uri))
  (define doc-text (new racket:text%))
  (send doc-text insert text 0)
  (define trace (check-syntax path doc-text))
  (hash-set! open-docs (string->symbol uri) (doc doc-text trace)))

(define (did-close! params)
  (match-define (hash-table ['textDocument (DocItem #:uri uri)]) params)
  (when (uri-is-path? uri)
    (hash-remove! open-docs (string->symbol uri))))

(define (did-change! params)
  (match-define (hash-table ['textDocument (DocIdentifier #:uri uri)]
                            ['contentChanges content-changes]) params)
  (when (uri-is-path? uri)
    (define this-doc (hash-ref open-docs (string->symbol uri)))
    (match-define (doc doc-text _) this-doc)
    (define content-changes*
      (cond [(eq? (json-null) content-changes) empty]
            [(list? content-changes) content-changes]
            [else (list content-changes)]))
    (for ([change (in-list content-changes*)])
      (match change
        [(ContentChangeEvent #:range (Range #:start (Pos #:line st-ln #:char st-ch))
                             #:rangeLength range-ln
                             #:text text)
         (define st-pos (line/char->pos doc-text st-ln st-ch))
         (define end-pos (+ st-pos range-ln))
         (send doc-text insert text st-pos end-pos)]
        [(ContentChangeEvent #:text text)
         (send doc-text erase)
         (send doc-text insert text 0)]))
    ;; Only perform syntax check if the 'skip-syncheck' flag is *not*
    ;; set. See 'append-message.rkt' for more info.
    (unless (hash-ref params skip-syncheck #f)
      (define path (uri->path uri))
      (define trace (check-syntax path doc-text))
      (set-doc-trace! this-doc trace))))

;; Hover request
;; Returns an object conforming to the Hover interface, to
;; be used as the result of the response message.
(define (hover id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char ch)])
     (unless (uri-is-path? uri)
       (error 'hover "uri is not a path"))
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define hovers (send doc-trace get-hovers))
     (define pos (line/char->pos doc-text line ch))
     (define-values (start end text)
       (interval-map-ref/bounds hovers pos #f))
     (define result
       (cond [text
              (hasheq 'contents text
                      'range (Range #:start (abs-pos->Pos doc-text start)
                                    #:end   (abs-pos->Pos doc-text end)))]
             [else (hasheq 'contents empty)]))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/hover failed")]))

;; Definition request
(define (definition id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char char)])
     (unless (uri-is-path? uri)
       (error 'definition "uri is not a path"))
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define doc-bindings (send doc-trace get-sym-bindings))
     (define pos (line/char->pos doc-text line char))
     (define decl (interval-map-ref doc-bindings pos #f))
     (define result
       (match decl
         [#f (json-null)]
         [(Decl _ start end)
          (Location #:uri uri
                    #:range (Range #:start (abs-pos->Pos doc-text start)
                                   #:end   (abs-pos->Pos doc-text end)))]))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/definition failed")]))

;; Reference request
(define (references id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char char)]
                 ['context (hash-table ['includeDeclaration include-decl?])])
     (define ranges (get-doc-refs uri line char include-decl?))
     (define result
       (for/list ([range (in-list ranges)])
         (Location #:uri uri
                   #:range range)))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/references failed")]))

;; Document Highlight request
(define (document-highlight id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char char)])
     (define ranges (get-doc-refs uri line char #t))
     (define result
       (for/list ([range (in-list ranges)])
         (DocHighlight #:range range)))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/documentHighlight failed")]))

;; Gets the document highlights for the current position and returns
;; a list of Range objects containing those highlights. Right now this
;; function is used by both 'references' and 'document-highlight' because
;; there is currently no support for project-wide symbol references.
(define (get-doc-refs uri line char include-decl?)
  (unless (uri-is-path? uri)
    (error 'get-doc-refs "uri is not a path"))
  (match-define (doc doc-text doc-trace)
    (hash-ref open-docs (string->symbol uri)))
  (define doc-decls (send doc-trace get-sym-decls))
  (define doc-bindings (send doc-trace get-sym-bindings))
  (define pos (line/char->pos doc-text line char))
  (define (refs-from-decl decl-left decl-right bindings)
    (if include-decl?
        (set-add bindings (cons decl-left decl-right))
        bindings))
  (define (refs-from-binding)
    (define-values (use-left use-right decl)
      (interval-map-ref/bounds doc-bindings pos #f))
    (match decl
      [#f (set)]
      [(Decl require? decl-left decl-right)
       (cond [(and require? include-decl?)
              (set (cons decl-left decl-right) (cons use-left use-right))]
             [require? (set (cons use-left use-right))]
             [else
              (define bindings (interval-map-ref doc-decls decl-left))
              (if include-decl?
                  (set-add bindings (cons decl-left decl-right))
                  bindings)])]))
  (define-values (decl-left decl-right bindings)
    (interval-map-ref/bounds doc-decls pos #f))
  (define refs (if bindings
                   (refs-from-decl decl-left decl-right bindings)
                   (refs-from-binding)))
  (for/list ([rf (in-set refs)])
    (match-define (cons start end) rf)
    (Range #:start (abs-pos->Pos doc-text start)
           #:end   (abs-pos->Pos doc-text end))))

;; Document Symbol request
(define (document-symbol id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)])
     (unless (uri-is-path? uri)
       (error 'document-symbol "uri is not a path"))
     (match-define (doc doc-text _)
       (hash-ref open-docs (string->symbol uri)))
     (define in (open-input-string (send doc-text get-text)))
     (port-count-lines! in)
     (define lexer (get-lexer in))
     (define results
       (for/fold ([out empty])
                 ([lst (in-port (lexer-wrap lexer) in)])
         (match-define (list text type paren? start end) lst)
         (cond [(set-member? '(constant string symbol) type)
                (define kind (match type
                               ['constant SymbolKind-Constant]
                               ['string SymbolKind-String]
                               ['symbol SymbolKind-Variable]))
                (define range
                  (Range #:start (abs-pos->Pos doc-text start)
                         #:end   (abs-pos->Pos doc-text end)))
                (define sym-info
                  (SymbolInfo #:name text
                              #:kind kind
                              #:location (Location #:uri uri
                                                   #:range range)))
                (cons sym-info out)]
               [else out])))
     (success-response id results)]
    [_
     (error-response id INVALID-PARAMS "textDocument/documentSymbol failed")]))

;; Full document formatting request
(define (formatting! id params)
  (match params
    ;; We're ignoring 'options for now
    [(hash-table ['textDocument (DocIdentifier #:uri uri)])
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define end-pos (send doc-text last-position))
     (range-formatting!
       id
       (hash-set params
                 'range (Range #:start (abs-pos->Pos doc-text 0)
                               #:end (abs-pos->Pos doc-text end-pos))))]
    [_
     (error-response id INVALID-PARAMS "textDocument/formatting failed")]))

;; Range Formatting request
(define (range-formatting! id params)
  (match params
    ;; XXX We're ignoring 'options' for now
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['range (Range  #:start start #:end end)])
     (unless (uri-is-path? uri)
       (error 'range-formatting! "uri is not a path"))
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define indenter (send doc-trace get-indenter))
     (define start-pos (Pos->abs-pos doc-text start))
     (define end-pos (Pos->abs-pos doc-text end))
     (define start-line (send doc-text position-paragraph start-pos))
     (define end-line (send doc-text position-paragraph end-pos))
     (define results
       (let loop ([line start-line])
         (if (> line end-line)
             null
             (let ([edit (indent-line! doc-text indenter line)])
               (if edit
                   (cons edit (loop (add1 line)))
                   (loop (add1 line)))))))
     (success-response id results)]
    [_
     (error-response id INVALID-PARAMS "textDocument/rangeFormatting failed")]))

;; Returns a TextEdit, or #f if the line is already correct.
(define (indent-line! doc-text indenter line)
  (define line-start (send doc-text paragraph-start-position line))
  (define line-end (send doc-text paragraph-end-position line))
  (define line-text (send doc-text get-text line-start line-end))
  (define line-length (string-length line-text))
  (define current-spaces
    (let loop ([i 0])
      (cond [(= i line-length) i]
            [(char=? (string-ref line-text i) #\space) (loop (add1 i))]
            [else i])))
  (define desired-spaces
    (if indenter
        (indenter doc-text line-start)
        (send doc-text compute-racket-amount-to-indent line-start)))
  (cond
    [(= current-spaces desired-spaces) #f]
    [(< current-spaces desired-spaces)
     ;; Insert spaces
     (define insert-count (- desired-spaces current-spaces))
     (define new-text (make-string insert-count #\space))
     (define pos (Pos #:line line #:char 0))
     (send doc-text insert new-text line-start 'same)
     (TextEdit #:range (Range #:start pos #:end pos)
               #:newText new-text)]
    [else
     ;; Delete spaces
     (define span (- current-spaces desired-spaces))
     (send doc-text delete line-start (+ line-start span))
     (TextEdit #:range (Range #:start (Pos #:line line #:char 0)
                              #:end   (Pos #:line line #:char span))
               #:newText "")]))

;; Wrapper for in-port, returns a list or EOF.
(define ((lexer-wrap lexer) in)
  (define-values (txt type paren? start end)
    (lexer in))
  (if (eof-object? txt)
      eof
      (list txt type paren? start end)))

;; Call module-lexer on an input port, then discard all
;; values except the lexer.
(define (get-lexer in)
  (match-define-values
    (_ _ _ _ _ _ lexer)
    (module-lexer in 0 #f))
  (if (procedure? lexer) ;; TODO: Is this an issue with module-lexer docs?
      lexer
      (if (eq? lexer 'no-lang-line)
          racket-lexer
          (error 'get-lexer "~v" lexer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [did-open! (jsexpr? . -> . void?)]
  [did-close! (jsexpr? . -> . void?)]
  [did-change! (jsexpr? . -> . void?)]
  [hover (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [definition (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [document-highlight (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [references (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [document-symbol (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [formatting! (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [range-formatting! (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]))
