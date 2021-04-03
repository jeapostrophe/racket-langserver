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
         racket/dict
         racket/format
         "append-message.rkt"
         "check-syntax.rkt"
         "error-codes.rkt"
         "interfaces.rkt"
         "json-util.rkt"
         "responses.rkt"
         "symbol-kinds.rkt"
         "docs-helpers.rkt"
         "doc-trace.rkt")

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
  (define trace (check-syntax path doc-text #f))
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
    (match-define (doc doc-text doc-trace) this-doc)
    (define content-changes*
      (cond [(eq? (json-null) content-changes) empty]
            [(list? content-changes) content-changes]
            [else (list content-changes)]))
    (for ([change (in-list content-changes*)])
      (match change
        [(ContentChangeEvent #:range (Range #:start (Pos #:line st-ln #:char st-ch) #:end (Pos #:line ed-ln #:char ed-ch))
                             #:text text)
         (define st-pos (line/char->pos doc-text st-ln st-ch))
         (define end-pos (line/char->pos doc-text ed-ln ed-ch))
         (define old-len (- end-pos st-pos))
         (define new-len (string-length text))
         (cond [(> new-len old-len) (send doc-trace expand end-pos (+ st-pos new-len))]
               [(< new-len old-len) (send doc-trace contract (+ st-pos new-len) end-pos)]
               [else #f])
         (send doc-text insert text st-pos end-pos)]
        [(ContentChangeEvent #:text text)
         (send doc-trace reset)
         (send doc-text erase)
         (send doc-text insert text 0)]))
    ;; Only perform syntax check if the 'skip-syncheck' flag is *not*
    ;; set. See 'append-message.rkt' for more info.
    (unless (hash-ref params skip-syncheck #f)
      (define path (uri->path uri))
      (check-syntax path doc-text doc-trace)))
  (void))

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
     (match-define (list link tag)
       (interval-map-ref (send doc-trace get-docs) pos (list #f #f)))
     (define result
       (cond [text
              (hasheq 'contents (if link (~a text " - [docs](" (~a "https://docs.racket-lang.org/" (last (string-split link "/doc/"))) ")") text)
                      'range (Range #:start (abs-pos->Pos doc-text start)
                                    #:end   (abs-pos->Pos doc-text end)))]
             [else (hasheq 'contents empty)]))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/hover failed")]))

;; Signature Help request
(define (signatureHelp id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char ch)])
     (unless (uri-is-path? uri)
       (error 'signatureHelp "uri is not a path"))
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define pos (line/char->pos doc-text line ch))
     (define text (send doc-text get-text))
     (define new-pos (find-containing-paren (- pos 1) text))
     (define result
       (cond [new-pos
              (define maybe-tag (interval-map-ref (send doc-trace get-docs) (+ new-pos 1) #f))
              (define tag
                (cond [maybe-tag (last maybe-tag)]
                      [else
                       (define doc-symbols (send doc-trace get-symbols))
                       (define-values (start end symbol)
                         (interval-map-ref/bounds doc-symbols (+ new-pos 2) #f))
                       (cond [symbol
                              (id-to-tag (first symbol) doc-trace)]
                             [else #f])]))
              (cond [tag
                     (match-define (list sigs docs) (get-docs-for-tag tag))
                     (hasheq 'signatures (map (lambda sig (hasheq 'label sig 'documentation (or docs (json-null)))) sigs))]
                    [else (json-null)])]
             [else (json-null)]))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/signatureHelp failed")]))

;; Completion Request
(define (completion id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char ch)])
     (unless (uri-is-path? uri)
       (error 'completion "uri is not a path"))
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define completions (send doc-trace get-completions))
     (define result
       (for/list ([completion (in-list completions)])
         (hasheq 'label (symbol->string completion))))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/completion failed")]))

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
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define results
       (dict-map (send doc-trace get-symbols)
                 (λ (key value)
                   (match-define (cons start end) key)
                   (match-define (list text type) value)
                   (define kind (match type
                                  ['constant SymbolKind-Constant]
                                  ['string SymbolKind-String]
                                  ['symbol SymbolKind-Variable]))
                   (define range
                     (Range #:start (abs-pos->Pos doc-text start)
                            #:end   (abs-pos->Pos doc-text end)))
                   (SymbolInfo #:name text
                               #:kind kind
                               #:location (Location #:uri uri
                                                    #:range range)))))
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
     (define mut-doc-text
       (if (is-a? doc-text racket:text%)
           (let ([r-text (new racket:text%)]) (send r-text insert (send doc-text get-text)) r-text)
           (send doc-text copy-self)))
     (define results
       (if (eq? indenter 'missing) (json-null)
           (let loop ([line start-line])
             (if (> line end-line)
                 null
                 (let ([edit (indent-line! mut-doc-text indenter line)])
                   (if edit
                       (cons edit (loop (add1 line)))
                       (loop (add1 line))))))))
     (success-response id results)]
    [_
     (error-response id INVALID-PARAMS "textDocument/rangeFormatting failed")]))

;; On-type formatting request
(define (on-type-formatting! id params)
  (match params
    ;; We're ignoring 'options for now
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char char)]
                 ['ch ch])
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define pos (- (line/char->pos doc-text line char) 1))
     (define range
       (match ch
         ["\n"
          (Range 
           #:start (abs-pos->Pos doc-text (send doc-text paragraph-start-position line)) 
           #:end (abs-pos->Pos doc-text (send doc-text paragraph-end-position line)))]
         [")"
          (Range
           #:start (abs-pos->Pos doc-text (or (find-containing-paren pos (send doc-text get-text)) 0))
           #:end (abs-pos->Pos doc-text pos))]))
     
     (range-formatting!
      id
      (hash-set params
                'range range))]
    [_
     (error-response id INVALID-PARAMS "textDocument/onTypeformatting failed")]))

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
    [(not (number? desired-spaces)) #f]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [did-open! (jsexpr? . -> . void?)]
  [did-close! (jsexpr? . -> . void?)]
  [did-change! (jsexpr? . -> . void?)]
  [hover (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [completion (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [signatureHelp (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [definition (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [document-highlight (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [references (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [document-symbol (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [formatting! (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [range-formatting! (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [on-type-formatting! (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]))
