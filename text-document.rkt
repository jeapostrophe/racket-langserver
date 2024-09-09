#lang racket/base
(require data/interval-map
         json
         racket/match
         racket/list
         racket/contract
         racket/class
         racket/format
         racket/string
         racket/set
         racket/dict
         "error-codes.rkt"
         "interfaces.rkt"
         "json-util.rkt"
         "path-util.rkt"
         "responses.rkt"
         "symbol-kinds.rkt"
         "docs-helpers.rkt"
         "documentation-parser.rkt"
         "doc.rkt"
         "struct.rkt")

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

(define-json-expander InlayHint
  ; must a position `Pos`, defined in interfaces.rkt
  [position any/c]
  ; should be `string | InlayHintLabelPart[]`
  ; but let's stay in simple case for now
  [label string?])

(define (pos->abs-pos doc pos)
  (match-define (Pos #:line line #:char char) pos)
  (doc-pos doc line char))

(define (abs-pos->pos doc pos)
  (define-values (line char) (doc-line/ch doc pos))
  (Pos #:line line #:char char))

(define (start/end->range doc start end)
  (Range #:start (abs-pos->pos doc start) #:end (abs-pos->pos doc end)))

;;
;; Methods
;;;;;;;;;;;;

(define open-docs (make-hasheq))

(define (did-open! params)
  (match-define (hash-table ['textDocument (DocItem #:uri uri #:text text)]) params)
  (hash-set! open-docs (string->symbol uri)
             (new-doc uri text)))

(define (did-close! params)
  (match-define (hash-table ['textDocument (DocItem #:uri uri)]) params)
  (hash-remove! open-docs (string->symbol uri)))

(define (did-change! params)
  (match-define (hash-table ['textDocument (DocIdentifier #:uri uri)]
                            ['contentChanges content-changes]) params)
  (define this-doc (hash-ref open-docs (string->symbol uri)))
  (define content-changes*
    (cond [(eq? (json-null) content-changes) empty]
          [(list? content-changes) content-changes]
          [else (list content-changes)]))

  (doc-batch-change this-doc
                    (for ([change (in-list content-changes*)])
                      (match change
                        [(ContentChangeEvent #:range (Range #:start (Pos #:line st-ln #:char st-ch)
                                                            #:end (Pos #:line ed-ln #:char ed-ch))
                                             #:text text)
                         (doc-update! this-doc st-ln st-ch ed-ln ed-ch text)]
                        [(ContentChangeEvent #:text text)
                         (doc-reset! this-doc text)])))
  (void))

;; Hover request
;; Returns an object conforming to the Hover interface, to
;; be used as the result of the response message.
(define (hover id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char ch)])
     (define this-doc (hash-ref open-docs (string->symbol uri)))
     (define doc-trace (Doc-trace this-doc))

     (define hovers (send doc-trace get-hovers))
     (define pos (doc-pos this-doc line ch))
     (define-values (start end text)
       (interval-map-ref/bounds hovers pos #f))
     (match-define (list link tag)
       (interval-map-ref (send doc-trace get-docs) pos (list #f #f)))
     (define result
       (cond [text
              ;; We want signatures from `scribble/blueboxes` as they have better indentation,
              ;; but in some super rare cases blueboxes aren't accessible, thus we try to use the
              ;; parsed signature instead
              (match-define (list sigs args-descr)
                (if tag
                    (get-docs-for-tag tag)
                    (list #f #f)))
              (define maybe-signature
                (if sigs
                    (~a "```\n"
                        (string-join sigs "\n")
                        (if args-descr (~a "\n" args-descr) "")
                        "\n```\n---\n")
                    #f))
              (define documentation-text
                (if link
                    (~a (or maybe-signature "")
                        (or (extract-documentation-for-selected-element
                             link #:include-signature? (not maybe-signature))
                            ""))
                    ""))
              (define contents (if link
                                   (~a text
                                       " - [online docs]("
                                       (make-proper-url-for-online-documentation link)
                                       ")\n"
                                       (if (non-empty-string? documentation-text)
                                           (~a "\n---\n" documentation-text)
                                           ""))
                                   text))
              (hasheq 'contents contents
                      'range (start/end->range this-doc start end))]
             [else (hasheq 'contents empty)]))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/hover failed")]))

;; Code Action request
(define (code-action id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['range (Range #:start start #:end _end)]
                 ; TODO: _ctx has three fields
                 ; 1. `diagnostics`
                 ; 2. `only: CodeActionKind[]` server should use this to filter out client-unwanted code action
                 ; 3. `triggerKind?: CodeActionTriggerKind` the reason why code action were requested
                 ['context _ctx])
     (define this-doc (hash-ref open-docs (string->symbol uri)))
     (define doc-trace (Doc-trace this-doc))

     (define act (interval-map-ref (send doc-trace get-quickfixs)
                                   (pos->abs-pos this-doc start)
                                   #f))
     (if act
         (success-response id (list act))
         (success-response id (list)))]
    [(hash-table ['textDocument (DocIdentifier #:uri uri)])
     (error-response id INVALID-PARAMS
                     (format "textDocument/codeAction failed uri is not a path ~a" uri))]
    [_ (error-response id INVALID-PARAMS "textDocument/codeAction failed")]))

;; Signature Help request
(define (signatureHelp id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char ch)])
     (define this-doc (hash-ref open-docs (string->symbol uri)))
     (define doc-trace (Doc-trace this-doc))

     (define pos (doc-pos this-doc line ch))
     (define new-pos (doc-find-containing-paren this-doc (- pos 1)))
     (define result
       (cond [new-pos
              (define maybe-tag (interval-map-ref (send doc-trace get-docs) (+ new-pos 1) #f))
              (define tag
                (cond [maybe-tag (last maybe-tag)]
                      [else
                       (define symbols (doc-get-symbols this-doc))
                       (define-values (start end symbol)
                         (interval-map-ref/bounds symbols (+ new-pos 2) #f))
                       (cond [symbol
                              (id-to-tag (first symbol) doc-trace)]
                             [else #f])]))
              (cond [tag
                     (match-define (list sigs docs) (get-docs-for-tag tag))
                     (if sigs
                         (hasheq 'signatures (map (lambda (sig)
                                                    (hasheq 'label sig
                                                            'documentation (or docs (json-null))))
                                                  sigs))
                         (json-null))]
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
     (define this-doc (hash-ref open-docs (string->symbol uri)))
     (define doc-trace (Doc-trace this-doc))

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
     (define-values (start end decl) (get-decl uri line char))

     (define this-doc (hash-ref open-docs (string->symbol uri)))

     (define result
       (match decl
         [#f (json-null)]
         [(Decl #f id start end)
          (Location #:uri uri
                    #:range (start/end->range this-doc start end))]
         [(Decl path id 0 0)
          (Location #:uri (path->uri path)
                    #:range (Range->hash (get-definition-by-id path id)))]))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/definition failed")]))

;; Reference request
(define (references id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char char)]
                 ['context (hash-table ['includeDeclaration include-decl?])])
     (define-values (start end decl) (get-decl uri line char))

     (define this-doc (hash-ref open-docs (string->symbol uri)))

     (define result
       (match decl
         [(Decl req? id left right)
          (define ranges
            (if req?
                (list (start/end->range this-doc start end)
                      (start/end->range this-doc left right))
                (or (get-bindings uri decl))))
          (for/list ([range (in-list ranges)])
            (hasheq 'uri uri 'range range))]
         [#f (json-null)]))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/references failed")]))

;; Document Highlight request
(define (document-highlight id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char char)])

     (define this-doc (hash-ref open-docs (string->symbol uri)))

     (define-values (start end decl) (get-decl uri line char))
     (define result
       (match decl
         [(Decl filename id left right)
          (define ranges
            (if filename
                (list (start/end->range this-doc start end)
                      (start/end->range this-doc left right))
                (or (append (get-bindings uri decl)
                            (list (start/end->range this-doc left right))))))
          (for/list ([range (in-list ranges)])
            (hasheq 'range range))]
         [#f (json-null)]))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/documentHighlight failed")]))

;; Rename request
(define (_rename id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char char)]
                 ['newName new-name])
     (define-values (start end decl) (get-decl uri line char))

     (define this-doc (hash-ref open-docs (string->symbol uri)))

     (define result
       (match decl
         [(Decl req? id left right)
          (cond [req? (json-null)]
                [else
                 (define ranges (cons (start/end->range this-doc left right)
                                      (get-bindings uri decl)))
                 (WorkspaceEdit
                  #:changes
                  (hasheq (string->symbol uri)
                          (for/list ([range (in-list ranges)])
                            (TextEdit #:range range #:newText new-name))))])]
         [#f (json-null)]))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/documentHighlight failed")]))

;; Prepare rename
(define (prepareRename id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char char)])
     (define-values (start end decl) (get-decl uri line char))

     (define this-doc (hash-ref open-docs (string->symbol uri)))

     (if (and decl (not (Decl-filename decl)))
         (success-response id (start/end->range this-doc start end))
         (success-response id (json-null)))]
    [_
     (error-response id INVALID-PARAMS "textDocument/documentHighlight failed")]))

;; Gets a list of Range objects indicating bindings related to the
;; symbol at the given position. If #:include-decl is #t, the list includes
;; the declaration. If #:include-decl is 'all, the list includes the declaration
;; and all bound occurrences.
(define (get-bindings uri decl)
  (define this-doc (hash-ref open-docs (string->symbol uri)))
  (define doc-trace (Doc-trace this-doc))

  (define doc-decls (send doc-trace get-sym-decls))
  (match-define (Decl req? id left right) decl)
  (define-values (bind-start bind-end bindings)
    (interval-map-ref/bounds doc-decls left #f))
  (if bindings
      (for/list ([range (in-set bindings)])
        (start/end->range this-doc (car range) (cdr range)))
      empty))

(define (get-decl uri line char)
  (define this-doc (hash-ref open-docs (string->symbol uri)))
  (define doc-trace (Doc-trace this-doc))

  (define pos (doc-pos this-doc line char))
  (define doc-decls (send doc-trace get-sym-decls))
  (define doc-bindings (send doc-trace get-sym-bindings))
  (define-values (start end maybe-decl)
    (interval-map-ref/bounds doc-bindings pos #f))
  (define-values (bind-start bind-end maybe-bindings)
    (interval-map-ref/bounds doc-decls pos #f))
  (if maybe-decl
      (values start end maybe-decl)
      (if maybe-bindings
          (values bind-start
                  bind-end
                  (interval-map-ref doc-bindings (car (set-first maybe-bindings)) #f))
          (values #f #f #f))))

;; Document Symbol request
(define (document-symbol id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)])
     (define this-doc (hash-ref open-docs (string->symbol uri)))

     (define results
       (dict-map (doc-get-symbols this-doc)
                 (λ (key value)
                   (match-define (cons start end) key)
                   (match-define (list text type) value)
                   (define kind (match type
                                  ['constant SymbolKind-Constant]
                                  ['string SymbolKind-String]
                                  ['symbol SymbolKind-Variable]))
                   (define range
                     (Range #:start (abs-pos->pos this-doc start)
                            #:end   (abs-pos->pos this-doc end)))
                   (SymbolInfo #:name text
                               #:kind kind
                               #:location (Location #:uri uri
                                                    #:range range)))))
     (success-response id results)]
    [_
     (error-response id INVALID-PARAMS "textDocument/documentSymbol failed")]))

;; Inlay Hint
(define (inlay-hint id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['range (Range  #:start start #:end end)])
     (success-response id '())]
    [_ (error-response id INVALID-PARAMS "textDocument/inlayHint failed")]))

;; Full document formatting request
(define (formatting! id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['options (as-FormattingOptions opts)])

     (define this-doc (hash-ref open-docs (string->symbol uri)))

     (define-values (st-ln st-ch) (doc-line/ch this-doc 0))
     (define-values (ed-ln ed-ch) (doc-line/ch this-doc (doc-endpos this-doc)))
     (success-response id (format! this-doc st-ln st-ch ed-ln ed-ch #:formatting-options opts))]
    [_
     (error-response id INVALID-PARAMS "textDocument/formatting failed")]))

;; Range Formatting request
(define (range-formatting! id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['range (Range  #:start (Pos #:line st-ln #:char st-ch)
                                 #:end (Pos #:line ed-ln #:char ed-ch))]
                 ['options (as-FormattingOptions opts)])
     (define this-doc (hash-ref open-docs (string->symbol uri)))
     (success-response id (format! this-doc st-ln st-ch ed-ln ed-ch #:formatting-options opts))]
    [_
     (error-response id INVALID-PARAMS "textDocument/rangeFormatting failed")]))

;; On-type formatting request
(define (on-type-formatting! id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ;; `position` is assumed to be the cursor position that after the edit.
                 ;; Therefore, `position - 1` is the position of `ch`.
                 ;; Also see issue https://github.com/jeapostrophe/racket-langserver/issues/111
                 ['position (Pos #:line line #:char char)]
                 ['ch ch]
                 ['options (as-FormattingOptions opts)])
     (define this-doc (hash-ref open-docs (string->symbol uri)))

     (define ch-pos (- (doc-pos this-doc line char) 1))
     (define-values (st-ln st-ch ed-ln ed-ch)
       (match ch
         ["\n"
          (define-values (st-ln st-ch) (doc-line/ch this-doc (doc-line-start-pos this-doc line)))
          (define-values (ed-ln ed-ch) (doc-line/ch this-doc (doc-line-end-pos this-doc line)))
          (values st-ln st-ch ed-ln ed-ch)]
         [_
          (define-values (st-ln st-ch)
            (doc-line/ch this-doc (or (doc-find-containing-paren this-doc (max 0 (sub1 ch-pos))) 0)))
          (define-values (ed-ln ed-ch) (doc-line/ch this-doc ch-pos))
          (values st-ln st-ch ed-ln ed-ch)]))
     (success-response id (format! this-doc st-ln st-ch ed-ln ed-ch
                                   #:on-type? #t
                                   #:formatting-options opts))]
    [_
     (error-response id INVALID-PARAMS "textDocument/onTypeFormatting failed")]))

(define (full-semantic-tokens id params)
  (match params
    [(hash* ['textDocument (DocIdentifier #:uri uri)])
     (define this-doc (hash-ref open-docs (string->symbol uri)))
     (success-response id (hash 'data (doc-range-tokens this-doc uri 0 (doc-endpos this-doc))))]
    [_ (error-response id INVALID-PARAMS "textDocument/semanticTokens/full failed")]))

(define (range-semantic-tokens id params)
  (match params
    [(hash* ['textDocument (DocIdentifier #:uri uri)]
            ['range (Range #:start (Pos #:line st-ln #:char st-ch)
                           #:end (Pos #:line ed-ln #:char ed-ch))])
     (define this-doc (hash-ref open-docs (string->symbol uri)))
     (define start-pos (doc-pos this-doc st-ln st-ch))
     (define end-pos (doc-pos this-doc ed-ln ed-ch))
     (success-response id (hash 'data (doc-range-tokens this-doc uri start-pos end-pos)))]
    [_ (error-response id INVALID-PARAMS "textDocument/semanticTokens/full failed")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [did-open! (jsexpr? . -> . void?)]
  [did-close! (jsexpr? . -> . void?)]
  [did-change! (jsexpr? . -> . void?)]
  [hover (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [code-action (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [completion (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [signatureHelp (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [definition (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [document-highlight (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [references (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [document-symbol (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [inlay-hint (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [rename _rename rename (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [prepareRename (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [formatting! (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [range-formatting! (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [on-type-formatting! (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [full-semantic-tokens (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [range-semantic-tokens (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]))

