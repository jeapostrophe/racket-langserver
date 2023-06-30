#lang racket/base
(require data/interval-map
         framework
         json
         racket/gui
         syntax-color/module-lexer
         syntax-color/racket-lexer
         drracket/check-syntax
         syntax/modread
         "check-syntax.rkt"
         "error-codes.rkt"
         "interfaces.rkt"
         "json-util.rkt"
         "path-util.rkt"
         "responses.rkt"
         "symbol-kinds.rkt"
         "docs-helpers.rkt"
         "doc-trace.rkt"
         "queue-only-latest.rkt"
         "documentation-parser.rkt")

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
        [(ContentChangeEvent #:range (Range #:start (Pos #:line st-ln #:char st-ch)
                                            #:end (Pos #:line ed-ln #:char ed-ch))
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
    (try-queue-check (uri->path uri) this-doc))
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
                      'range (start/end->Range doc-text start end))]
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
     #:when (uri-is-path? uri)
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define act (interval-map-ref (send doc-trace get-quickfixs)
                                   (Pos->abs-pos doc-text start)
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
                       (define symbols (get-symbols doc-text))
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

(define (get-symbols doc-text)
  (define text (send doc-text get-text))
  (define in (open-input-string text))
  (port-count-lines! in)
  (define lexer (get-lexer in))
  (define symbols (make-interval-map))
  (for ([lst (in-port (lexer-wrap lexer) in)]
        #:when (set-member? '(constant string symbol) (first (rest lst))))
    (match-define (list text type paren? start end) lst)
    (interval-map-set! symbols start end (list text type)))
  symbols)

;; Wrapper for in-port, returns a list or EOF.
(define ((lexer-wrap lexer) in)
  (define (eof-or-list txt type paren? start end)
    (if (eof-object? txt)
        eof
        (list txt type paren? start end)))
  (cond
    [(procedure? lexer)
     (define-values (txt type paren? start end)
       (lexer in))
     (eof-or-list txt type paren? start end)]
    [(cons? lexer)
     (define-values (txt type paren? start end backup mode)
       ((car lexer) in 0 (cdr lexer)))
     (set! lexer (cons (car lexer) mode))
     (eof-or-list txt type paren? start end)]))

;; Call module-lexer on an input port, then discard all
;; values except the lexer.
(define (get-lexer in)
  (match-define-values
    (_ _ _ _ _ _ lexer)
    (module-lexer in 0 #f))
  (cond
    [(procedure? lexer) lexer]
    [(cons? lexer) lexer]
    [(eq? lexer 'no-lang-line) racket-lexer]
    [(eq? lexer 'before-lang-line) racket-lexer]))

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
(define (get-def path doc-text id)
  (define collector
    (new (class (annotations-mixin object%)
           (define defs (make-hash))
           (define/public (get id) (hash-ref defs id #f))
           (define/override (syncheck:add-definition-target source-obj start end id mods)
             (hash-set! defs id (cons start end)))
           (super-new))))
  (define-values (src-dir _file _dir?)
    (split-path path))
  (define in (open-input-string (send doc-text get-text)))

  (define ns (make-base-namespace))
  (define-values (add-syntax done)
    (make-traversal ns src-dir))
  (parameterize ([current-annotations collector]
                 [current-namespace ns]
                 [current-load-relative-directory src-dir])
    (define stx (expand (with-module-reading-parameterization
                          (λ () (read-syntax path in)))))
    (add-syntax stx))
  (send collector get id))
(define (definition id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char char)])
     (define-values (start end decl) (get-decl uri line char))
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define result
       (match decl
         [#f (json-null)]
         [(Decl #f id start end)
          (Location #:uri uri
                    #:range (start/end->Range doc-text start end))]
         [(Decl path id 0 0)
          (define doc-text (new text%))
          (send doc-text load-file path)
          (match-define (cons start end) (get-def path doc-text id))
          (Location #:uri (path->uri path)
                    #:range (start/end->Range doc-text start end))]))
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
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define result
       (match decl
         [(Decl req? id left right)
          (define ranges
            (if req?
                (list (start/end->Range doc-text start end)
                      (start/end->Range doc-text left right))
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
     (define-values (start end decl) (get-decl uri line char))
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define result
       (match decl
         [(Decl filename id left right)
          (define ranges
            (if filename
                (list (start/end->Range doc-text start end)
                      (start/end->Range doc-text left right))
                (or (append (get-bindings uri decl)
                            (list (start/end->Range doc-text left right))))))
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
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define result
       (match decl
         [(Decl req? id left right)
          (cond [req? (json-null)]
                [else
                 (define ranges (cons (start/end->Range doc-text left right)
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
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (if (and decl (not (Decl-filename decl)))
         (success-response id (start/end->Range doc-text start end))
         (success-response id (json-null)))]
    [_
     (error-response id INVALID-PARAMS "textDocument/documentHighlight failed")]))

;; Gets a list of Range objects indicating bindings related to the
;; symbol at the given position. If #:include-decl is #t, the list includes
;; the declaration. If #:include-decl is 'all, the list includes the declaration
;; and all bound occurrences.
(define (get-bindings uri decl)
  (unless (uri-is-path? uri)
    (error 'get-bindings "uri is not a path"))
  (match-define (doc doc-text doc-trace)
    (hash-ref open-docs (string->symbol uri)))
  (define doc-decls (send doc-trace get-sym-decls))
  (match-define (Decl req? id left right) decl)
  (define-values (bind-start bind-end bindings)
    (interval-map-ref/bounds doc-decls left #f))
  (if bindings
      (for/list ([range (in-set bindings)])
        (start/end->Range doc-text (car range) (cdr range)))
      empty))

(define (get-decl uri line char)
  (unless (uri-is-path? uri)
    (error 'get-decl "uri is not a path"))
  (match-define (doc doc-text doc-trace)
    (hash-ref open-docs (string->symbol uri)))
  (define pos (line/char->pos doc-text line char))
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
     (unless (uri-is-path? uri)
       (error 'document-symbol "uri is not a path"))
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define results
       (dict-map (get-symbols doc-text)
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

;; Inlay Hint
(define (inlay-hint id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)])
     #:when (not (uri-is-path? uri))
     (error-response id INVALID-PARAMS "document-symbol: uri is not a path")]
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['range (Range  #:start start #:end end)])
     (success-response id '())]
    [_ (error-response id INVALID-PARAMS "textDocument/inlayHint failed")]))

;; Full document formatting request
(define (formatting! id params)
  (match params
    ;; We're ignoring 'options for now
    [(hash-table ['textDocument (DocIdentifier #:uri uri)])
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define end-pos (send doc-text last-position))
     (define start (abs-pos->Pos doc-text 0))
     (define end (abs-pos->Pos doc-text end-pos))
     (success-response id (format! uri start end))]
    [_
     (error-response id INVALID-PARAMS "textDocument/formatting failed")]))

;; Range Formatting request
(define (range-formatting! id params)
  (match params
    ;; XXX We're ignoring 'options' for now
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['range (Range  #:start start #:end end)])
     (success-response id (format! uri start end))]
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
     (define-values (start end)
       (match ch
         ["\n"
          (values
           (abs-pos->Pos doc-text (send doc-text paragraph-start-position line))
           (abs-pos->Pos doc-text (send doc-text paragraph-end-position line)))]
         [_
          (values
           (abs-pos->Pos doc-text (or (find-containing-paren pos (send doc-text get-text))
                                      0))
           (abs-pos->Pos doc-text pos))]))
     (success-response id (format! uri start end #:on-type? #t))]
    [_
     (error-response id INVALID-PARAMS "textDocument/onTypeFormatting failed")]))

;; Shared path for all formatting requests
(define (format! uri start end #:on-type? [on-type? #f])
  (unless (uri-is-path? uri)
    (error 'format! "uri is not a path"))
  (match-define (doc doc-text doc-trace)
    (hash-ref open-docs (string->symbol uri)))
  (define indenter (send doc-trace get-indenter))
  (define start-pos (Pos->abs-pos doc-text start))
  ;; Adjust for line endings (#92)
  (define end-pos (max start-pos (sub1 (Pos->abs-pos doc-text end))))
  (define start-line (send doc-text position-paragraph start-pos))
  (define end-line (send doc-text position-paragraph end-pos))
  (define mut-doc-text
    (if (is-a? doc-text racket:text%)
        (let ([r-text (new racket:text%)])
          (send r-text insert (send doc-text get-text))
          r-text)
        (send doc-text copy-self)))
  (define skip-this-line? #f)
  (if (eq? indenter 'missing) (json-null)
      (let loop ([line start-line])
        (define line-start (send mut-doc-text paragraph-start-position line))
        (define line-end (send mut-doc-text paragraph-end-position line))
        (for ([i (range line-start (add1 line-end))])
          (when (and (char=? #\" (send mut-doc-text get-character i))
                      (not (char=? #\\ (send mut-doc-text get-character (sub1 i)))))
            (set! skip-this-line? (not skip-this-line?))))
        (if (> line end-line)
            null
            (append (filter-map
                      identity
                      ;; NOTE: The order is important here.
                      ;; `remove-trailing-space!` deletes content relative to the initial document
                      ;; position. If we were to instead call `indent-line!` first and then
                      ;; `remove-trailing-space!` second, the remove step could result in
                      ;; losing user entered code.
                      (list (remove-trailing-space! mut-doc-text skip-this-line? line)
                            (indent-line! mut-doc-text indenter line #:on-type? on-type?)))
                    (loop (add1 line)))))))

;; Returns a TextEdit, or #f if the line is a part of multiple-line string
(define (remove-trailing-space! doc-text in-string? line)
  (define line-start (send doc-text paragraph-start-position line))
  (define line-end (send doc-text paragraph-end-position line))
  (define line-text (send doc-text get-text line-start line-end))
  (cond
    [(not in-string?)
     (define from (string-length (string-trim line-text #px"\\s+" #:left? #f)))
     (define to (string-length line-text))
     (send doc-text delete (+ line-start from) (+ line-start to))
     (TextEdit #:range (Range #:start (Pos #:line line #:char from)
                              #:end   (Pos #:line line #:char to))
               #:newText "")]
    [else #f]))

;; Returns a TextEdit, or #f if the line is already correct.
(define (indent-line! doc-text indenter line #:on-type? [on-type? #f])
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
        (or (indenter doc-text line-start)
            (send doc-text compute-racket-amount-to-indent line-start))
        (send doc-text compute-racket-amount-to-indent line-start)))
  (cond
    [(not (number? desired-spaces)) #f]
    [(= current-spaces desired-spaces) #f]
    [(and (not on-type?) (= line-length 0)) #f]
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
  [on-type-formatting! (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]))
