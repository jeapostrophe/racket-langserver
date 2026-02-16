#lang racket/base

;; This module provides a library for representing a document. It is designed
;; to be easy to use and single-threaded, containing only document-related logic
;; functions. For use in a multi-threaded LSP environment, it should be wrapped
;; by a structure that provides thread safety, such as `SafeDoc`.

(require "interfaces.rkt"
         "editor.rkt"
         "path-util.rkt"
         "doc-trace.rkt"
         "internal-types.rkt"
         "symbol-kinds.rkt"
         racket/match
         racket/contract
         racket/class
         racket/set
         racket/list
         racket/string
         racket/bool
         racket/dict
         data/interval-map
         syntax-color/module-lexer
         syntax-color/racket-lexer
         json
         "check-syntax.rkt"
         "docs-helpers.rkt"
         "documentation-parser.rkt"
         drracket/check-syntax
         racket/format)

(struct Doc
  (uri text trace version trace-version)
  #:mutable)

(define/contract (new-doc uri text [version 0])
  (->* (string? string?)
       (exact-nonnegative-integer?)
       Doc?)
  (define doc-text (new lsp-editor%))
  (send doc-text insert text 0)
  ;; the init trace should not be #f
  (define doc-trace (new build-trace% [src (uri->path uri)] [doc-text doc-text] [indenter #f]))
  (Doc uri doc-text doc-trace version #f))

(define/contract (doc-update-version! doc new-ver)
  (-> Doc? exact-nonnegative-integer? void?)
  (set-Doc-version! doc new-ver))

(define/contract (doc-update-uri! doc new-uri)
  (-> Doc? string? void?)
  (set-Doc-uri! doc new-uri))

(define/contract (doc-reset! doc new-text)
  (-> Doc? string? void?)
  (define doc-text (Doc-text doc))
  (define doc-trace (Doc-trace doc))

  (send doc-text erase)
  (send doc-trace reset)
  (send doc-text insert new-text 0))

(define/contract (doc-update! doc range text)
  (-> Doc? Range? string? void?)
  (define doc-text (Doc-text doc))
  (define doc-trace (Doc-trace doc))

  (define st-pos (doc-pos->abs-pos doc (Range-start range)))
  (define end-pos (doc-pos->abs-pos doc (Range-end range)))
  (define old-len (- end-pos st-pos))
  (define new-len (string-length text))

  ;; try reuse old information as the check-syntax can fail
  ;; and return the old build-trace% object
  (cond [(> new-len old-len) (send doc-trace expand end-pos (+ st-pos new-len))]
        [(< new-len old-len) (send doc-trace contract (+ st-pos new-len) end-pos)])
  (send doc-text replace text st-pos end-pos))

(define/contract (doc-expand uri doc-text)
  (-> string? (is-a?/c lsp-editor%) CSResult?)
  (check-syntax uri doc-text))

(define/contract (doc-update-trace! doc new-trace new-version)
  (-> Doc? (is-a?/c build-trace%) exact-nonnegative-integer? void?)
  (set-Doc-trace! doc new-trace)
  (set-Doc-trace-version! doc new-version))

(define/contract (doc-trace-latest? doc)
  (-> Doc? boolean?)
  (equal? (Doc-version doc) (Doc-trace-version doc)))

(define/contract (doc-walk-text trace text)
  (-> (is-a?/c build-trace%) string? void?)
  (send trace walk-text text))

(define/contract (doc-expand! doc)
  (-> Doc? boolean?)
  (define result (doc-expand (Doc-uri doc) (Doc-text doc)))
  (define new-trace (CSResult-trace result))
  (cond [(CSResult-succeed? result)
         (define text (CSResult-text result))
         (doc-update-trace! doc new-trace (Doc-version doc))
         (doc-walk-text new-trace text)
         #t]
        [else #f]))

(define/contract (doc-pos->abs-pos doc pos)
  (-> Doc? Pos? exact-nonnegative-integer?)
  (match-define (Pos #:line line #:char char) pos)
  (send (Doc-text doc) line/char->pos line char))

(define/contract (doc-abs-pos->pos doc pos)
  (-> Doc? exact-nonnegative-integer? Pos?)
  (match-define (list line char) (send (Doc-text doc) pos->line/char pos))
  (Pos #:line line #:char char))

(define/contract (doc-line-start-abs-pos doc line)
  (-> Doc? exact-nonnegative-integer? exact-nonnegative-integer?)
  (send (Doc-text doc) line-start-pos line))

(define/contract (doc-line-end-abs-pos doc line)
  (-> Doc? exact-nonnegative-integer? exact-nonnegative-integer?)
  (send (Doc-text doc) line-end-pos line))

(define/contract (doc-end-abs-pos doc)
  (-> Doc? exact-nonnegative-integer?)
  (send (Doc-text doc) end-pos))

(define/contract (doc-get-text doc)
  (-> Doc? string?)
  (send (Doc-text doc) get-text))

(define/contract (doc-copy-text-buffer doc)
  (-> Doc? (is-a?/c lsp-editor%))
  (send (Doc-text doc) copy))

;; TODO: Use lexer/token info here instead of scanning raw characters.
(define/contract (doc-find-containing-paren doc pos)
  (-> Doc? exact-nonnegative-integer? (or/c exact-nonnegative-integer? #f))
  (define text (send (Doc-text doc) get-text))
  (define l (string-length text))
  (cond
    [(>= pos l) #f]
    [else
     (let loop ([i pos] [p 0])
       (cond
         [(< i 0) #f]
         [(or (char=? (string-ref text i) #\() (char=? (string-ref text i) #\[))
          (if (> p 0) (loop (- i 1) (- p 1)) i)]
         [(or (char=? (string-ref text i) #\)) (char=? (string-ref text i) #\]))
          (loop (- i 1) (+ p 1))]
         [else (loop (- i 1) p)]))]))

(define (get-symbols doc-text)
  (define text (send doc-text get-text))
  (define in (open-input-string text))
  (port-count-lines! in)
  (define lexer (get-lexer in))
  (define symbols (make-interval-map))
  (for ([lst (in-port (lexer-wrap lexer) in)]
        #:when (set-member? '(constant string symbol) (second lst)))
    (match-define (list text type _paren? start end) lst)
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
     (define-values (txt type paren? start end _backup mode)
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
    [(eq? lexer 'before-lang-line) racket-lexer]
    [else racket-lexer]))

(define symbol-entry/c
  (list/c string? (or/c 'constant 'string 'symbol)))

;; TODO: Lexer positions start at 1.
;; Convert them to start at 0 so they match the rest of this file.
(define/contract (doc-get-symbols doc)
  (-> Doc? (interval-map-of symbol-entry/c))
  (get-symbols (Doc-text doc)))

;; definition BEG ;;

(define (get-def path doc-text id)
  (define collector
    (new (class (annotations-mixin object%)
           (define defs (make-hash))
           (define/public (get id) (hash-ref defs id #f))
           (define/override (syncheck:add-definition-target source-obj start end id mods)
             (hash-set! defs id (cons start end)))
           (super-new))))
  (define in (open-input-string (send doc-text get-text)))

  ;; expand-source handles traversal and adding syntax to collector
  (expand-source path in collector)
  (send collector get id))

(define (doc-get-definition-by-id path id)
  (define doc-text (new lsp-editor%))
  (send doc-text load-file path)
  (match-define (cons start end) (get-def path doc-text id))
  (Range (abs-pos->Pos doc-text start)
         (abs-pos->Pos doc-text end)))

;; definition END ;;

;; formatting ;;

;; Shared path for all formatting requests
(define (format! doc fmt-range
                 #:on-type? [on-type? #f]
                 #:formatting-options opts)
  (define doc-text (Doc-text doc))
  (define doc-trace (Doc-trace doc))

  (define indenter (send doc-trace get-indenter))
  (define start-pos (doc-pos->abs-pos doc (Range-start fmt-range)))
  ;; Adjust for line endings (#92)
  (define end-pos
    (max start-pos
         (sub1 (doc-pos->abs-pos doc (Range-end fmt-range)))))
  (define start-line (send doc-text at-line start-pos))
  (define end-line (send doc-text at-line end-pos))

  (define mut-doc-text (send doc-text copy))
  ;; replace \t with spaces at line `(sub1 start-line)`
  ;; as we cannot make `compute-racket-amount-to-indent`
  ;; to respect the given tab size
  (replace-tab! mut-doc-text
                (max 0 (sub1 start-line))
                (FormattingOptions-tab-size opts))

  (define indenter-wp (indenter-wrapper indenter mut-doc-text on-type?))
  (define skip-this-line? #f)

  (if (eq? indenter 'missing) (json-null)
      (let loop ([line start-line])
        (define line-start (send mut-doc-text line-start-pos line))
        (define line-end (send mut-doc-text line-end-pos line))
        (for ([i (range line-start (add1 line-end))])
          (when (and (char=? #\" (send mut-doc-text get-char i))
                     (not (char=? #\\ (send mut-doc-text get-char (sub1 i)))))
            (set! skip-this-line? (not skip-this-line?))))
        (if (> line end-line)
            null
            (append (filter-map
                      values
                      ;; NOTE: The order is important here.
                      ;; `remove-trailing-space!` deletes content relative to the initial document
                      ;; position. If we were to instead call `indent-line!` first and then
                      ;; `remove-trailing-space!` second, the remove step could result in
                      ;; losing user entered code.
                      (list (if (false? (FormattingOptions-trim-trailing-whitespace opts))
                                #f
                                (remove-trailing-space! mut-doc-text skip-this-line? line))
                            (indent-line! mut-doc-text indenter-wp line)))
                    (loop (add1 line)))))))

(define (replace-tab! doc-text line tabsize)
  (define old-line (send doc-text get-line line))
  (define spaces (make-string tabsize #\space))
  (define new-line-str (string-replace old-line "\t" spaces))
  (send doc-text replace-in-line
        new-line-str
        line 0 (string-length old-line)))

(define (indenter-wrapper indenter doc-text on-type?)
  (λ (line)
    (cond [(and (not on-type?)
                (= (send doc-text line-start-pos line)
                   (send doc-text line-end-pos line)))
           #f]
          [else
           (define line-start (send doc-text line-start-pos line))
           (if indenter
               (or (send doc-text run-indenter indenter line-start)
                   (send doc-text compute-racket-amount-to-indent line-start))
               (send doc-text compute-racket-amount-to-indent line-start))])))

;; Returns a TextEdit, or #f if the line is a part of multiple-line string
(define (remove-trailing-space! doc-text in-string? line)
  (define line-text (send doc-text get-line line))
  (cond
    [(not in-string?)
     (define from (string-length (string-trim line-text #px"\\s+" #:left? #f)))
     (define to (string-length line-text))
     (send doc-text replace-in-line "" line from to)
     (TextEdit #:range (Range #:start (Pos #:line line #:char from)
                              #:end (Pos #:line line #:char to))
               #:newText "")]
    [else #f]))

(define (extract-indent-string content)
  (define len
    (or (for/first ([(c i) (in-indexed content)]
                    #:when (not (char-whitespace? c)))
          i)
        (string-length content)))
  (substring content 0 len))

;; Returns a TextEdit, or #f if the line is already correct.
(define (indent-line! doc-text indenter line)
  (define content (send doc-text get-line line))
  (define old-indent-string (extract-indent-string content))
  (define expect-indent (indenter line))
  (define really-indent (string-length old-indent-string))
  (define has-tab? (string-contains? old-indent-string "\t"))

  (cond [(false? expect-indent) #f]
        [(and (= expect-indent really-indent) (not has-tab?)) #f]
        [else
         (define new-text (make-string expect-indent #\space))
         (send doc-text replace-in-line new-text line 0 really-indent)
         (TextEdit #:range (Range #:start (Pos #:line line #:char 0)
                                  #:end (Pos #:line line #:char really-indent))
                   #:newText new-text)]))

(define (token-type-encoding token)
  (index-of *semantic-token-types* (SemanticToken-type token)))

(define (token-modifier-encoding token)
  (define indexes (indexes-where *semantic-token-modifiers*
                                 (λ (m) (memq m (SemanticToken-modifiers token)))))
  ;; build a bit flag of the modifiers of `token`.
  ;;
  ;; equivalent to C family pseudocode
  ;;
  ;; uint32_t flag = 0
  ;; for index in indexes:
  ;;   flag = flag | (1 << index)
  ;; return flag
  ;;
  ;; But the integer bit width is ignored here, because
  ;; the *semantic-token-modifiers* is very small.
  (for/sum ([index indexes])
    (expt 2 index)))

;; encode `token` using relative encoding
;;
;; each token is encoded as five integers (copied from lsp specificatioin 3.17):
;; * deltaLine: token line number, relative to the start of the previous token
;; * deltaStart: token start character, relative to the start of the previous token
;;               (relative to 0 or the previous token’s start if they are on the same line)
;; * length: the length of the token.
;; * tokenType: will be looked up in SemanticTokensLegend.tokenTypes.
;;              We currently ask that tokenType < 65536.
;; * tokenModifiers: each set bit will be looked up in SemanticTokensLegend.tokenModifiers
;;
;; for the first token, its previous token is defined as a zero length fake token which
;; has line number 0 and character position 0.
(define (token-encoding doc token prev-pos)
  (match-define (list line ch) (send (Doc-text doc) pos->line/char (SemanticToken-start token)))
  (match-define (list prev-line prev-ch) (send (Doc-text doc) pos->line/char prev-pos))
  (define delta-line (- line prev-line))
  (define delta-start
    (if (= line prev-line)
        (- ch prev-ch)
        ch))
  (define len (- (SemanticToken-end token) (SemanticToken-start token)))
  (define type (token-type-encoding token))
  (define modifier (token-modifier-encoding token))
  (values delta-line delta-start len type modifier))

;; get the tokens whose range are contained in interval [pos-start, pos-end)
;; the tokens whose range intersects the given range is included.
;; the previous token of the first token in the result is defined as a zero length fake token which
;; has line number 0 and character position 0.
(define/contract (doc-range-tokens doc range)
  (-> Doc? Range? list?)
  (define tokens (send (Doc-trace doc) get-semantic-tokens))
  (define pos-start (doc-pos->abs-pos doc (Range-start range)))
  (define pos-end (doc-pos->abs-pos doc (Range-end range)))
  (define tokens-in-range
    (filter-not (λ (tok) (or (<= (SemanticToken-end tok) pos-start)
                             (>= (SemanticToken-start tok) pos-end)))
                tokens))
  (for/fold ([result '()]
             [prev-pos 0]
             #:result (flatten (reverse result)))
            ([token tokens-in-range])
    (define-values (delta-line delta-start len type modifier)
      (token-encoding doc token prev-pos))
    (values (cons (list delta-line delta-start len type modifier) result)
            (SemanticToken-start token))))

(define (-doc-find-token text pos)
  (define ch (string-ref text pos))
  (cond [(= pos 0) (list ch)]
        [(or (char=? ch #\") (char-whitespace? ch)) '()]
        [else (cons ch (-doc-find-token text (sub1 pos)))]))

;; TODO: Use lexer/token data here instead of manual character scanning.
(define/contract (doc-guess-token doc pos)
  (-> Doc? exact-nonnegative-integer? string?)
  (list->string (reverse (-doc-find-token (send (Doc-text doc) get-text) pos))))

(define (start/end->range doc start end)
  (Range #:start (doc-abs-pos->pos doc start) #:end (doc-abs-pos->pos doc end)))

(define/contract (doc-hover doc pos)
  (-> Doc? Pos? (or/c Hover? #f))
  (define doc-trace (Doc-trace doc))
  (define hovers (send doc-trace get-hovers))
  (define pos* (doc-pos->abs-pos doc pos))
  (define-values (start end text)
    (interval-map-ref/bounds hovers pos* #f))
  (match-define (list link tag)
    (interval-map-ref (send doc-trace get-docs) pos* (list #f #f)))
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
             (and sigs
                  (~a "```\n"
                      (string-join sigs "\n")
                      (if args-descr
                          (~a "\n" args-descr)
                          "")
                      "\n```\n---\n")))
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
           (Hover #:contents contents
                  #:range (start/end->range doc start end))]
          [else #f]))
  result)

(define/contract (doc-code-action doc range)
  (-> Doc? Range? (listof CodeAction?))
  (define doc-trace (Doc-trace doc))
  (define act
    (interval-map-ref (send doc-trace get-quickfixs)
                      (doc-pos->abs-pos doc (Range-start range))
                      #f))
  (if act (list act) (list)))

(define/contract (doc-signature-help doc pos)
  (-> Doc? Pos? (or/c SignatureHelp? #f))
  (define doc-trace (Doc-trace doc))

  (define pos* (doc-pos->abs-pos doc pos))
  (define new-pos (doc-find-containing-paren doc (- pos* 1)))
  (define result
    (cond [new-pos
           (define maybe-tag (interval-map-ref (send doc-trace get-docs) (+ new-pos 1) #f))
           (define tag
             (cond [maybe-tag (last maybe-tag)]
                   [else
                    (define symbols (doc-get-symbols doc))
                    (define-values (start end symbol)
                      (interval-map-ref/bounds symbols (+ new-pos 2) #f))
                    (cond [symbol
                           (id-to-tag (first symbol) doc-trace)]
                          [else #f])]))
           (cond [tag
                  (match-define (list sigs docs) (get-docs-for-tag tag))
                  (if sigs
                      (SignatureHelp
                        #:signatures
                        (map (lambda (sig)
                               (SignatureInformation
                                 #:label sig
                                 #:documentation (or docs "")))
                             sigs))
                      #f)]
                 [else #f])]
          [else #f]))
  result)

;; Get the declaration at a given position in the document.
;; Returns (values start end decl) where decl is a Decl or #f.
(define/contract (doc-get-decl doc pos)
  (-> Doc?
      Pos?
      (values (or/c exact-nonnegative-integer? #f)
              (or/c exact-nonnegative-integer? #f)
              (or/c Decl? #f)))
  (define doc-trace (Doc-trace doc))
  (define pos* (doc-pos->abs-pos doc pos))
  (define doc-decls (send doc-trace get-sym-decls))
  (define doc-bindings (send doc-trace get-sym-bindings))
  (define-values (start end maybe-decl)
    (interval-map-ref/bounds doc-bindings pos* #f))
  (define-values (bind-start bind-end maybe-bindings)
    (interval-map-ref/bounds doc-decls pos* #f))
  (if maybe-decl
      (values start end maybe-decl)
      (if maybe-bindings
          (values bind-start
                  bind-end
                  (interval-map-ref doc-bindings (car (set-first maybe-bindings)) #f))
          (values #f #f #f))))

;; Get binding ranges for a declaration.
;; Returns a list of Range hashes.
(define/contract (doc-get-bindings doc decl)
  (-> Doc? Decl? (listof Range?))
  (define doc-trace (Doc-trace doc))
  (define doc-decls (send doc-trace get-sym-decls))
  (match-define (Decl req? id left right) decl)
  (define-values (bind-start bind-end bindings)
    (interval-map-ref/bounds doc-decls left #f))
  (if bindings
      (for/list ([range (in-set bindings)])
        (start/end->range doc (car range) (cdr range)))
      empty))

;; Completion: returns a CompletionList.
(define/contract (doc-completion doc pos)
  (-> Doc? Pos? CompletionList?)
  (define doc-trace (Doc-trace doc))
  (define pos* (sub1 (doc-pos->abs-pos doc pos)))
  (define completions
    (append (send doc-trace get-completions)
            (send doc-trace get-online-completions (doc-guess-token doc pos*))))
  (define result
    (for/list ([completion (in-list completions)])
      (CompletionItem #:label (symbol->string completion))))
  (CompletionList #:isIncomplete #t
                  #:items result))

;; Definition: returns a Location, or #f.
(define/contract (doc-definition doc uri pos)
  (-> Doc? string? Pos? (or/c Location? #f))
  (define-values (start end decl) (doc-get-decl doc pos))
  (match decl
    [#f #f]
    [(Decl #f id start end)
     (Location #:uri uri
               #:range (start/end->range doc start end))]
    [(Decl path id 0 0)
     (Location #:uri (path->uri path)
               #:range (doc-get-definition-by-id path id))]))

;; References: returns a list of Locations, or #f.
(define/contract (doc-references doc uri pos include-decl?)
  (-> Doc? string? Pos? boolean? (or/c (listof Location?) #f))
  (define-values (start end decl) (doc-get-decl doc pos))
  (match decl
    [(Decl req? id left right)
     (define ranges
       (if req?
           (list (start/end->range doc start end)
                 (start/end->range doc left right))
           (or (doc-get-bindings doc decl))))
     (for/list ([range (in-list ranges)])
       (Location #:uri uri #:range range))]
    [#f #f]))

;; Document Highlight: returns a list of DocumentHighlights, or #f.
(define/contract (doc-highlights doc pos)
  (-> Doc? Pos? (or/c (listof DocumentHighlight?) #f))
  (define-values (start end decl) (doc-get-decl doc pos))
  (match decl
    [(Decl filename id left right)
     (define ranges
       (if filename
           (list (start/end->range doc start end)
                 (start/end->range doc left right))
           (or (append (doc-get-bindings doc decl)
                       (list (start/end->range doc left right))))))
     (for/list ([range (in-list ranges)])
       (DocumentHighlight #:range range))]
    [#f #f]))

;; Rename: returns a WorkspaceEdit, or #f.
(define/contract (doc-rename doc uri pos new-name)
  (-> Doc? string? Pos? string? (or/c WorkspaceEdit? #f))
  (define-values (start end decl) (doc-get-decl doc pos))
  (match decl
    [(Decl req? id left right)
     (cond [req? #f]
           [else
            (define ranges (cons (start/end->range doc left right)
                                 (doc-get-bindings doc decl)))
            (WorkspaceEdit
              #:changes
              (hasheq (string->symbol uri)
                      (for/list ([range (in-list ranges)])
                        (TextEdit #:range range #:newText new-name))))])]
    [#f #f]))

;; Prepare Rename: returns a Range, or #f.
(define/contract (doc-prepare-rename doc pos)
  (-> Doc? Pos? (or/c Range? #f))
  (define-values (start end decl) (doc-get-decl doc pos))
  (if (and decl (not (Decl-filename decl)))
      (start/end->range doc start end)
      #f))

;; Document Symbols: returns a list of SymbolInformation.
(define/contract (doc-symbols doc uri)
  (-> Doc? string? (listof SymbolInformation?))
  (dict-map (doc-get-symbols doc)
            (λ (key value)
              (match-define (cons start end) key)
              (match-define (list text type) value)
              (define kind (match type
                             ['constant SymbolKind-Constant]
                             ['string SymbolKind-String]
                             ['symbol SymbolKind-Variable]))
              (define range
                (Range #:start (doc-abs-pos->pos doc start)
                       #:end (doc-abs-pos->pos doc end)))
              (SymbolInformation
                #:name text
                #:kind kind
                #:location (Location #:uri uri
                                     #:range range)))))

(provide Doc
         Doc-version
         Doc-uri
         new-doc
         doc-update!
         doc-reset!
         doc-update-version!
         doc-update-uri!
         doc-pos->abs-pos
         doc-end-abs-pos
         doc-get-text
         doc-copy-text-buffer
         doc-abs-pos->pos
         doc-line-start-abs-pos
         doc-line-end-abs-pos
         doc-find-containing-paren
         doc-get-symbols
         doc-get-definition-by-id
         format!
         doc-range-tokens
         doc-guess-token
         doc-expand
         doc-update-trace!
         doc-trace-latest?
         doc-expand!
         doc-walk-text
         doc-hover
         doc-code-action
         doc-signature-help
         doc-get-decl
         doc-get-bindings
         doc-completion
         doc-definition
         doc-references
         doc-highlights
         doc-rename
         doc-prepare-rename
         doc-symbols
         )

