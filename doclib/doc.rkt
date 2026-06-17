#lang racket/base

;; This module provides a library for representing a document. It is designed
;; to be easy to use and single-threaded, containing only document-related logic
;; functions. For use in a multi-threaded LSP environment, it should be wrapped
;; by a structure that provides thread safety, such as `SafeDoc`.

(require "../common/interfaces.rkt"
         "editor.rkt"
         "../common/path-util.rkt"
         "lazy-cache.rkt"
         "doc-trace.rkt"
         "formatting.rkt"
         "internal-types.rkt"
         "lexer.rkt"
         (only-in "lexer/state.rkt"
                  lexer-state-body-forest)
         (only-in "lexer/token-tree.rkt"
                  Token-Leaf? Token-Leaf-span
                  Token-List? Token-List-children
                  Token-Prefix-Tree?
                  Token-Forest-nodes
                  token-node-children
                  token-node-start
                  token-node-end
                  non-skippable-node?
                  token-leaf-type?)
         "doc-lang.rkt"
         racket/match
         racket/contract
         racket/class
         racket/set
         racket/list
         racket/string
         data/interval-map
         "check-syntax.rkt"
         "external/resyntax.rkt"
         "docs-helpers.rkt"
         "documentation-parser.rkt"
         drracket/check-syntax
         racket/format)

(struct/contract Doc
  ([uri string?]
   [text (is-a?/c lsp-editor%)]
   [trace (is-a?/c build-trace%)]
   [version exact-nonnegative-integer?]
   [trace-version (or/c false/c exact-nonnegative-integer?)]
   [resyntax-results (listof Resyntax-Result?)]
   [lexer-state (lazy-cache-of LexerState?)])
  #:mutable)

(define/contract (make-doc uri text [version 0])
  (->* (string? string?)
       (exact-nonnegative-integer?)
       Doc?)
  (define doc-text (new lsp-editor%))
  (send doc-text insert text 0)
  (define lexer-state (build-lexer-state text uri))
  ;; the init trace should not be #f
  (define doc-trace
    (new build-trace%
      [src (uri->path uri)]
      [doc-text doc-text]
      [lexer-state lexer-state]))
  (Doc uri doc-text doc-trace version #f (list) (make-lazy-cache)))

(define (invalidate-resyntax-results! doc)
  (set-Doc-resyntax-results! doc (list)))

(define (invalidate-lexer-state! doc)
  (lazy-cache-invalidate! (Doc-lexer-state doc)))

(define/contract (doc-get-resyntax-results doc)
  (-> Doc? (listof Resyntax-Result?))
  (Doc-resyntax-results doc))

(define/contract (doc-update-resyntax-result! doc results)
  (-> Doc? (listof Resyntax-Result?) void?)
  (set-Doc-resyntax-results! doc results))

(define/contract (resyntax-result->diag doc res)
  (-> Doc? Resyntax-Result? Diagnostic?)
  (define range
    (Range #:start (doc-abs-pos->pos doc (Resyntax-Result-start res))
           #:end (doc-abs-pos->pos doc (Resyntax-Result-end res))))
  (Diagnostic #:range range
              #:severity DiagnosticSeverity-Information
              #:source "Resyntax"
              #:message (format "[~a] ~a" (Resyntax-Result-rule-name res) (Resyntax-Result-message res))))

(define/contract (resyntax-result->code-action doc res)
  (-> Doc? Resyntax-Result? CodeAction?)
  (define diag (resyntax-result->diag doc res))
  (define doc-uri (Doc-uri doc))
  (define range (Diagnostic-range diag))
  (CodeAction
    #:title (format "Apply rule [~a]" (Resyntax-Result-rule-name res))
    #:kind "quickfix"
    #:diagnostics (list diag)
    #:isPreferred #f
    #:edit (WorkspaceEdit
             #:changes
             (hasheq (string->symbol doc-uri)
                     (list (TextEdit #:range range
                                     #:newText (Resyntax-Result-new-text res)))))))

(define/contract (doc-resyntax doc)
  (-> Doc? (listof Resyntax-Result?))
  (run-resyntax (send (Doc-text doc) get-text) (Doc-uri doc)))

(define/contract (doc-resyntax! doc)
  (-> Doc? void?)
  (define results (doc-resyntax doc))
  (doc-update-resyntax-result! doc results))

(define/contract (doc-resyntax-available?)
  (-> boolean?)
  (resyntax-available?))

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

  (invalidate-resyntax-results! doc)
  (invalidate-lexer-state! doc)
  (send doc-text erase)
  (send doc-trace reset)
  (send doc-text insert new-text 0))

(define (text-edit-start doc edit)
  (doc-pos->abs-pos doc (Range-start (TextEdit-range edit))))

(define (text-edit-end doc edit)
  (doc-pos->abs-pos doc (Range-end (TextEdit-range edit))))

(define (sort-text-edits-descending doc edits)
  (sort edits
        (lambda (a b)
          (define a-start (text-edit-start doc a))
          (define b-start (text-edit-start doc b))
          (if (= a-start b-start)
              (> (text-edit-end doc a) (text-edit-end doc b))
              (> a-start b-start)))))

(define (doc-apply-absolute-edit! doc start end text)
  (define doc-text (Doc-text doc))
  (define doc-trace (Doc-trace doc))
  (define old-len (- end start))
  (define new-len (string-length text))

  ;; try reuse old information as the check-syntax can fail
  ;; for updated code.
  (cond [(> new-len old-len) (send doc-trace expand end (+ start new-len))]
        [(< new-len old-len) (send doc-trace contract (+ start new-len) end)])
  (send doc-text replace text start end))

(define (doc-check-non-overlapping-edits! doc edits-descending-by-start)
  (for ([later-start-edit (in-list edits-descending-by-start)]
        [earlier-start-edit (in-list (rest edits-descending-by-start))])
    (define later-start (text-edit-start doc later-start-edit))
    (define earlier-start (text-edit-start doc earlier-start-edit))
    (define earlier-end (text-edit-end doc earlier-start-edit))
    (when (> earlier-end later-start)
      (error 'doc-apply-edits!
             "overlapping edits: ~a..~a, next starts at ~a"
             earlier-start
             earlier-end
             earlier-start))))

(define/contract (doc-apply-edit! doc range text)
  (-> Doc? Range? string? void?)
  (invalidate-resyntax-results! doc)
  (invalidate-lexer-state! doc)
  (define start (doc-pos->abs-pos doc (Range-start range)))
  (define end (doc-pos->abs-pos doc (Range-end range)))
  (doc-apply-absolute-edit! doc start end text))

(define/contract (doc-apply-edits! doc edits)
  (-> Doc? (listof TextEdit?) void?)
  (unless (empty? edits)
    (invalidate-resyntax-results! doc)
    (invalidate-lexer-state! doc)
    ;; Apply from the end of the document so earlier edits do not shift
    ;; the positions of later edits.
    (define edits-descending-by-start
      (sort-text-edits-descending doc edits))
    (doc-check-non-overlapping-edits! doc edits-descending-by-start)
    (for ([edit (in-list edits-descending-by-start)])
      (doc-apply-absolute-edit! doc
                                (text-edit-start doc edit)
                                (text-edit-end doc edit)
                                (TextEdit-newText edit)))))

(define/contract (doc-expand uri doc-text lexer-state)
  (-> string? (is-a?/c lsp-editor%) LexerState? CSResult?)
  (check-syntax uri doc-text lexer-state))

(define/contract (doc-update-trace! doc new-trace new-version)
  (-> Doc? (is-a?/c build-trace%) exact-nonnegative-integer? void?)
  (set-Doc-trace! doc new-trace)
  (set-Doc-trace-version! doc new-version))

(define/contract (doc-trace-latest? doc)
  (-> Doc? boolean?)
  (equal? (Doc-version doc) (Doc-trace-version doc)))

(define/contract (doc-expand! doc)
  (-> Doc? boolean?)
  (define result
    (doc-expand (Doc-uri doc)
                (Doc-text doc)
                (doc-lexer-state doc)))
  (define new-trace (CSResult-trace result))
  (cond [(CSResult-succeed? result)
         (doc-update-trace! doc new-trace (Doc-version doc))
         #t]
        [else #f]))

(define/contract (doc-pos->abs-pos doc pos)
  (-> Doc? Pos? exact-nonnegative-integer?)
  (send (Doc-text doc) line/char->pos (Pos-line pos) (Pos-char pos)))

(define/contract (doc-abs-pos->pos doc pos)
  (-> Doc? exact-nonnegative-integer? Pos?)
  (match-define (list line char) (send (Doc-text doc) pos->line/char pos))
  (Pos line char))

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

(define/contract (doc-diagnostics doc)
  (-> Doc? (listof Diagnostic?))
  (append (set->list (send (Doc-trace doc) get-warn-diags))
          (for/list ([res (in-list (doc-get-resyntax-results doc))])
            (resyntax-result->diag doc res))))

(define/contract (doc-copy-text-buffer doc)
  (-> Doc? (is-a?/c lsp-editor%))
  (send (Doc-text doc) copy))

(define (interval-map-iterate-least/end>?/fallback intervals end)
  (let loop ([iter (interval-map-iterate-first intervals)])
    (cond
      [(not iter) #f]
      [else
       (match-define (cons _ interval-end)
         (interval-map-iterate-key intervals iter))
       (if (> interval-end end)
           iter
           (loop (interval-map-iterate-next intervals iter)))])))

(define maybe-interval-map-iterate-least/end>?
  (dynamic-require 'data/interval-map
                   'interval-map-iterate-least/end>?
                   (lambda () interval-map-iterate-least/end>?/fallback)))

(define (interval-map-overlap-values intervals start end)
  (let loop ([iter (maybe-interval-map-iterate-least/end>? intervals start)]
             [values (list)])
    (cond
      [(not iter) (reverse values)]
      [else
       (match-define (cons interval-start _)
         (interval-map-iterate-key intervals iter))
       (if (>= interval-start end)
           (reverse values)
           (loop (interval-map-iterate-next intervals iter)
                 (cons (interval-map-iterate-value intervals iter) values)))])))

;; Return the absolute position of the opening delimiter of the innermost form
;; that contains `pos`, or #f when `pos` is outside any parsed form.
(define/contract (doc-find-containing-paren doc pos)
  (-> Doc? exact-nonnegative-integer? (or/c exact-nonnegative-integer? #f))
  (lexer-state-containing-open-paren (doc-lexer-state doc) pos))

;; Cache lexer-derived state lazily. Query paths may build a cache miss while
;; the caller already holds whatever lock stabilizes the document state.
(define (doc-build-lexer-state doc)
  (build-lexer-state (send (Doc-text doc) get-text) (Doc-uri doc)))

(define (doc-lexer-state doc)
  (call-with-lazy-cache!
    (Doc-lexer-state doc)
    (lambda ()
      (doc-build-lexer-state doc))))

(define (doc-lexer-snapshot doc)
  (LexerState-snapshot (doc-lexer-state doc)))

(define (doc-language-policy doc)
  (LexerState-language-policy (doc-lexer-state doc)))

(define (doc-language-body-mode doc)
  (Language-Policy-body-mode (doc-language-policy doc)))

(define (doc-body-forest doc)
  (lexer-state-body-forest (doc-lexer-state doc)))

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

(define/contract (doc-get-definition-by-id path id)
  (-> path-string? symbol? Range?)
  (define doc-text (new lsp-editor%))
  (send doc-text load-file path)
  (match-define (cons start end) (get-def path doc-text id))
  (Range (abs-pos->Pos doc-text start)
         (abs-pos->Pos doc-text end)))

;; definition END ;;

;; formatting ;;

(define (doc-src-dir doc)
  (with-handlers ([exn:fail? (λ (_exn) #f)])
    (define doc-path (uri->path (Doc-uri doc)))
    (define-values (base _name _must-be-dir?) (split-path doc-path))
    base))

(define (formatting-range->lines doc-text fmt-range)
  (define last-line (send doc-text at-line (send doc-text end-pos)))
  (define requested-start-line (Pos-line (Range-start fmt-range)))
  (define requested-end-line (Pos-line (Range-end fmt-range)))
  (define start-line (min requested-start-line last-line))
  ;; PR #92 established that formatting ranges use inclusive end-line semantics,
  ;; even though the LSP range end itself is exclusive.
  (values start-line (max start-line (min requested-end-line last-line))))

(define (doc-sexp-language? doc)
  (eq? 'sexp (doc-language-body-mode doc)))

(define (doc-on-type-formatting-range doc pos ch)
  (define ch-pos (max 0 (sub1 (doc-pos->abs-pos doc pos))))
  (define current-line (Pos-line pos))
  (define current-line-start-pos (doc-line-start-abs-pos doc current-line))
  (define current-line-end-pos (doc-line-end-abs-pos doc current-line))

  (define (current-line-range)
    (Range (doc-abs-pos->pos doc current-line-start-pos)
           (doc-abs-pos->pos doc current-line-end-pos)))

  (define (containing-form-range)
    (define raw-pos (max 0 (sub1 ch-pos)))
    (define token (doc-token-at doc raw-pos))
    (define query-pos
      (if (and token (eq? 'close-paren (LexerEntry-type token)))
          (add1 raw-pos)
          raw-pos))
    (define maybe-paren-pos (doc-find-containing-paren doc query-pos))
    (define start-pos (if maybe-paren-pos maybe-paren-pos 0))
    (Range (doc-abs-pos->pos doc start-pos)
           (doc-abs-pos->pos doc current-line-end-pos)))

  (match ch
    ["\n" (current-line-range)]
    [")" (containing-form-range)]
    ["]" (containing-form-range)]
    [_ (current-line-range)]))

;; Shared path for all formatting requests
(define/contract (doc-format-edits doc fmt-range
                                   #:formatting-options _opts
                                   #:on-type? [on-type? #f])
  (->* (Doc? Range? #:formatting-options FormattingOptions?)
       (#:on-type? boolean?)
       (or/c (listof TextEdit?) #f))
  (define doc-text (Doc-text doc))
  (define-values (start-line end-line)
    (formatting-range->lines doc-text fmt-range))
  (define text (send doc-text get-text))
  (define policy (doc-language-policy doc))
  (cond
    [(Language-Policy-format? policy)
     (formatting text
                 start-line
                 end-line
                 #:src-dir (doc-src-dir doc)
                 #:interactive? on-type?)]
    [else '()]))

(define/contract (doc-on-type-format-edits doc pos ch
                                           #:formatting-options opts)
  (->* (Doc? Pos? string? #:formatting-options FormattingOptions?)
       ()
       (or/c (listof TextEdit?) #f))
  (cond
    [(doc-sexp-language? doc)
     (doc-format-edits doc
                       (doc-on-type-formatting-range doc pos ch)
                       #:on-type? #t
                       #:formatting-options opts)]
    [else '()]))

;; get the tokens whose range are contained in interval [pos-start, pos-end)
;; the tokens whose range intersects the given range is included.
;; the previous token of the first token in the result is defined as a zero length fake token which
;; has line number 0 and character position 0.
(define/contract (doc-range-tokens doc range)
  (-> Doc? Range? (listof SemanticToken?))
  (define pos-start (doc-pos->abs-pos doc (Range-start range)))
  (define pos-end (doc-pos->abs-pos doc (Range-end range)))
  (split-semantic-tokens-by-line
    doc
    (filter (λ (token)
              (char-range-intersect?
                (SemanticToken-start token)
                (SemanticToken-end token)
                pos-start pos-end))
            (doc-semantic-tokens doc))))

(define (doc-sexp-comment-semantic-tokens doc)
  (for/list ([span (in-list (lexer-state-sexp-comment-spans (doc-lexer-state doc)))])
    (SemanticToken (CharRange-start span) (CharRange-end span) SemanticTokenType-comment '())))

(define (doc-semantic-tokens doc)
  (define trace-tokens
    (sort (send (Doc-trace doc) get-semantic-tokens) < #:key SemanticToken-start))
  (define sexp-comment-tokens
    (sort (doc-sexp-comment-semantic-tokens doc) < #:key SemanticToken-start))
  (merge-semantic-tokens trace-tokens sexp-comment-tokens))

(define (merge-semantic-tokens tokens high-priority-tokens [merged '()])
  (define (continue-merge tokens merged)
    (cond [(empty? tokens)
           (reverse merged)]
          [else
           (continue-merge (rest tokens) (cons (car tokens) merged))]))

  (define (merge-non-empty-stream tokens high-priority-tokens merged)
    (define ht (car high-priority-tokens))
    (define t (car tokens))
    (define ht-start (SemanticToken-start ht))
    (define ht-end (SemanticToken-end ht))
    (define t-start (SemanticToken-start t))
    (define t-end (SemanticToken-end t))
    (cond [(<= t-end ht-start)
           (merge-semantic-tokens (cdr tokens) high-priority-tokens (cons t merged))]
          [(<= ht-end t-start)
           (merge-semantic-tokens tokens (cdr high-priority-tokens) (cons ht merged))]
          [else
           (merge-semantic-tokens (cdr tokens) high-priority-tokens merged)]))

  (cond [(empty? high-priority-tokens)
         (continue-merge tokens merged)]
        [(empty? tokens)
         (continue-merge high-priority-tokens merged)]
        [else
         (merge-non-empty-stream tokens high-priority-tokens merged)]))

(define (split-semantic-tokens-by-line doc tokens)
  (for*/list ([token (in-list tokens)]
              [tstart (in-value (SemanticToken-start token))]
              [tend (in-value (SemanticToken-end token))]
              [type (in-value (SemanticToken-type token))]
              [modifiers (in-value (SemanticToken-modifiers token))]
              [start-line (in-value (Pos-line (doc-abs-pos->pos doc tstart)))]
              [end-line (in-value (Pos-line (doc-abs-pos->pos doc (sub1 tend))))]
              [line (in-range start-line (add1 end-line))]
              [start (in-value (max tstart (doc-line-start-abs-pos doc line)))]
              [end (in-value (min tend (doc-line-end-abs-pos doc line)))])
    (SemanticToken start end type modifiers)))

(define/contract (doc-token-at doc pos)
  (-> Doc? exact-nonnegative-integer? (or/c LexerEntry? #f))
  (lexer-snapshot-token-at (doc-lexer-snapshot doc) pos))

(define/contract (doc-token-prefix-at doc pos)
  (-> Doc? exact-nonnegative-integer? string?)
  (define lexer-snapshot (doc-lexer-snapshot doc))
  (define token
    (lexer-snapshot-token-at lexer-snapshot pos))
  (cond
    [(not token) ""]
    [else
     (define token-start (LexerEntry-start token))
     (substring (LexerEntry-text token)
                0
                (add1 (- pos token-start)))]))

(define (abs-range->range doc start end)
  (Range (doc-abs-pos->pos doc start) (doc-abs-pos->pos doc end)))

(define (hover-tag->signature-block tag)
  ;; We want signatures from `scribble/blueboxes` as they have better indentation,
  ;; but in some super rare cases blueboxes aren't accessible, thus we try to use the
  ;; parsed signature instead.
  (match-define (list signatures args-description)
    (if tag
        (get-docs-for-tag tag)
        (list #f #f)))
  (and signatures
       (~a "```\n"
           (string-join signatures "\n")
           (if args-description
               (~a "\n" args-description)
               "")
           "\n```\n---\n")))

(define (hover-documentation-text link signature-block)
  (if link
      (~a (or signature-block "")
          (or (extract-documentation-for-selected-element
                link #:include-signature? (not signature-block))
              ""))
      ""))

(define (build-hover-contents hover-text link documentation-text)
  (if link
      (~a hover-text
          " - [online docs]("
          (make-proper-url-for-online-documentation link)
          ")\n"
          (if (non-empty-string? documentation-text)
              (~a "\n---\n" documentation-text)
              ""))
      hover-text))

(define/contract (doc-hover doc pos)
  (-> Doc? Pos? (or/c Hover? #f))
  (define doc-trace (Doc-trace doc))
  (define pos* (doc-pos->abs-pos doc pos))
  (define-values (start end hover-text)
    (interval-map-ref/bounds (send doc-trace get-hovers) pos* #f))
  (cond
    [(not hover-text) #f]
    [else
     (match-define (list link tag)
       (interval-map-ref (send doc-trace get-docs) pos* (list #f #f)))
     (define signature-block (hover-tag->signature-block tag))
     (define documentation-text
       (hover-documentation-text link signature-block))
     (Hover #:contents (build-hover-contents hover-text link documentation-text)
            #:range (abs-range->range doc start end))]))

(define/contract (doc-code-action doc range)
  (-> Doc? Range? (listof CodeAction?))
  (define doc-trace (Doc-trace doc))
  (define req-start (doc-pos->abs-pos doc (Range-start range)))
  (define req-end (doc-pos->abs-pos doc (Range-end range)))
  (define trace-actions
    (interval-map-overlap-values (send doc-trace get-quickfixs)
                                 req-start
                                 req-end))

  (define resyntax-actions
    (for/list ([res (in-list (doc-get-resyntax-results doc))]
               #:when (char-range-intersect?
                        req-start
                        req-end
                        (Resyntax-Result-start res)
                        (Resyntax-Result-end res)))
      (resyntax-result->code-action doc res)))

  (append trace-actions resyntax-actions))

(define (doc-signature-form-head-pos doc query-pos)
  (define maybe-head (lexer-state-form-head-at (doc-lexer-state doc) query-pos))
  (and maybe-head (LexerTokenSpan-start maybe-head)))

(define (doc-signature-tag doc-trace snapshot callee-pos)
  (define maybe-docs-entry
    (interval-map-ref (send doc-trace get-docs) callee-pos #f))
  (cond
    [maybe-docs-entry (last maybe-docs-entry)]
    [else
     (define maybe-symbol (lexer-snapshot-symbol-at snapshot callee-pos))
     (and maybe-symbol
          (id-to-tag (LexerEntry-text maybe-symbol) doc-trace))]))

(define (tag->signature-help tag)
  (match-define (list signatures docs) (get-docs-for-tag tag))
  (and signatures
       (SignatureHelp
         #:signatures
         (for/list ([signature (in-list signatures)])
           (SignatureInformation
             #:label signature
             #:documentation (or docs ""))))))

(define/contract (doc-signature-help doc pos)
  (-> Doc? Pos? (or/c SignatureHelp? #f))
  (define doc-trace (Doc-trace doc))
  (define pos* (doc-pos->abs-pos doc pos))
  (define callee-pos (doc-signature-form-head-pos doc pos*))
  (define tag
    (and callee-pos
         (doc-signature-tag doc-trace (doc-lexer-snapshot doc) callee-pos)))
  (and tag (tag->signature-help tag)))

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
          (let ([decl (interval-map-ref doc-bindings (car (set-first maybe-bindings)) #f)])
            (if decl
                (values bind-start bind-end decl)
                (values #f #f #f)))
          (values #f #f #f))))

;; Get binding ranges for a declaration.
;; Returns a list of Range values.
(define/contract (doc-get-bindings doc decl)
  (-> Doc? Decl? (listof Range?))
  (define doc-trace (Doc-trace doc))
  (define doc-decls (send doc-trace get-sym-decls))
  (match-define (Decl req? id left right) decl)
  (define-values (bind-start bind-end bindings)
    (interval-map-ref/bounds doc-decls left #f))
  (if bindings
      (for/list ([range (in-set bindings)])
        (abs-range->range doc (car range) (cdr range)))
      empty))

(define (doc-completion-online-prefix token left-fragment cursor-pos)
  ;; Only two token classes currently produce a useful module-path prefix in
  ;; end-to-end completion behavior: `symbol` for ordinary path text such as
  ;; `racket/base`, and `string` for quoted module paths such as
  ;; `"racket/base"`.
  ;;
  ;; Other lexer classes either represent structural syntax or do not improve
  ;; the prefix we can hand to the module-name completion backend, so we treat
  ;; them the same as "no prefix".
  (define (completion-prefix-token? token)
    (memq (LexerEntry-type token)
          '(string symbol)))

  (cond
    [(not token) left-fragment]
    [(not (completion-prefix-token? token)) ""]
    [(eq? (LexerEntry-type token) 'string)
     (define token-start (LexerEntry-start token))
     (define token-end (LexerEntry-end token))
     (cond
       [(or (= (sub1 cursor-pos) token-start)
            (= (sub1 cursor-pos) (sub1 token-end)))
        ""]
       [else
        (substring left-fragment 1)])]
    [else left-fragment]))

;; Completion: returns a CompletionList.
(define/contract (doc-completion doc pos)
  (-> Doc? Pos? CompletionList?)
  (define doc-trace (Doc-trace doc))
  (define cursor-pos (doc-pos->abs-pos doc pos))
  (define lexer-snapshot (doc-lexer-snapshot doc))
  (define token
    (and (positive? cursor-pos)
         (lexer-snapshot-token-at lexer-snapshot (sub1 cursor-pos))))
  (define left-fragment
    (if (zero? cursor-pos)
        ""
        (doc-token-prefix-at doc (sub1 cursor-pos))))

  (define online-prefix
    (doc-completion-online-prefix token left-fragment cursor-pos))
  (define completions
    (append (send doc-trace get-completions)
            (send doc-trace get-online-completions online-prefix)))
  (define result
    (for/list ([completion (in-list completions)])
      (CompletionItem #:label (symbol->string completion))))
  (CompletionList #:isIncomplete #t
                  #:items result))

;; Definition: returns a Location or #f.
(define/contract (doc-definition doc uri pos)
  (-> Doc? string? Pos? (or/c Location? #f))
  (define-values (start end decl) (doc-get-decl doc pos))
  (match decl
    [#f #f]
    [(Decl #f id start end)
     (Location #:uri uri
               #:range (abs-range->range doc start end))]
    [(Decl path id 0 0)
     (Location #:uri (path->uri path)
               #:range (doc-get-definition-by-id path id))]))

;; References: returns a list of Locations or #f.
(define/contract (doc-references doc uri pos include-decl?)
  (-> Doc? string? Pos? boolean? (or/c (listof Location?) #f))
  (define-values (start end decl) (doc-get-decl doc pos))
  (match decl
    [(Decl req? id left right)
     (define ranges
       (if req?
           (list (abs-range->range doc start end)
                 (abs-range->range doc left right))
           (or (doc-get-bindings doc decl))))
     (define local-locations
       (for/list ([range (in-list ranges)])
         (Location #:uri uri #:range range)))
     ;; id can be #f. Use position range to get its name
     (define ws-id
       (or id
           (for/or ([(sym def) (in-hash (send (Doc-trace doc) get-definitions))])
             (and (= (Decl-left def) left) (= (Decl-right def) right) sym))))
     (define workspace-locations
       (if ws-id
           (send (Doc-trace doc) get-workspace-bindings (Doc-uri doc) ws-id)
           '()))
     (append local-locations workspace-locations)]
    [#f #f]))

;; Document Highlight: returns a list of DocumentHighlights or #f.
(define/contract (doc-highlights doc pos)
  (-> Doc? Pos? (or/c (listof DocumentHighlight?) #f))
  (define-values (start end decl) (doc-get-decl doc pos))
  (match decl
    [(Decl filepath id left right)
     (define ranges
       (if filepath
           (list (abs-range->range doc start end)
                 (abs-range->range doc left right))
           (or (append (doc-get-bindings doc decl)
                       (list (abs-range->range doc left right))))))
     (for/list ([range (in-list ranges)])
       (DocumentHighlight #:range range))]
    [#f #f]))

;; Rename: returns a WorkspaceEdit or #f.
(define/contract (doc-rename doc uri pos new-name)
  (-> Doc? string? Pos? string? (or/c WorkspaceEdit? #f))
  (define-values (start end decl) (doc-get-decl doc pos))
  (match decl
    [(Decl req? id left right)
     (cond [req? #f]
           [else
            (define ranges (cons (abs-range->range doc left right)
                                 (doc-get-bindings doc decl)))
            (WorkspaceEdit
              #:changes
              (hasheq (string->symbol uri)
                      (for/list ([range (in-list ranges)])
                        (TextEdit #:range range #:newText new-name))))])]
    [#f #f]))

;; Prepare Rename: returns a Range or #f.
(define/contract (doc-prepare-rename doc pos)
  (-> Doc? Pos? (or/c Range? #f))
  (define-values (start end decl) (doc-get-decl doc pos))
  (if (and decl (not (Decl-filepath decl)))
      (abs-range->range doc start end)
      #f))

;; Document Symbols: returns a list of SymbolInformation.
(define/contract (doc-symbols doc uri)
  (-> Doc? string? (listof SymbolInformation?))

  (define (symbol-information-lexer-entry? entry)
    (memq (LexerEntry-type entry)
          '(constant string symbol)))

  (define (lexer-entry->symbol-kind entry)
    (match (LexerEntry-type entry)
      ['constant SymbolKind-Constant]
      ['string SymbolKind-String]
      ['symbol SymbolKind-Variable]))

  (for/list ([entry (in-lexer-snapshot (doc-lexer-snapshot doc))]
             #:when (symbol-information-lexer-entry? entry))
    (define range (abs-range->range doc (LexerEntry-start entry) (LexerEntry-end entry)))
    (SymbolInformation
      #:name (LexerEntry-text entry)
      #:kind (lexer-entry->symbol-kind entry)
      #:location (Location #:uri uri
                           #:range range))))

;; Hierarchical Document Symbols: returns a tree of DocumentSymbol.
;;
;; The tree is built by recursing over the lexer's token forest, which already
;; encodes paren nesting. Definition-like forms (define, struct, module+, ...)
;; become symbols whose `range` spans the whole form and whose `selectionRange`
;; covers the defined name; any other form is transparent and passes nested
;; definitions up to the enclosing symbol. The forest tolerates incomplete code
;; (forms left open mid-edit still parse to the end of the document), so the
;; symbol tree stays stable during editing.

;; Maps the head symbol of a form to how its SymbolKind is chosen. 'fn-or-var
;; picks Function when the defined name uses the function shorthand
;; (define (f x) ...) and Variable otherwise.
(define *definition-form-kinds*
  (hash "define" 'fn-or-var
        "define/contract" 'fn-or-var
        "define/public" 'fn-or-var
        "define/private" 'fn-or-var
        "define/override" 'fn-or-var
        "define/augment" 'fn-or-var
        "define-values" SymbolKind-Variable
        "define-syntax" 'fn-or-var
        "define-syntax-rule" SymbolKind-Function
        "define-syntaxes" SymbolKind-Variable
        "define-match-expander" SymbolKind-Function
        "define-struct" SymbolKind-Struct
        "struct" SymbolKind-Struct
        "module" SymbolKind-Module
        "module+" SymbolKind-Module
        "module*" SymbolKind-Module))

;; Container forms become a symbol named after their own head so that
;; scrolling inside a long body keeps a sticky header even though the form
;; defines no name itself.
(define *container-form-kinds*
  (hash "provide" SymbolKind-Namespace
        "require" SymbolKind-Namespace
        "class" SymbolKind-Class
        "class*" SymbolKind-Class
        "match" SymbolKind-Object
        "match*" SymbolKind-Object
        "cond" SymbolKind-Object
        "for" SymbolKind-Object
        "for/list" SymbolKind-Object
        "for/vector" SymbolKind-Object
        "for/hash" SymbolKind-Object
        "for/hasheq" SymbolKind-Object
        "for/hasheqv" SymbolKind-Object
        "for/hashalw" SymbolKind-Object
        "for/and" SymbolKind-Object
        "for/or" SymbolKind-Object
        "for/sum" SymbolKind-Object
        "for/product" SymbolKind-Object
        "for/lists" SymbolKind-Object
        "for/first" SymbolKind-Object
        "for/last" SymbolKind-Object
        "for/fold" SymbolKind-Object
        "for/foldr" SymbolKind-Object))

;; Class member forms: every name they declare becomes a Field symbol, either
;; a bare name as in (field z) or a binding group as in (field [y 0]).
(define *field-form-heads*
  (set "field" "init-field" "init"))

(define/contract (doc-symbols-hierarchical doc)
  (-> Doc? (listof DocumentSymbol?))
  ;; Paren tracking means nothing in non-sexp languages (e.g. Rhombus,
  ;; Scribble): form extents are set by blocks or markup rather than parens,
  ;; so any symbol the walker produced would have a bogus range. Report no
  ;; symbols instead. Unknown languages keep the sexp treatment, matching the
  ;; lexer fallback used elsewhere.
  (if (eq? (doc-language-body-mode doc) 'non-sexp)
      '()
      (doc-symbols-hierarchical/sexp doc)))

;; Walk the lexer's token forest. Each `(...)` form is a Token-List whose head
;; is its first meaningful child. A definition form becomes a symbol named after
;; the bound name, a container form a symbol named after its own head, and a
;; field form one Field symbol per declared name. Any other form is transparent:
;; it contributes its nested definitions to the enclosing scope. The forest
;; already tracks paren nesting and tolerates unclosed mid-edit forms, so this
;; reuses that structure instead of re-deriving it from the flat token stream.
(define (doc-symbols-hierarchical/sexp doc)
  (define text (LexerSnapshot-text (doc-lexer-snapshot doc)))
  (define (span-text span)
    (substring text (LexerTokenSpan-start span) (LexerTokenSpan-end span)))

  (define (symbol-leaf? node)
    (and (Token-Leaf? node)
         (token-leaf-type? node 'symbol)))

  (define (meaningful nodes)
    (filter non-skippable-node? nodes))

  ;; The first symbol leaf in pre-order across `nodes`, descending into lists
  ;; and prefix forms. This is the bound name of a definition, e.g. `f` in
  ;; `(define (f x) ...)` or `a` in `(define-values (a b) ...)`.
  (define (first-symbol-span nodes)
    (for/or ([node (in-list (meaningful nodes))])
      (cond
        [(symbol-leaf? node) (Token-Leaf-span node)]
        [else (first-symbol-span (token-node-children node))])))

  (define (node->document-symbol node name-span kind children)
    (DocumentSymbol
      #:name (span-text name-span)
      #:kind kind
      #:range (abs-range->range doc
                                (token-node-start node)
                                (token-node-end node))
      #:selectionRange (abs-range->range doc
                                         (LexerTokenSpan-start name-span)
                                         (LexerTokenSpan-end name-span))
      #:children children))

  (define (definition-symbol-kind head function?)
    (match (hash-ref *definition-form-kinds* head)
      ['fn-or-var (if function? SymbolKind-Function SymbolKind-Variable)]
      [kind kind]))

  (define (collect-symbols nodes)
    (append-map (lambda (node)
                  ;; Symbols contributed by `node` to its enclosing scope.
                  (cond
                    [(Token-List? node) (list-symbols node)]
                    ;; A quoted/prefixed datum is transparent to symbol collection.
                    [(Token-Prefix-Tree? node) (collect-symbols (token-node-children node))]
                    [else '()]))
                (meaningful nodes)))

  (define (list-symbols node)
    (define forms (meaningful (Token-List-children node)))
    (define head-node (and (pair? forms) (car forms)))
    (define head (and head-node
                      (symbol-leaf? head-node)
                      (span-text (Token-Leaf-span head-node))))
    (define args (if (pair? forms) (cdr forms) '()))
    (cond
      [(and head (set-member? *field-form-heads* head)
            ;; A field form like `(field z)`, `(field [y 0])`, or a mix declares one Field
            ;; per argument: a bare name covers just the name, a binding group covers the
            ;; whole `[name v]`.
            (filter-map
              (lambda (node)
                (define name-span
                  (cond
                    [(symbol-leaf? node) (Token-Leaf-span node)]
                    [(Token-List? node) (first-symbol-span (Token-List-children node))]
                    [else #f]))
                (and name-span
                     (node->document-symbol node name-span SymbolKind-Field '())))
              args))]
      ;; A container form is its own name, so scrolling its body keeps a sticky
      ;; header even though the form binds no name itself.
      [(and head (hash-has-key? *container-form-kinds* head))
       (list (node->document-symbol node
                                    (Token-Leaf-span head-node)
                                    (hash-ref *container-form-kinds* head)
                                    (collect-symbols args)))]
      [(and head (hash-has-key? *definition-form-kinds* head))
       (define name-span (first-symbol-span args))
       (cond
         [name-span
          ;; The function shorthand `(define (f x) ...)` puts the name inside a
          ;; nested list, so the first argument is a list rather than a symbol.
          (define function? (and (pair? args) (not (symbol-leaf? (car args)))))
          (list (node->document-symbol node
                                       name-span
                                       (definition-symbol-kind head function?)
                                       ;; The first argument is the name header;
                                       ;; nested definitions live in the body.
                                       (collect-symbols (cdr args))))]
         ;; A definition head with no name yet (mid-edit) is transparent.
         [else (collect-symbols args)])]
      [else (collect-symbols args)]))

  (collect-symbols (Token-Forest-nodes (doc-body-forest doc))))

(provide Doc?
         Doc-version
         Doc-uri
         make-doc
         doc-apply-edit!
         doc-apply-edits!
         doc-reset!
         doc-update-version!
         doc-update-uri!
         doc-pos->abs-pos
         doc-end-abs-pos
         doc-get-text
         doc-diagnostics
         doc-copy-text-buffer
         doc-abs-pos->pos
         doc-line-start-abs-pos
         doc-line-end-abs-pos
         doc-find-containing-paren
         doc-get-definition-by-id
         doc-format-edits
         doc-on-type-format-edits
         doc-range-tokens
         doc-token-at
         doc-token-prefix-at
         doc-language-policy
         doc-body-forest
         doc-expand
         doc-update-trace!
         doc-trace-latest?
         doc-expand!
         doc-resyntax
         doc-resyntax!
         doc-resyntax-available?
         doc-get-resyntax-results
         doc-update-resyntax-result!
         resyntax-result->diag
         resyntax-result->code-action
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
         doc-symbols-hierarchical)

