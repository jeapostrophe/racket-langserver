#lang racket/base

(require "../common/interfaces.rkt"
         racket/contract
         racket/match
         syntax-color/module-lexer
         syntax-color/racket-lexer)

;; Upstream lexer type docs:
;;   https://docs.racket-lang.org/syntax-color/Racket_Lexer.html
;;   https://docs.racket-lang.org/syntax-color/Module_Lexer.html
;;
;; `racket-lexer` reports one of these type symbols:
;; - 'error: malformed input, such as an unterminated string or bad char literal.
;; - 'comment: ordinary comments, block comments, and special comments.
;; - 'sexp-comment: the `#;` token that comments out the following datum.
;; - 'white-space: spaces, tabs, and newlines.
;; - 'constant: datum-like literals and reader forms such as numbers,
;;   booleans, and some quoting/prefix tokens.
;; - 'string: string literals.
;; - 'no-color: a non-whitespace token that should be left plain/uncolored.
;;   The framework docs call this out explicitly, and `racket-lexer` uses it
;;   for specials that do not map to a richer token class.
;; - 'parenthesis: grouping delimiters like `(`, `)`, `[`, `]`, `{`, and `}`.
;; - 'hash-colon-keyword: keywords like `#:name`.
;; - 'symbol: identifiers and operator names.
;; - 'eof: end of input.
;; - 'other: punctuation or reader/control tokens that do not fit the other
;;   buckets, such as `,` and the `#lang` line when lexed directly.
;;
;; `module-lexer` returns the same kind of type information for `#lang`-aware
;; lexing. For installed `#lang` languages such as Rhombus, it can dispatch to
;; that language's `color-lexer`; when a colorer reports an attribute hash it
;; extracts the `'type` field.
;;
;; This module normalizes token kinds so callers see stable shapes across
;; valid and invalid inputs:
;; - 'lang-directive: a leading `#lang`
;; - 'reader-directive: a leading `#reader`
;; - quote-family prefixes and `#;`
;;
;; This module caches the full lexer stream so callers can distinguish any
;; token class at a given position while reconstructing public token strings on
;; demand.

;; Cached lexer output for a document. Tokens are stored in source order so
;; point lookups can binary-search by span start while reconstructing public
;; token strings on demand.
(struct LexerTokenSpan
  (start end type)
  #:transparent)

(struct/contract LexerSnapshot
  ([text string?]
   [tokens (vectorof LexerTokenSpan?)])
  #:transparent)

;; Racket lexers report 1-based positions. Normalize to zero-based offsets and
;; clamp at zero so synthetic positions do not go negative.
(define (normalize-lexer-pos pos)
  (max 0 (sub1 pos)))

(define (make-lexer-span start end type)
  (and (< start end)
       (LexerTokenSpan start end type)))

;; Record one lexer span from the lexer stream.
(define (record-lexer-entry type start end)
  (define normalized-start (normalize-lexer-pos start))
  (define normalized-end (normalize-lexer-pos end))
  (make-lexer-span normalized-start normalized-end type))

(define (normalize-token type text)
  (match* (type text)
    [((or 'other 'error) (regexp #px"^#lang(?:\\s|$)"))
     'lang-directive]
    [(_ (regexp #px"^#reader(?:\\s|$)"))
     'reader-directive]
    [(_ "'") 'quote]
    [(_ "`") 'quasiquote]
    [(_ ",") 'unquote]
    [(_ ",@") 'unquote-splicing]
    [(_ "#;") 'sexp-comment]
    [(_ _) type]))

(define (lexer-span->public snapshot span)
  (LexerEntry (LexerTokenSpan-start span)
              (LexerTokenSpan-end span)
              (substring (LexerSnapshot-text snapshot)
                         (LexerTokenSpan-start span)
                         (LexerTokenSpan-end span))
              (LexerTokenSpan-type span)))

;; Binary search returns the first token whose end is strictly after `pos`.
;; That token is only a candidate, because spans may have gaps, so the caller
;; still checks the start bound to confirm `pos` is inside the span.
(define (find-first-token-ending-after tokens pos)
  (define token-count (vector-length tokens))
  (let loop ([low 0]
             [high token-count])
    (cond
      [(= low high)
       (and (< low token-count) low)]
      [else
       (define mid (quotient (+ low high) 2))
       (define span (vector-ref tokens mid))
       (if (<= (LexerTokenSpan-end span) pos)
           (loop (add1 mid) high)
           (loop low mid))])))

(define (lookup-lexer-entry snapshot pos)
  (define tokens (LexerSnapshot-tokens snapshot))
  (define idx (find-first-token-ending-after tokens pos))
  (and idx
       (let ([span (vector-ref tokens idx)])
         (and (<= (LexerTokenSpan-start span) pos)
              (lexer-span->public snapshot span)))))

;; Find the token at `pos`, or the last token before `pos` when `pos` falls
;; between token spans.
(define (find-token-index-at-or-before tokens pos)
  (define token-count (vector-length tokens))
  (define idx (find-first-token-ending-after tokens pos))
  (cond
    [(not idx)
     (and (positive? token-count) (sub1 token-count))]
    [else
     (define span (vector-ref tokens idx))
     (if (<= (LexerTokenSpan-start span) pos)
         idx
         (sub1 idx))]))

(define (lexer-layout-token? type)
  (memq type '(white-space comment sexp-comment)))

(define (lexer-token-span-paren-kind snapshot span)
  (and (eq? (LexerTokenSpan-type span) 'parenthesis)
       (case (string-ref (LexerSnapshot-text snapshot) (LexerTokenSpan-start span))
         [(#\() 'open]
         [(#\[) 'open]
         [(#\)) 'close]
         [(#\]) 'close]
         [else #f])))

(define (scan-enclosing-paren snapshot tokens idx depth)
  (cond
    [(< idx 0) #f]
    [else
     (define span (vector-ref tokens idx))
     (match (lexer-token-span-paren-kind snapshot span)
       ['close
        (scan-enclosing-paren snapshot tokens (sub1 idx) (add1 depth))]
       ['open
        (if (positive? depth)
            (scan-enclosing-paren snapshot tokens (sub1 idx) (sub1 depth))
            (LexerTokenSpan-start span))]
       [_
        (scan-enclosing-paren snapshot tokens (sub1 idx) depth)])]))

(define/contract (in-lexer-snapshot snapshot)
  (-> LexerSnapshot? sequence?)
  (define tokens (LexerSnapshot-tokens snapshot))
  (define token-count (vector-length tokens))
  (let ([idx 0])
    (in-producer
      (lambda ()
        (cond
          [(= idx token-count) eof]
          [else
           (define entry (lexer-span->public snapshot (vector-ref tokens idx)))
           (set! idx (add1 idx))
           entry]))
      eof)))

(define/contract (for-each-lexer-snapshot-entry snapshot proc)
  (-> LexerSnapshot? (-> LexerEntry? any/c) void?)
  (for ([entry (in-lexer-snapshot snapshot)])
    (proc entry)))

(define/contract (lexer-snapshot-enclosing-paren-start snapshot pos)
  (-> LexerSnapshot? exact-nonnegative-integer? (or/c exact-nonnegative-integer? #f))
  (define tokens (LexerSnapshot-tokens snapshot))
  (define token-count (vector-length tokens))
  (cond
    [(or (zero? token-count)
         (>= pos (string-length (LexerSnapshot-text snapshot))))
     #f]
    [else
     (define idx (find-token-index-at-or-before tokens pos))
     (scan-enclosing-paren snapshot tokens idx 0)]))

(define/contract (lexer-snapshot-next-symbol-start snapshot pos)
  (-> LexerSnapshot? exact-nonnegative-integer? (or/c exact-nonnegative-integer? #f))
  (define tokens (LexerSnapshot-tokens snapshot))
  (let loop ([idx (find-first-token-ending-after tokens pos)])
    (cond
      [(not idx) #f]
      [(>= idx (vector-length tokens)) #f]
      [else
       (define span (vector-ref tokens idx))
       (match (LexerTokenSpan-type span)
         [(? lexer-layout-token?) (loop (add1 idx))]
         ['symbol (LexerTokenSpan-start span)]
         [_ #f])])))

(define (lexer-wrap lexer)
  (define (eof-or-list txt type paren? start end)
    (if (eof-object? txt)
        eof
        (list txt type paren? start end)))

  (cond
    [(procedure? lexer)
     (lambda (in)
       (define-values (txt type paren? start end)
         (lexer in))
       (eof-or-list txt type paren? start end))]
    [(pair? lexer)
     (define lexer-proc (car lexer))
     (define mode (cdr lexer))
     (lambda (in)
       (define-values (txt type paren? start end _backup next-mode)
         (lexer-proc in 0 mode))
       ;; Preserve the updated lexer mode so the next call continues where
       ;; this one left off.
       (set! mode next-mode)
       (eof-or-list txt type paren? start end))]))

;; `module-lexer` can return a procedure, a `(procedure . mode)` pair, or a
;; sentinel value. That is how language-specific lexers are surfaced for
;; `#lang` modules such as Rhombus. For the pair case, `car` is the lexer
;; procedure and `cdr` is the lexer-specific state to pass back on the next
;; call.
(define (module-lexer->lexer lexer)
  (if (or (procedure? lexer) (pair? lexer))
      lexer
      racket-lexer))

(define (get-initial-lexer-state in)
  (define-values (txt type paren? start end _backup lexer)
    (module-lexer in 0 #f))
  (values txt type paren? start end (module-lexer->lexer lexer)))

(define/contract (build-lexer-snapshot text)
  (-> string? LexerSnapshot?)
  ;; Count lines before lexing so the lexer reports stable source locations for
  ;; the entire document snapshot.
  (define in (open-input-string text))
  (port-count-lines! in)
  (define-values (initial-txt initial-type _initial-paren? initial-start initial-end lexer)
    (get-initial-lexer-state in))
  ;; The initial token comes from `module-lexer`; subsequent tokens come from
  ;; the language-specific lexer selected above.
  (define initial-span
    (and (not (eof-object? initial-txt))
         (record-lexer-entry (normalize-token initial-type initial-txt)
                             initial-start
                             initial-end)))
  (define token-spans
    (append (if initial-span (list initial-span) '())
            (for*/list ([lst (in-port (lexer-wrap lexer) in)]
                        [span (in-value
                                (match lst
                                  [(list txt type _paren? start end)
                                   (record-lexer-entry (normalize-token type txt)
                                                       start
                                                       end)]))]
                        #:when span)
              span)))

  (LexerSnapshot text (list->vector token-spans)))

(define/contract (lexer-snapshot-token-at snapshot pos)
  (-> LexerSnapshot? exact-nonnegative-integer? (or/c LexerEntry? #f))
  (lookup-lexer-entry snapshot pos))

(define/contract (lexer-snapshot-symbol-at snapshot pos)
  (-> LexerSnapshot? exact-nonnegative-integer? (or/c LexerEntry? #f))
  (define entry (lookup-lexer-entry snapshot pos))
  (and entry
       (eq? (LexerEntry-type entry) 'symbol)
       entry))

(provide LexerSnapshot?
         build-lexer-snapshot
         in-lexer-snapshot
         for-each-lexer-snapshot-entry
         lexer-snapshot-enclosing-paren-start
         lexer-snapshot-next-symbol-start
         lexer-snapshot-token-at
         lexer-snapshot-symbol-at)
