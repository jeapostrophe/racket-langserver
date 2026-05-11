#lang racket/base

(require "../common/interfaces.rkt"
         "doc-lang.rkt"
         "lazy-cache.rkt"
         "lexer/scan.rkt"
         "lexer/shared.rkt"
         "lexer/token-tree.rkt"
         "lexer/tree-query.rkt"
         racket/contract
         racket/match)

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
;; The scan layer normalizes token kinds so callers see stable shapes across
;; valid and invalid inputs:
;; - 'lang-directive: a leading `#lang`
;; - 'reader-directive: a leading `#reader`
;; - quote-family and syntax quote-family prefixes, plus `#;`
;; - open-paren/close-paren direction for the internal span cache
;;
;; This module is the public lexer facade. It builds snapshots from normalized
;; token spans, attaches a token forest, and exposes position-oriented queries.

(define/contract (lexer-snapshot-span->entry snapshot span)
  (-> LexerSnapshot? LexerTokenSpan? LexerEntry?)
  (LexerEntry (LexerTokenSpan-start span)
              (LexerTokenSpan-end span)
              (substring (LexerSnapshot-text snapshot)
                         (LexerTokenSpan-start span)
                         (LexerTokenSpan-end span))
              (LexerTokenSpan-type span)))

(define (lexer-token-span-contains-pos? span pos)
  (and (<= (LexerTokenSpan-start span) pos)
       (< pos (LexerTokenSpan-end span))))

;; Find the token at `pos`, or the next token after `pos` when `pos` falls
;; between token spans. Returns #f when `pos` is after the last token.
(define (find-token-index-at-or-after tokens pos)
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

(define (find-token-index-at tokens pos)
  (define idx (find-token-index-at-or-after tokens pos))
  (and idx
       (let ([span (vector-ref tokens idx)])
         (and (lexer-token-span-contains-pos? span pos) idx))))

(define (lookup-lexer-entry snapshot pos)
  (define tokens (LexerSnapshot-tokens snapshot))
  (define idx (find-token-index-at tokens pos))
  (and idx
       (lexer-snapshot-span->entry snapshot (vector-ref tokens idx))))

;; Find the token at `pos`, or the last token before `pos` when `pos` falls
;; between token spans. Returns #f when `pos` is before the first token.
(define (find-token-index-at-or-before tokens pos)
  (define token-count (vector-length tokens))
  (define idx (find-token-index-at-or-after tokens pos))
  (cond
    [(not idx)
     (and (positive? token-count) (sub1 token-count))]
    [else
     (define span (vector-ref tokens idx))
     (cond
       [(lexer-token-span-contains-pos? span pos) idx]
       [(zero? idx) #f]
       [else (sub1 idx)])]))

;; Build a token forest from token spans. Uses language info to decide what
;; portion of the spans to parse: for non-sexp languages only the prefix is
;; parsed; for sexp languages, the body starting at body-start-idx is parsed.
(define (build-snapshot-token-forest text uri spans)
  (define info (lexer-language-info text spans uri))
  (define total (vector-length spans))
  (define body-start-idx
    (cond [(Language-Info-prefix info)
           => Language-Prefix-body-start-idx]
          [else 0]))
  (define-values (start end)
    (cond
      [(eq? 'non-sexp (Language-Info-body-mode info))
       (values 0 body-start-idx)]
      [else
       (values (if (and (< body-start-idx total)
                        (span-at spans body-start-idx))
                   body-start-idx
                   0)
               total)]))
  (parse-token-forest spans start end))

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
           (define entry
             (lexer-snapshot-span->entry snapshot (vector-ref tokens idx)))
           (set! idx (add1 idx))
           entry]))
      eof)))

(define/contract (for-each-lexer-snapshot-entry snapshot proc)
  (-> LexerSnapshot? (-> LexerEntry? any/c) void?)
  (for ([entry (in-lexer-snapshot snapshot)])
    (proc entry)))

;; Flat token queries — these never need a token forest.

(define/contract (lexer-snapshot-token-at snapshot pos)
  (-> LexerSnapshot? exact-nonnegative-integer? (or/c LexerEntry? #f))
  (lookup-lexer-entry snapshot pos))

(define/contract (lexer-snapshot-symbol-at snapshot pos)
  (-> LexerSnapshot? exact-nonnegative-integer? (or/c LexerEntry? #f))
  (define entry (lookup-lexer-entry snapshot pos))
  (and entry
       (eq? (LexerEntry-type entry) 'symbol)
       entry))

;; Structural tree queries — these require a token forest.
;; Callers should ensure the document is in a sexp-compatible body mode.

(define/contract (build-lexer-snapshot text [uri #f])
  (->* (string?) ((or/c #f string?)) LexerSnapshot?)
  (define token-span-vector (text->lexer-token-spans text))
  (LexerSnapshot text token-span-vector))

;; LexerState groups the flat snapshot, language metadata, and a lazy body-forest
;; cache. Documents keep one LexerState instead of separate caches for snapshot,
;; language, and forest. The forest covers whatever portion of the token spans is
;; relevant for the language's body mode (full file for sexp/unknown, header-only
;; for non-sexp).
(struct/contract LexerState
  ([snapshot LexerSnapshot?]
   [language-info Language-Info?]
   [body-forest-cache (lazy-cache-of Token-Forest?)])
  #:transparent)

(define (build-lexer-state text uri)
  (define snapshot (build-lexer-snapshot text uri))
  (define info (lexer-language-info (LexerSnapshot-text snapshot)
                                    (LexerSnapshot-tokens snapshot)
                                    uri))
  (LexerState snapshot info (make-lazy-cache)))

(define (lexer-state-body-forest state text uri)
  (call-with-lazy-cache!
    (LexerState-body-forest-cache state)
    (lambda ()
      (build-snapshot-token-forest text
                                   uri
                                   (LexerSnapshot-tokens (LexerState-snapshot state))))))

(provide (struct-out LexerTokenSpan)
         (struct-out LexerSnapshot)
         LexerSnapshot?
         token-node?
         sexp-comment-node?
         token-node-children
         token-node-span
         parse-token-forest
         token-forest-flattened-nodes
         token-forest-node-path
         token-node-parent/path
         token-forest-ancestors-at-pos
         token-forest-deepest-enclosing-list
         token-forest-form-head
         token-forest-sexp-comment-spans
         (struct-out Token-Leaf)
         (struct-out Token-List)
         (struct-out Token-Prefix-Tree)
         (struct-out Token-Forest)
         build-lexer-snapshot
         build-lexer-state
         build-snapshot-token-forest
         LexerState?
         LexerState-snapshot
         LexerState-language-info
         lexer-state-body-forest
         lexer-language-info
         Language-Info
         Language-Info?
         Language-Info-prefix
         Language-Info-language
         Language-Info-body-mode
         lexer-snapshot-span->entry
         lexer-token-span-contains-pos?
         in-lexer-snapshot
         for-each-lexer-snapshot-entry
         find-token-index-at
         find-token-index-at-or-before
         find-token-index-at-or-after
         lexer-snapshot-token-at
         lexer-snapshot-symbol-at)
