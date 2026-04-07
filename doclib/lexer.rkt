#lang racket/base

(require "../common/interfaces.rkt"
         "internal-types.rkt"
         racket/contract
         racket/match
         data/interval-map
         syntax-color/module-lexer
         syntax-color/racket-lexer)

(define symbol-entry/c
  (list/c string? SymbolKind?))

;; Token data stored in the interval map. Positions are zero-based offsets
;; so they can be queried directly with LSP document positions.
(struct/contract LexerEntry
  ([text string?]
   [type symbol?]
   [start exact-nonnegative-integer?]
   [end exact-nonnegative-integer?])
  #:transparent)

;; Cached lexer output for a document. `tokens` keeps the raw token metadata
;; while `symbols` stores only the subset used for document symbols/highlighting.
(struct/contract LexerSnapshot
  ([tokens (interval-map-of LexerEntry?)]
   [symbols (interval-map-of symbol-entry/c)])
  #:transparent)

;; Only these token classes are surfaced as symbol entries for consumers of the
;; snapshot. Everything else is ignored after lexing.
(define relevant-lexer-token-types
  '(constant string symbol))

(define (lexer-entry->symbol-kind type)
  (match type
    ['constant SymbolKind-Constant]
    ['string SymbolKind-String]
    ['symbol SymbolKind-Variable]))

;; Racket lexers report 1-based positions. Normalize to zero-based offsets and
;; clamp at zero so synthetic positions do not go negative.
(define (normalize-lexer-pos pos)
  (max 0 (sub1 pos)))

;; Record one lexer span in both interval maps when it represents a token class
;; we expose to language-server features.
(define (record-lexer-entry! tokens symbols txt type start end)
  (when (memq type relevant-lexer-token-types)
    (define entry
      (LexerEntry txt
                  type
                  (normalize-lexer-pos start)
                  (normalize-lexer-pos end)))
    (when (< (LexerEntry-start entry) (LexerEntry-end entry))
      (interval-map-set! tokens
                         (LexerEntry-start entry)
                         (LexerEntry-end entry)
                         entry)
      (interval-map-set! symbols
                         (LexerEntry-start entry)
                         (LexerEntry-end entry)
                         (list (LexerEntry-text entry)
                               (lexer-entry->symbol-kind (LexerEntry-type entry)))))))

;; Adapt both lexer state shapes used by `in-port`: a plain procedure, or the
;; stateful pair returned by lexers that thread an explicit mode value.
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
    [(pair? lexer)
     (define-values (txt type paren? start end _backup mode)
       ((car lexer) in 0 (cdr lexer)))
     (set! lexer (cons (car lexer) mode))
     (eof-or-list txt type paren? start end)]))

;; `module-lexer` handles the optional `#lang` line and tells us which lexer
;; should continue the rest of the document. When it cannot supply a specific
;; lexer state, fall back to the standard Racket lexer.
(define (get-initial-lexer-state in)
  (define-values (txt type paren? start end _backup lexer)
    (module-lexer in 0 #f))
  (values txt
          type
          paren?
          start
          end
          (cond
            [(procedure? lexer) lexer]
            [(pair? lexer) lexer]
            [(eq? lexer 'no-lang-line) racket-lexer]
            [(eq? lexer 'before-lang-line) racket-lexer]
            [else racket-lexer])))

(define/contract (build-lexer-snapshot text)
  (-> string? LexerSnapshot?)
  ;; Count lines before lexing so the lexer reports stable source locations for
  ;; the entire document snapshot.
  (define in (open-input-string text))
  (port-count-lines! in)
  (define tokens (make-interval-map))
  (define symbols (make-interval-map))
  (define-values (initial-txt initial-type _initial-paren? initial-start initial-end lexer)
    (get-initial-lexer-state in))
  ;; The initial token comes from `module-lexer`; subsequent tokens come from
  ;; the language-specific lexer selected above.
  (unless (eof-object? initial-txt)
    (record-lexer-entry! tokens
                         symbols
                         initial-txt
                         initial-type
                         initial-start
                         initial-end))
  (for ([lst (in-port (lexer-wrap lexer) in)])
    (match-define (list txt type _paren? start end) lst)
    (record-lexer-entry! tokens symbols txt type start end))
  (LexerSnapshot tokens symbols))

(define/contract (lexer-snapshot-symbols snapshot)
  (-> LexerSnapshot? (interval-map-of symbol-entry/c))
  (LexerSnapshot-symbols snapshot))

(define/contract (lexer-snapshot-token-at snapshot pos)
  (-> LexerSnapshot? exact-nonnegative-integer? (or/c LexerEntry? #f))
  (interval-map-ref (LexerSnapshot-tokens snapshot) pos #f))

(define/contract (lexer-snapshot-symbol-at snapshot pos)
  (-> LexerSnapshot? exact-nonnegative-integer? (or/c symbol-entry/c #f))
  (interval-map-ref (LexerSnapshot-symbols snapshot) pos #f))

(provide symbol-entry/c
         (struct-out LexerEntry)
         LexerSnapshot?
         build-lexer-snapshot
         lexer-snapshot-symbols
         lexer-snapshot-token-at
         lexer-snapshot-symbol-at)
