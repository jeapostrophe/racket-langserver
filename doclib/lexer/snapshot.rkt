#lang racket/base

(require "../../common/interfaces.rkt"
         racket/contract)

;; Flat lexer data shapes and position-oriented snapshot queries. Keep this
;; module independent from scanning and token-tree parsing so the rest of the
;; lexer stack can depend on common data without forming cycles.

(struct/contract LexerTokenSpan
  ([start exact-nonnegative-integer?]
   [end exact-nonnegative-integer?]
   [type symbol?])
  #:transparent)

(struct/contract LexerSnapshot
  ([text string?]
   [tokens (vectorof LexerTokenSpan?)])
  #:transparent)

(define (make-lexer-span start end type)
  (and (< start end)
       (LexerTokenSpan start end type)))

(define (span-at spans idx)
  (and (<= 0 idx)
       (< idx (vector-length spans))
       (vector-ref spans idx)))

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

(define/contract (lexer-entry-paren-kind entry)
  (-> LexerEntry? (or/c 'open 'close #f))
  (case (LexerEntry-type entry)
    [(open-paren)
     (and (memv (string-ref (LexerEntry-text entry) 0) '(#\( #\[ #\{)) 'open)]
    [(close-paren)
     (and (memv (string-ref (LexerEntry-text entry) 0) '(#\) #\] #\})) 'close)]
    [else #f]))

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

(define/contract (lexer-snapshot-token-at snapshot pos)
  (-> LexerSnapshot? exact-nonnegative-integer? (or/c LexerEntry? #f))
  (lookup-lexer-entry snapshot pos))

(define/contract (lexer-snapshot-symbol-at snapshot pos)
  (-> LexerSnapshot? exact-nonnegative-integer? (or/c LexerEntry? #f))
  (define entry (lookup-lexer-entry snapshot pos))
  (and entry
       (eq? (LexerEntry-type entry) 'symbol)
       entry))

(provide (struct-out LexerTokenSpan)
         (struct-out LexerSnapshot)
         make-lexer-span
         span-at
         lexer-snapshot-span->entry
         lexer-token-span-contains-pos?
         find-token-index-at
         find-token-index-at-or-before
         find-token-index-at-or-after
         in-lexer-snapshot
         lexer-entry-paren-kind
         for-each-lexer-snapshot-entry
         lexer-snapshot-token-at
         lexer-snapshot-symbol-at)
