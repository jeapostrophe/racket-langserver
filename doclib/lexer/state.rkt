#lang racket/base

(require "../../common/interfaces.rkt"
         "../doc-lang.rkt"
         "../lazy-cache.rkt"
         "scan.rkt"
         "snapshot.rkt"
         "token-tree.rkt"
         "tree-query.rkt"
         racket/contract)

;; `LexerState` groups the flat snapshot, language policy, and a lazy
;; body-forest cache. Documents keep one LexerState instead of separate caches
;; for snapshot, language, and forest.
(struct/contract LexerState
  ([snapshot LexerSnapshot?]
   [language-policy Language-Policy?]
   [body-forest-cache (lazy-cache-of Token-Forest?)])
  #:transparent)

(define/contract (build-lexer-snapshot text [uri #f])
  (->* (string?) ((or/c #f string?)) LexerSnapshot?)
  (define token-span-vector (text->lexer-token-spans text))
  (LexerSnapshot text token-span-vector))

(define (lexer-state-body-mode state)
  (Language-Policy-body-mode (LexerState-language-policy state)))

(define (forest-range-for-language-policy policy spans)
  (define total (vector-length spans))
  (define body-start-idx (Language-Policy-body-start-idx policy))
  (cond
    [(eq? 'non-sexp (Language-Policy-body-mode policy))
     (values 0 body-start-idx)]
    [else
     (values (if (and (< body-start-idx total)
                      (span-at spans body-start-idx))
                 body-start-idx
                 0)
             total)]))

(define (build-token-forest-for-language-policy policy spans)
  (define-values (start end)
    (forest-range-for-language-policy policy spans))
  (parse-token-forest spans start end))

;; Build a token forest from token spans. Uses language policy to decide what
;; portion of the spans to parse: for non-sexp languages only the prefix is
;; parsed; for sexp languages, the body starting at body-start-idx is parsed.
(define (build-snapshot-token-forest text uri spans)
  (define policy (lexer-language-policy text spans uri))
  (build-token-forest-for-language-policy policy spans))

(define (build-lexer-state text uri)
  (define snapshot (build-lexer-snapshot text uri))
  (define policy (lexer-language-policy (LexerSnapshot-text snapshot)
                                        (LexerSnapshot-tokens snapshot)
                                        uri))
  (LexerState snapshot policy (make-lazy-cache)))

(define (lexer-state-body-forest state)
  (call-with-lazy-cache!
    (LexerState-body-forest-cache state)
    (lambda ()
      (build-token-forest-for-language-policy
        (LexerState-language-policy state)
        (LexerSnapshot-tokens (LexerState-snapshot state))))))

(define/contract (lexer-state-token-at state pos)
  (-> LexerState? exact-nonnegative-integer? (or/c LexerEntry? #f))
  (lexer-snapshot-token-at (LexerState-snapshot state) pos))

(define/contract (lexer-state-symbol-at state pos)
  (-> LexerState? exact-nonnegative-integer? (or/c LexerEntry? #f))
  (lexer-snapshot-symbol-at (LexerState-snapshot state) pos))

(define (lexer-state-structural-forest state)
  (and (not (eq? 'non-sexp (lexer-state-body-mode state)))
       (lexer-state-body-forest state)))

(define/contract (lexer-state-containing-open-paren state pos)
  (-> LexerState? exact-nonnegative-integer? (or/c exact-nonnegative-integer? #f))
  (define maybe-forest (lexer-state-structural-forest state))
  (and maybe-forest
       (let ([enclosing-list
              (token-forest-deepest-enclosing-list maybe-forest pos)])
         (and enclosing-list
              (LexerTokenSpan-start
                (Token-List-open-span enclosing-list))))))

(define/contract (lexer-state-form-head-at state pos)
  (-> LexerState? exact-nonnegative-integer? (or/c LexerTokenSpan? #f))
  (define maybe-forest (lexer-state-structural-forest state))
  (and maybe-forest
       (token-forest-form-head maybe-forest pos)))

(define/contract (lexer-state-sexp-comment-spans state)
  (-> LexerState? (listof CharRange?))
  (define maybe-forest (lexer-state-structural-forest state))
  (if maybe-forest
      (token-forest-sexp-comment-spans maybe-forest)
      '()))

(provide (struct-out LexerState)
         build-lexer-snapshot
         build-lexer-state
         build-snapshot-token-forest
         lexer-state-body-mode
         lexer-state-body-forest
         lexer-state-token-at
         lexer-state-symbol-at
         lexer-state-containing-open-paren
         lexer-state-form-head-at
         lexer-state-sexp-comment-spans)
