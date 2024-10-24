#lang racket/base

(require drracket/check-syntax
         syntax/parse
         "../struct.rkt"
         racket/class
         racket/list
         racket/bool
         racket/dict
         racket/match
         data/interval-map
         "interface.rkt")

(provide highlight%)

(define highlight%
  (class base-service%
    (super-new)
    (init-field src doc-text)
    (define styles '())
    (define token-map (make-interval-map))

    (define/override (get)
      (interval-map->token-lst token-map))

    (define/override (reset)
      (set! styles '())
      (set! token-map (make-interval-map)))

    (define/override (expand start end)
      (interval-map-expand! token-map start end))

    (define/override (contract start end)
      (interval-map-contract! token-map start end))

    (define/override (syncheck:color-range src start end style)
      (when (< start end)
        (set! styles (cons (Token start end (string->symbol style)) styles))))

    (define/override (syncheck:add-definition-target src start finish id mods)
      (when (< start finish)
        (set! styles (cons (Token start finish 'definition) styles))))

    (define/override (walk-stx stx expanded)
      (set! token-map (token-list->interval-map (collect-tokens stx expanded styles src doc-text))))

    (define (token-list->interval-map lst)
      (define interval-map (make-interval-map))
      (for ([tok lst])
        (interval-map-set! interval-map (SemanticToken-start tok) (SemanticToken-end tok) tok))
      interval-map)

    (define (interval-map->token-lst token-map)
      (for/list ([(k v) (in-dict token-map)])
        (SemanticToken (car k) (cdr k) (SemanticToken-type v) (SemanticToken-modifiers v))))
    ))

;; A temporary structure to hold tokens
;; `tag` is symbol that is a tag associated with this token.
;; An identifier may correspond multiple tokens. They will be merged, then converted into
;; lsp semantic token types and modifiers.
(struct Token
  (start end tag))

(define (collect-tokens stx expanded styles src doc-text)
  (define drracket-styles (convert-drracket-color-styles styles))
  (define code-str (send doc-text get-text))

  (define token-list
    (append drracket-styles
            (if (syntax? stx) (walk-orig-stx stx) '())
            (if (syntax? expanded) (walk-expanded-stx src expanded) '())))

  (let* ([tokens-no-false (filter-not false? token-list)]
         [tokens-no-out-bounds (filter (λ (t) (< -1 (Token-start t) (string-length code-str)))
                                       tokens-no-false)]
         [tokens-in-order (sort tokens-no-out-bounds < #:key Token-start)]
         [same-ident-token-groups (group-by Token-start tokens-in-order)]
         [tokens-with-merged-tags
          (for/list ([token-group same-ident-token-groups])
            (define tok (first token-group))
            (list (Token-start tok) (Token-end tok) (map Token-tag token-group)))]
         [result-tokens
          (for*/list ([t tokens-with-merged-tags]
                      [type (in-value (select-type (third t)))]
                      [modifiers (in-value (get-valid-modifiers (third t)))]
                      #:when (not (false? type)))
            (SemanticToken (first t) (second t) type modifiers))])
    result-tokens))

(define (convert-drracket-color-styles styles)
  (for/list ([s styles])
    (match s
      [(Token start end 'drracket:check-syntax:lexically-bound)
       (Token start end 'variable)]
      [(Token start end 'drracket:check-syntax:set!d)
       (Token start end 'variable)]
      [_ #f])))

;; `tags` might contains multiple valid types.
;; This function selects a proper type based on some rules.
(define (select-type tags)
  (define valid-types (filter (λ (t) (memq t *semantic-token-types*)) tags))
  (cond [(null? valid-types)
         #f]
        [(memq 'function valid-types)
         'function]
        [(memq 'variable valid-types)
         'variable]
        [else (first valid-types)]))

(define (get-valid-modifiers tags)
  (filter (λ (t) (memq t *semantic-token-modifiers*)) tags))

(define (walk-orig-stx stx)
  (syntax-parse stx
    #:datum-literals (#%module-begin)
    [() (list)]
    [(any1 any* ...)
     (append (walk-orig-stx #'any1)
             (walk-orig-stx #'(any* ...)))]
    [#(any1 any* ...)
     (append (walk-orig-stx #'any1)
             (walk-orig-stx #'(any* ...)))]
    [#%module-begin
     (list)]
    [atom (list (tag-of-atom-stx #'atom))]))

(define (walk-expanded-stx src stx)
  (syntax-parse stx
    #:datum-literals (lambda define-values #%app)
    [(lambda (args ...) expr ...)
     (walk-expanded-stx src #'(expr ...))]
    [(define-values (fs) (lambda _ ...))
     (append (list (tag-of-expanded-symbol-stx src #'fs 'function))
             (walk-expanded-stx src (drop (syntax-e stx) 2)))]
    [(define-values (names ...) expr)
     (walk-expanded-stx src #'expr)]
    [(#%app proc args ...)
     (append (list (tag-of-expanded-symbol-stx src #'proc 'function))
             (walk-expanded-stx src #'(args ...)))]
    [(any1 any* ...)
     (append (walk-expanded-stx src #'any1)
             (walk-expanded-stx src #'(any* ...)))]
    [_ (list)]))

(define (tag-of-expanded-symbol-stx src stx tag)
  (define (in-current-file? stx)
    (equal? src (syntax-source stx)))

  (if (and (in-current-file? stx)
           (symbol? (syntax->datum stx)))
      (tag-of-atom-stx stx tag)
      #f))

(define (tag-of-atom-stx atom-stx [expect-tag #f])
  (define pos+1 (syntax-position atom-stx))
  (define len (syntax-span atom-stx))
  (if (or (not pos+1) (not len) (= len 0)
          (not (syntax-original? atom-stx)))
      #f
      (let ([pos (sub1 pos+1)])
        (Token pos (+ pos len)
               (if (false? expect-tag)
                   (get-atom-tag (syntax-e atom-stx))
                   expect-tag)))))

(define (get-atom-tag atom)
  (match atom
    [(? number?) 'number]
    [(? symbol?) 'symbol]
    [(? string?) 'string]
    [(? bytes?) 'string]
    [(? regexp?) 'regexp]
    [_ 'unknown]))

