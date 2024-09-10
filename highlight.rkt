#lang racket/base

(require syntax/modread
         drracket/check-syntax
         syntax/parse
         "struct.rkt"
         racket/class
         racket/set
         racket/list
         racket/bool
         racket/match)

(provide collect-semantic-tokens)

;; A temporary structure to hold tokens
;; `tag` is symbol that is a tag associated with this token.
;; An identifier may correspond multiple tokens. They will be merged, then converted into
;; lsp semantic token types and modifiers.
(struct Token
  (start end tag))

(define collector%
  (class (annotations-mixin object%)
    (define styles '())

    (super-new)

    (define/override (syncheck:find-source-object stx)
      #f)

    (define/override (syncheck:color-range src start end style)
      (when (< start end)
        (set! styles (cons (Token start end (string->symbol style)) styles))))

    (define/override (syncheck:add-definition-target src start finish id mods)
      (when (< start finish)
        (set! styles (cons (Token start finish 'definition) styles))))

    (define/public (get-styles)
      (set->list (list->set styles)))))

; (-> lsp-editor% Path (Listof SemanticToken))
(define (collect-semantic-tokens doc-text path)
  (define code-str (send doc-text get-text))
  (define in (open-input-string code-str))
  (port-count-lines! in)
  (define-values (path-dir _1 _2) (split-path path))

  (define base-ns (make-base-namespace))

  (define-values (add-syntax done)
    (make-traversal base-ns #f))

  (define token-list '())

  (define collector (new collector%))
  (with-handlers ([(λ (_) #t) (λ (_) #f)])
    (parameterize ([current-load-relative-directory path-dir]
                   [current-namespace base-ns]
                   [current-annotations collector])
      (define stx (with-module-reading-parameterization
                    (lambda () (read-syntax path in))))
      (set! token-list (append (walk-stx stx) token-list))

      (define expanded (expand stx))
      (set! token-list (append (walk-expanded-stx path expanded) token-list))
      (add-syntax expanded)
      (done))

    (define drracket-styles (convert-drracket-color-styles (send collector get-styles)))
    (set! token-list (append drracket-styles token-list)))

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

(define (walk-stx stx)
  (syntax-parse stx
    #:datum-literals (#%module-begin)
    [() (list)]
    [(any1 any* ...)
     (append (walk-stx #'any1)
             (walk-stx #'(any* ...)))]
    [#(any1 any* ...)
     (append (walk-stx #'any1)
             (walk-stx #'(any* ...)))]
    [#%module-begin
     (list)]
    [atom (list (tag-of-atom-stx #'atom))]))

(define (walk-expanded-stx src stx)
  (syntax-parse stx
    #:datum-literals (lambda define-values)
    [(lambda (args ...) expr ...)
     (walk-expanded-stx src #'(expr ...))]
    [(define-values (fs) (lambda _ ...))
     (append (tags-of-stx-lst src #'(fs) 'function)
             (walk-expanded-stx src (drop (syntax-e stx) 2)))]
    [(any1 any* ...)
     (append (walk-expanded-stx src #'any1)
             (walk-expanded-stx src #'(any* ...)))]
    [_ (list)]))

(define (tags-of-stx-lst src stx-lst tag)
  (define (in-current-file? stx)
    (equal? src (syntax-source stx)))

  (let* ([stx-lst (syntax-e stx-lst)]
         [stx-lst-in-current-file (filter in-current-file? stx-lst)]
         [tag-lst (map (λ (stx) (tag-of-atom-stx stx tag)) stx-lst-in-current-file)])
    tag-lst))

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

