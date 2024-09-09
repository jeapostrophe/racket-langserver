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

(define collector%
  (class (annotations-mixin object%)
    (define styles '())

    (super-new)

    (define/override (syncheck:find-source-object stx)
      #f)

    (define/override (syncheck:color-range src start end style)
      (when (< start end)
        (set! styles (cons (list start end style) styles))))

    (define/override (syncheck:add-definition-target src start finish id mods)
      (when (< start finish)
        (set! styles (cons (list start finish 'definition) styles))))

    (define/public (get-color)
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

    (define drracket-styles (convert-drracket-color-styles (send collector get-color)))
    (set! token-list (append drracket-styles token-list)))

  (let* ([tokens-no-false (filter-not false? token-list)]
         [tokens-no-out-bounds (filter (λ (t) (< -1 (first t) (string-length code-str)))
                                       tokens-no-false)]
         [tokens-in-order (sort tokens-no-out-bounds < #:key first)]
         [same-loc-token-groups (group-by first tokens-in-order)]
         [tokens-merge-types
          (for/list ([group same-loc-token-groups])
            (define fst (first group))
            (list (first fst) (second fst) (map third group)))]
         [result-tokens
          (for*/list ([t tokens-merge-types]
                      [type (in-value (select-type (third t)))]
                      [modifiers (in-value (filter (λ (t) (memq t *semantic-token-modifiers*))
                                                   (third t)))]
                      #:when (not (false? type)))
            (SemanticToken (first t) (second t) type modifiers))])
    result-tokens))

(define (convert-drracket-color-styles styles)
  (for/list ([s styles])
    (match s
      [(list start end "drracket:check-syntax:lexically-bound")
       (list start end 'variable)]
      [_ #f])))

(define (select-type types)
  (define valid-types (filter (λ (t) (memq t *semantic-token-types*)) types))
  (cond [(null? valid-types)
         #f]
        [(memq 'function valid-types)
         'function]
        [(memq 'variable valid-types)
         'variable]
        [else (first valid-types)]))

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
    [atom (list (stx-typeof #'atom))]))

(define (walk-expanded-stx src stx)
  (syntax-parse stx
    #:datum-literals (lambda define-values)
    [(lambda (args ...) expr ...)
     (walk-expanded-stx src #'(expr ...))]
    [(define-values (fs) (lambda _ ...))
     (append (stx-lst-typeof src #'(fs) 'function)
             (walk-expanded-stx src (drop (syntax-e stx) 2)))]
    [(any1 any* ...)
     (append (walk-expanded-stx src #'any1)
             (walk-expanded-stx src #'(any* ...)))]
    [_ (list)]))

(define (stx-lst-typeof src stx-lst type)
  (define (in-current-file? stx)
    (equal? src (syntax-source stx)))

  (let* ([stx-lst (syntax-e stx-lst)]
         [stx-lst-in-current-file (filter in-current-file? stx-lst)]
         [type-lst (map (λ (stx) (stx-typeof stx type)) stx-lst-in-current-file)])
    type-lst))

(define (stx-typeof atom-stx [expect-type #f])
  (define pos+1 (syntax-position atom-stx))
  (define len (syntax-span atom-stx))
  (if (or (not pos+1) (not len) (= len 0)
          (not (syntax-original? atom-stx)))
      #f
      (let ([pos (sub1 pos+1)])
        (list pos (+ pos len)
              (if (false? expect-type)
                  (get-type (syntax-e atom-stx))
                  expect-type)))))

(define (get-type atom)
  (match atom
    [(? number?) 'number]
    [(? symbol?) 'symbol]
    [(? string?) 'string]
    [(? bytes?) 'string]
    [(? regexp?) 'regexp]
    [_ 'unknown]))

