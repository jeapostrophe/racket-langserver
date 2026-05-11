#lang racket/base

(require "shared.rkt"
         racket/contract
         racket/match
         racket/string
         syntax-color/module-lexer
         syntax-color/racket-lexer)

;; Raw syntax-color integration. This module turns source text into normalized
;; token spans and does not know about token trees, snapshots, or language
;; policy.

(define (lang-directive? txt)
  (and (string? txt)
       (string-prefix? txt "#lang ")))

;; Normalize token types to stable project-local names.
(define (normalize-token type text)
  (match* (type text)
    [('parenthesis (or "(" "[" "{"))
     'open-paren]
    [('parenthesis (or ")" "]" "}"))
     'close-paren]
    [(_ "'") 'quote]
    [(_ "`") 'quasiquote]
    [(_ ",") 'unquote]
    [(_ "#;") 'sexp-comment]
    [(_ "#'") 'syntax-quote]
    [(_ "#`") 'syntax-quasiquote]
    [(_ "#,@") 'syntax-unquote-splicing]
    [(_ "#,") 'syntax-unquote]
    [(_ ",@") 'unquote-splicing]
    [(_ "#reader") 'reader-directive]
    ;; `module-lexer` uses `read-language` to detect lang directives. Correct
    ;; lang lines are assigned `other`; incorrect ones are assigned `error`.
    [('other (? lang-directive?)) 'lang-directive]
    [(_ _) type]))

;; Racket lexers report 1-based positions. Normalize to zero-based offsets and
;; clamp at zero so synthetic positions do not go negative.
(define (normalize-lexer-pos pos)
  (max 0 (sub1 pos)))

(define (record-lexer-span type start end)
  (define normalized-start (normalize-lexer-pos start))
  (define normalized-end (normalize-lexer-pos end))
  (make-lexer-span normalized-start normalized-end type))

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

;; `module-lexer` returns the lexer to use on the remaining input as one of:
;;   - a procedure          (simple lexers),
;;   - a `(procedure . mode)` pair (stateful lexers), or
;;   - a non-lexer value such as `#f` (meaning “no specific lexer available”).
;; For `#lang` modules the result is a language-specific lexer (e.g. Rhombus).
;; For `#reader` headers the result is currently a non-lexer value because
;; `read-language` (which `module-lexer` relies on) does not recognize
;; `#reader`, so we fall back to `racket-lexer` even when the reader names
;; a non-sexp language.
(define (module-lexer->lexer lexer)
  (if (or (procedure? lexer) (pair? lexer))
      lexer
      racket-lexer))

(define (get-initial-lexer-state in)
  (define-values (txt type paren? start end _backup lexer)
    (module-lexer in 0 #f))
  (values txt type paren? start end (module-lexer->lexer lexer)))

(define/contract (text->lexer-token-spans text)
  (-> string? (vectorof LexerTokenSpan?))
  (define in (open-input-string text))
  (port-count-lines! in)
  (define-values (initial-txt initial-type _initial-paren? initial-start initial-end lexer)
    (get-initial-lexer-state in))
  ;; The initial token comes from `module-lexer`; subsequent tokens come from
  ;; the language-specific lexer selected above.
  (define initial-span
    (and (not (eof-object? initial-txt))
         (record-lexer-span (normalize-token initial-type initial-txt)
                            initial-start
                            initial-end)))
  (define rest-token-spans
    (for*/list ([lst (in-port (lexer-wrap lexer) in)]
                [span (in-value
                        (match lst
                          [(list txt type _paren? start end)
                           (record-lexer-span (normalize-token type txt) start end)]))]
                #:when span)
      span))
  (list->vector
    (if initial-span
        (cons initial-span rest-token-spans)
        rest-token-spans)))

(provide text->lexer-token-spans)
