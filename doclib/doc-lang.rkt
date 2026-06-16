#lang racket/base

(require "../common/path-util.rkt"
         "lexer/scan.rkt"
         "lexer/snapshot.rkt"
         "lexer/token-tree.rkt"
         racket/contract
         racket/list
         racket/match
         racket/path
         racket/string)

; Base struct for a parsed language declaration.  Holds the language / reader /
; module-path name extracted from the source, or #f when the declaration is
; present but malformed (e.g. `#lang` with no language name).
(struct/contract Language-Declaration
  ([name (or/c string? #f)])
  #:transparent)

; A plain `#lang` directive.  Inherits `name` from Language-Declaration (the
; language name, e.g. "racket/base").
(struct/contract HashLang-Declaration Language-Declaration
  ()
  #:transparent)

; A wrapper-shaped `#lang` directive such as `#lang at-exp racket/base`.
; Inherits `name` from HashLang-Declaration (the wrapper language, e.g.
; "at-exp"). `delegate-name` records the wrapped language/reader selector.
(struct/contract WrapperHashLang-Declaration HashLang-Declaration
  ([delegate-name (or/c string? #f)])
  #:transparent)

; A `#reader` directive.  Inherits `name` from Language-Declaration (the
; reader module path, e.g. "scribble/reader").
(struct/contract Reader-Declaration Language-Declaration
  ()
  #:transparent)

; A raw `(module name lang ...)` form.  Inherits `name` from Language-Declaration
; (the language position in the module form, e.g. "racket/base").
(struct/contract Module-Declaration Language-Declaration
  ()
  #:transparent)

; Describes the language preamble at the top of a source file.  When there is no
; language declaration at all (#f from parse-language-prefix), no
; Language-Prefix is constructed and downstream code falls back to
; guess-language-by-uri (file extension) or #f.
;   declaration    - the parsed Language-Declaration (Lang-, Reader-, or Module-)
;   start-pos      - character offset where the preamble begins
;   end-pos        - character offset where the language-name span ends
;   body-start-idx - token index into the lexer-span vector where the source code body begins
(struct/contract Language-Prefix
  ([declaration Language-Declaration?]
   [start-pos exact-nonnegative-integer?]
   [end-pos exact-nonnegative-integer?]
   [body-start-idx exact-nonnegative-integer?])
  #:transparent)

(struct/contract Known-Language
  ([name symbol?]
   [sexp? boolean?]
   [suffixes (listof string?)]
   [name-rx regexp?])
  #:transparent)

(define (Known-Language~kw #:name name
                           #:sexp? sexp?
                           #:suffixes suffixes
                           #:name-rx name-rx)
  (Known-Language name sexp? suffixes name-rx))

(define known-languages
  (list
    (Known-Language~kw #:name 'racket
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^racket(?:/.*)?$")
    (Known-Language~kw #:name 'typed/racket
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^typed/racket(?:/.*)?$")
    (Known-Language~kw #:name 'scheme
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^scheme(?:/.*)?$")
    (Known-Language~kw #:name 'mzscheme
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^mzscheme$")
    (Known-Language~kw #:name 'r5rs
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^r5rs$")
    (Known-Language~kw #:name 'r6rs
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^r6rs$")
    (Known-Language~kw #:name 'r7rs
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^r7rs$")
    (Known-Language~kw #:name 'lazy
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^lazy$")
    (Known-Language~kw #:name 'slideshow
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^slideshow$")
    (Known-Language~kw #:name 'plai
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^plai(?:-typed|-lazy)?$")
    (Known-Language~kw #:name 'plait
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^plait$")
    (Known-Language~kw #:name 'htdp
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^htdp/(?:bsl\\+?|isl\\+?|asl)$")
    (Known-Language~kw #:name 'eopl
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^eopl$")
    (Known-Language~kw #:name 'sicp
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^sicp$")
    (Known-Language~kw #:name 'swindle
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^swindle$")
    (Known-Language~kw #:name 'frtime
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^frtime$")
    (Known-Language~kw #:name 'rosette
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^rosette(?:/safe)?$")
    (Known-Language~kw #:name 'pie
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^pie$")
    (Known-Language~kw #:name 'at-exp
                       #:sexp? #f
                       #:suffixes '()
                       #:name-rx #px"^at-exp$")
    (Known-Language~kw #:name 's-exp
                       #:sexp? #t
                       #:suffixes '()
                       #:name-rx #px"^s-exp$")
    (Known-Language~kw #:name 'scribble
                       #:sexp? #f
                       #:suffixes '("scrbl")
                       #:name-rx #px"^scribble(?:/.*)?$")
    (Known-Language~kw #:name 'rhombus
                       #:sexp? #f
                       #:suffixes '("rhm")
                       #:name-rx #px"^rhombus(?:/.*)?$")))

(define (source->text+spans source)
  (cond
    [(LexerSnapshot? source)
     (values (LexerSnapshot-text source) (LexerSnapshot-tokens source))]
    [(string? source)
     (values source (text->lexer-token-spans source))]))

(define (span-text text span)
  (substring text
             (LexerTokenSpan-start span)
             (LexerTokenSpan-end span)))

(define (token-node-text text node)
  (substring text
             (token-node-start node)
             (token-node-end node)))

(define (make-language-prefix declaration span body-start-idx)
  (Language-Prefix declaration
                   (LexerTokenSpan-start span)
                   (LexerTokenSpan-end span)
                   body-start-idx))

(define (parse-lang-directive-text text span header-end)
  (match text
    ;; `#lang reader` takes a reader module path as the rest of the same line.
    ;; The lexer may include the next line in a bare `#lang reader` error token,
    ;; so keep the payload match from crossing newlines.
    [(regexp #px"^#lang\\s+reader[^\\S\r\n]+([^\r\n]+)$"
             (list _ reader-payload))
     (make-language-prefix
       (WrapperHashLang-Declaration "reader" reader-payload)
       span header-end)]
    ;; Other two-word `#lang` forms are wrapper + language, including unknown
    ;; wrappers. Whether the wrapper is recognized affects body mode later.
    [(regexp #px"^#lang\\s+(\\S+)\\s+(\\S+)$"
             (list _ wrapper language-name))
     (make-language-prefix
       (WrapperHashLang-Declaration wrapper language-name)
       span header-end)]
    ;; A single word after `#lang` is the language itself.
    [(regexp #px"^#lang\\s+(\\S+)$"
             (list _ language-name))
     (make-language-prefix
       (HashLang-Declaration language-name)
       span header-end)]
    [_ #f]))

(define (malformed-lang-directive-text? text)
  (and (string? text)
       (string-prefix? text "#lang")))

(define (parse-lang-directive-node text node next-idx)
  ;; `module-lexer` uses `read-language` to classify language lines: resolved
  ;; `#lang` forms become `lang-directive`, while failed resolution stays as an
  ;; `error` token. An error token that still starts with `#lang` may be a
  ;; recoverable declaration like `#lang reader <module-path>`, or it may be
  ;; malformed. Try parsing first; then report present-but-malformed `#lang`.
  (match node
    [(Token-Leaf span)
     #:when (eq? 'lang-directive (LexerTokenSpan-type span))
     (parse-lang-directive-text (span-text text span) span next-idx)]
    [(Token-Leaf span)
     #:when (eq? 'error (LexerTokenSpan-type span))
     (define token-text (span-text text span))
     (or (parse-lang-directive-text token-text span next-idx)
         (and (malformed-lang-directive-text? token-text)
              (make-language-prefix (HashLang-Declaration #f) span next-idx)))]
    [_ #f]))

(define (leaf-symbol-text text node)
  (match node
    [(Token-Leaf span)
     #:when (eq? 'symbol (LexerTokenSpan-type span))
     (span-text text span)]
    [_ #f]))

(define (parse-reader-directive-node text spans node next-idx)
  ;; `read-language` is only for `#lang` lines, so `#reader` lines are handled
  ;; syntactically. The lexer exposes the marker and the following reader name
  ;; separately; keep the next non-skippable node as the reader text.
  (match node
    [(Token-Leaf span)
     #:when (eq? 'reader-directive (LexerTokenSpan-type span))
     (define-values (reader-nodes reader-idx)
       (read-next-non-skippable-nodes/spans spans next-idx 1))
     (match reader-nodes
       [(list reader-node)
        (Language-Prefix (Reader-Declaration (token-node-text text reader-node))
                         (LexerTokenSpan-start span)
                         (token-node-end reader-node)
                         reader-idx)]
       ['()
        (make-language-prefix
          (Reader-Declaration #f)
          span
          reader-idx)])]
    [_ #f]))

(define (parse-raw-module-node text node start-idx)
  ;; Raw `(module name lang ...)` forms do not go through `read-language` either,
  ;; so the language position is syntactic. If the first significant child is
  ;; `module`, keep the third significant child as the language text even when it
  ;; names an unknown module path; `parse-language` will decide whether it is
  ;; known.
  (cond
    [(Token-List? node)
     (define children (token-node-children node))
     (define (node-symbol-text node)
       (leaf-symbol-text text node))
     (match (read-next-non-skippable-nodes children 3)
       [(list (app node-symbol-text "module") _mod-id mod-path-node)
        (Language-Prefix (Module-Declaration (token-node-text text mod-path-node))
                         (token-node-start node)
                         (token-node-end mod-path-node)
                         start-idx)]
       [(list (app node-symbol-text "module") _ ...)
        (Language-Prefix (Module-Declaration #f)
                         (token-node-start node)
                         (token-node-end node)
                         start-idx)]
       [_ #f])]
    [else #f]))

(define (find-language-by-text text)
  (for/or ([language (in-list known-languages)])
    (and (regexp-match? (Known-Language-name-rx language) text)
         language)))

(define/contract (racket-data-file-path? path)
  (-> path-string? boolean?)
  (equal? (path-get-extension path) #".rktd"))

(define/contract (requires-expansion? path)
  (-> path-string? boolean?)
  (not (racket-data-file-path? path)))

(define/contract (requires-language-declaration? path)
  (-> path-string? boolean?)
  (not (racket-data-file-path? path)))

(define (uri->suffix uri)
  (define extension (path-get-extension (uri->path uri)))
  (bytes->string/utf-8 (subbytes extension 1)))

(define/contract (guess-language-by-uri uri)
  (-> (or/c #f string?)
      (or/c Known-Language? #f))
  (define maybe-suffix (and uri (uri->suffix uri)))
  (and maybe-suffix
       (for/or ([language (in-list known-languages)])
         (and (member maybe-suffix (Known-Language-suffixes language))
              language))))

(define (parse-language-prefix-from-spans text spans [idx 0])
  (define-values (_skippable-nodes start-idx)
    (parse-skippable-node spans idx))
  (cond
    [(not (span-at spans start-idx)) #f]
    [else
     (define-values (node next-idx)
       (parse-token-node spans start-idx))
     (or (parse-lang-directive-node text node next-idx)
         (parse-reader-directive-node text spans node next-idx)
         (parse-raw-module-node text node start-idx))]))

(define/contract (parse-language-prefix source [idx 0])
  (->* ((or/c string? LexerSnapshot?))
       (exact-nonnegative-integer?)
       (or/c Language-Prefix? #f))
  (define-values (text spans)
    (source->text+spans source))
  (parse-language-prefix-from-spans text spans idx))

(define (language-declaration-malformed? declaration)
  (not (Language-Declaration-name declaration)))

(define (language-prefix-language-text language-prefix)
  (Language-Declaration-name
    (Language-Prefix-declaration language-prefix)))

(define (language-prefix-delegate-text language-prefix)
  (match (Language-Prefix-declaration language-prefix)
    [(WrapperHashLang-Declaration _ delegate-name) delegate-name]
    [_ #f]))

(define (language-prefix->language language-prefix uri)
  (cond
    [language-prefix
     ;; In wrapper forms like `#lang at-exp racket/base`, the declaration name
     ;; is the wrapper language (`at-exp`). The delegate is separate metadata.
     (define maybe-language-text
       (language-prefix-language-text language-prefix))
     (or (and maybe-language-text
              (find-language-by-text maybe-language-text))
         'unrecognized-language)]
    [else (guess-language-by-uri uri)]))

(define/contract (parse-language source [uri #f])
  (->* ((or/c string? LexerSnapshot?))
       ((or/c #f string?))
       (or/c Known-Language? 'unrecognized-language #f))
  (define prefix (parse-language-prefix source))
  (language-prefix->language prefix uri))

(struct/contract Language-Info
  ([prefix (or/c Language-Prefix? #f)]
   [language (or/c Known-Language? 'unrecognized-language #f)]
   [body-mode (or/c 'sexp 'non-sexp 'unknown)])
  #:transparent)

(define/contract (lexer-language-info text spans [uri #f])
  (->* (string? (vectorof LexerTokenSpan?))
       ((or/c #f string?))
       Language-Info?)
  (define maybe-prefix (parse-language-prefix-from-spans text spans))
  (define maybe-language
    (language-prefix->language maybe-prefix uri))
  (Language-Info maybe-prefix
                 maybe-language
                 (language->body-mode maybe-language)))

(define (language->body-mode maybe-language)
  (cond
    [(not (Known-Language? maybe-language)) 'unknown]
    [(Known-Language-sexp? maybe-language) 'sexp]
    [else 'non-sexp]))

(define/contract (sexp-language? source [uri #f])
  (->* ((or/c string? LexerSnapshot?))
       ((or/c #f string?))
       boolean?)
  (define-values (text spans)
    (source->text+spans source))
  (eq? 'sexp (Language-Info-body-mode (lexer-language-info text spans uri))))

(define (indenter-load-failure? e)
  (or (exn:fail:read? e)
      (exn:missing-module? e)
      (string-contains? (exn-message e) "Gtk initialization failed for display")))

(define/contract (get-indenter text)
  (-> string? (or/c procedure? #f))
  (with-handlers ([indenter-load-failure? (lambda (_e) #f)])
    (define maybe-language-info
      (read-language (open-input-string text) (lambda () #f)))
    (and (procedure? maybe-language-info)
         (maybe-language-info 'drracket:indentation #f))))

(provide (struct-out Language-Declaration)
         (struct-out HashLang-Declaration)
         (struct-out WrapperHashLang-Declaration)
         (struct-out Reader-Declaration)
         (struct-out Module-Declaration)
         (struct-out Language-Prefix)
         language-declaration-malformed?
         language-prefix-language-text
         language-prefix-delegate-text
         (struct-out Known-Language)
         Language-Info
         Language-Info?
         Language-Info-prefix
         Language-Info-language
         Language-Info-body-mode
         Known-Language~kw
         known-languages
         find-language-by-text
         racket-data-file-path?
         requires-expansion?
         requires-language-declaration?
         parse-language-prefix
         parse-language
         guess-language-by-uri
         lexer-language-info
         sexp-language?
         get-indenter)
