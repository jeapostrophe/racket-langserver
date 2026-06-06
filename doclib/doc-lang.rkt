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

(define language-prefix-source/c
  (or/c
    ;; `#lang racket/base`
    'lang-directive
    ;; Present `#lang` line with no usable selector.
    'malformed-lang-directive
    ;; `#reader scribble/reader`
    'reader-directive
    ;; Present `#reader` line with no usable reader.
    'malformed-reader-directive
    ;; `#lang reader syntax/module-reader`
    'reader-lang
    ;; Present raw `module` form with no language position.
    'malformed-raw-module
    ;; `(module name racket/base ...)`
    'raw-module))

(struct/contract Language-Prefix
  ([source language-prefix-source/c]
   [text string?]
   [start exact-nonnegative-integer?]
   [end exact-nonnegative-integer?]
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

(define (make-language-prefix source text span body-start-idx)
  (Language-Prefix source
                   text
                   (LexerTokenSpan-start span)
                   (LexerTokenSpan-end span)
                   body-start-idx))

(define (parse-lang-directive-text text span header-end)
  (or (parse-reader-lang-directive-text text span header-end)
      (parse-plain-lang-directive-text text span header-end)))

(define (parse-reader-lang-directive-text text span header-end)
  ;; `#lang reader` is the chaining-reader meta-language, so the payload after
  ;; `reader` is a module path, not just a single language name token. Once the
  ;; first line is known to be a `#lang` directive, keep the full text after
  ;; `reader`, even when `module-lexer` reports the line as `error` in our
  ;; snapshot setup.
  (cond
    [(regexp-match #px"^#lang\\s+reader[ \t]+([^\r\n]+)$" text)
     => (lambda (match-data)
          (define reader-payload (second match-data))
          (make-language-prefix 'reader-lang reader-payload span header-end))]
    [else #f]))

(define (parse-plain-lang-directive-text text span header-end)
  (cond
    [(regexp-match #px"^#lang\\s+(\\S+)$" text)
     => (lambda (match-data)
          (define language-name (second match-data))
          (make-language-prefix 'lang-directive language-name span header-end))]
    [else #f]))

(define (malformed-lang-directive-text? text)
  (and (string? text)
       (string-prefix? text "#lang")))

(define (parse-lang-directive-node text node next-idx)
  ;; `module-lexer` uses `read-language` to classify language lines: resolved
  ;; `#lang` forms become `lang-directive`, while failed resolution stays
  ;; `error`. Preserve that distinction for plain `#lang`, but still recover
  ;; `#lang reader ...` from `error` tokens because its payload is a module path
  ;; that may be useful even when the chained reader cannot be loaded.
  (match node
    [(Token-Leaf span)
     #:when (eq? 'lang-directive (LexerTokenSpan-type span))
     (parse-lang-directive-text (span-text text span) span next-idx)]
    [(Token-Leaf span)
     #:when (eq? 'error (LexerTokenSpan-type span))
     (define token-text (span-text text span))
     (or (parse-lang-directive-text token-text span next-idx)
         ;; Explicitly recognize `#lang` error tokens as malformed language
         ;; directives.
         (and (malformed-lang-directive-text? token-text)
              (make-language-prefix 'malformed-lang-directive "" span next-idx)))]
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
        (Language-Prefix 'reader-directive
                         (token-node-text text reader-node)
                         (LexerTokenSpan-start span)
                         (token-node-end reader-node)
                         reader-idx)]
       ['()
        (make-language-prefix 'malformed-reader-directive "" span reader-idx)])]
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
        (Language-Prefix 'raw-module
                         (token-node-text text mod-path-node)
                         (token-node-start node)
                         (token-node-end mod-path-node)
                         start-idx)]
       [(list (app node-symbol-text "module") _ ...)
        (Language-Prefix 'malformed-raw-module
                         ""
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

(define (language-prefix->language language-prefix uri)
  (cond
    [language-prefix
     (or (find-language-by-text (Language-Prefix-text language-prefix))
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
  (define maybe-language (language-prefix->language maybe-prefix uri))
  (define body-mode
    (cond
      [(and (Known-Language? maybe-language)
            (Known-Language-sexp? maybe-language))
       'sexp]
      [(Known-Language? maybe-language)
       'non-sexp]
      [else 'unknown]))
  (Language-Info maybe-prefix
                 maybe-language
                 body-mode))

(define/contract (sexp-language? source [uri #f])
  (->* ((or/c string? LexerSnapshot?))
       ((or/c #f string?))
       boolean?)
  (define maybe-language (parse-language source uri))
  (and (Known-Language? maybe-language)
       (Known-Language-sexp? maybe-language)))

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

(provide (struct-out Language-Prefix)
         language-prefix-source/c
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
