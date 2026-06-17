#lang racket/base

(require "../common/interfaces.rkt"
         "../common/path-util.rkt"
         "lexer/scan.rkt"
         "lexer/snapshot.rkt"
         "lexer/token-tree.rkt"
         racket/contract
         racket/list
         racket/match
         racket/path
         racket/string)

;; Turns a document's language declaration into editor policy in two stages:
;;
;;   1. Parse the header into a `Language-Header` record.
;;   2. Match it against the predefined `language-specs` list to produce a
;;      `Language-Policy` record consumed by diagnostics, formatting,
;;      expansion, and the lexer.
;;
;; Key design choices:
;;
;;   - Suffix fallback is used only when no header exists at all.
;;   - A complete header whose language is not in `language-specs` is kept as
;;     `unrecognized` rather than guessed.
;;   - Incomplete headers (started but malformed) produce `#f` for the policy
;;     language, just like missing headers.
;;
;; Recognized header shapes:
;;
;;   `#lang <lang ...>`
;;     Records the full language chain. Pass-through wrappers like
;;     `errortrace` are skipped to find the policy language.
;;     e.g. `#lang racket/base`, `#lang typed/racket/base`
;;
;;   `#lang reader <payload>`
;;     Captures the reader payload verbatim. The actual language is
;;     reader-defined, so no spec can be matched.
;;     e.g. `#lang reader syntax/module-reader`
;;
;;   `#reader <module>`
;;     Uses the reader module text as the policy selector.
;;     e.g. `#reader scribble/reader`
;;
;;   `(module name lang ...)`
;;     Uses the module language path as the policy selector.
;;     e.g. `(module demo racket/base ...)`

;; No recognizable language declaration appeared before the first
;; non-skippable token. The document may still be matched by file suffix.
(struct Missing-Header
  ()
  #:transparent)

;; A declaration form was started but contained too few tokens to identify its
;; language, reader, or module path. `kind` records which syntactic prefix was
;; recognized (e.g. `#lang`, `#reader`, `module`).
(struct/contract Incomplete-Header
  ([kind (or/c 'hash-lang 'hash-lang-reader 'reader 'module)]
   [range CharRange?]
   [body-start-idx exact-nonnegative-integer?])
  #:transparent)

;; A complete `#lang` declaration. The chain may contain pass-through
;; wrappers like `errortrace`; these are skipped when choosing the
;; language to use for policy.
(struct/contract HashLang-Header
  ([language-chain (and/c (listof string?) (not/c empty?))]
   [range CharRange?]
   [body-start-idx exact-nonnegative-integer?])
  #:transparent)

;; A complete `#lang reader` declaration. The payload describes a reader, not
;; a concrete language, so no `Language-Spec` can be selected here.
(struct/contract HashLangReader-Header
  ([reader-payload string?]
   [range CharRange?]
   [body-start-idx exact-nonnegative-integer?])
  #:transparent)

;; A complete `#reader` declaration. The reader module text that follows
;; the directive is used directly as the policy selector.
(struct/contract Reader-Header
  ([reader-module string?]
   [range CharRange?]
   [body-start-idx exact-nonnegative-integer?])
  #:transparent)

;; A raw `(module name lang ...)` form without a leading reader directive.
;; The module language path serves as the policy selector.
(struct/contract Module-Header
  ([language string?]
   [range CharRange?]
   [body-start-idx exact-nonnegative-integer?])
  #:transparent)

(define Language-Header/c
  (or/c Missing-Header?
        Incomplete-Header?
        HashLang-Header?
        HashLangReader-Header?
        Reader-Header?
        Module-Header?))

;; The single decision record consumed by the rest of doclib.
;;
;; The first three fields (status, range, body-start-idx) are direct
;; header facts for diagnostics. The remaining fields are operational
;; policy derived from the matched `Language-Spec`.
(struct/contract Language-Policy
  ([header-status (or/c 'missing 'incomplete 'complete)]
   [header-range (or/c CharRange? #f)]
   [body-start-idx exact-nonnegative-integer?]
   [policy-language (or/c symbol? 'unrecognized-language #f)]
   [body-mode (or/c 'sexp 'non-sexp 'unknown)]
   [format? boolean?]
   [require-header? boolean?]
   [expand? boolean?])
  #:transparent)

;; A known language family. Matched against the header via `name-rx`,
;; or against the file suffix via `suffixes` when no header is present.
;;
;; Fields:
;;   name            - canonical name, e.g. 'racket, 'scribble
;;   name-rx         - regex matched against the header language
;;   suffixes        - file extensions
;;                     only consulted when no header was found at all
;;   body-mode       - structural shape of code after the header
;;                     'sexp, 'non-sexp (e.g. scribble), or 'unknown
;;   format?         - whether formatting is supported
;;   require-header? - whether a recognized header is required
;;   expand?         - whether do macro expansion
(struct/contract Language-Spec
  ([name symbol?]
   [name-rx (or/c regexp? #f)]
   [suffixes (listof string?)]
   [body-mode (or/c 'sexp 'non-sexp 'unknown)]
   [format? boolean?]
   [require-header? boolean?]
   [expand? boolean?])
  #:transparent)

(define (Language-Spec~kw #:name name
                          #:name-rx name-rx
                          #:suffixes suffixes
                          #:body-mode body-mode
                          #:format? format?
                          #:require-header? [require-header? #t]
                          #:expand? [expand? #t])
  (Language-Spec name name-rx suffixes body-mode format? require-header? expand?))

(define language-specs
  (list
    (Language-Spec~kw #:name 'racket
                      #:name-rx #px"^racket(?:/.*)?$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'typed/racket
                      #:name-rx #px"^typed/racket(?:/.*)?$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'scheme
                      #:name-rx #px"^scheme(?:/.*)?$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'mzscheme
                      #:name-rx #px"^mzscheme$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'r5rs
                      #:name-rx #px"^r5rs$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'r6rs
                      #:name-rx #px"^r6rs$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'r7rs
                      #:name-rx #px"^r7rs$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'lazy
                      #:name-rx #px"^lazy$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'slideshow
                      #:name-rx #px"^slideshow$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'plai
                      #:name-rx #px"^plai(?:-typed|-lazy)?$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'plait
                      #:name-rx #px"^plait$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'htdp
                      #:name-rx #px"^htdp/(?:bsl\\+?|isl\\+?|asl)$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'eopl
                      #:name-rx #px"^eopl$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'sicp
                      #:name-rx #px"^sicp$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'swindle
                      #:name-rx #px"^swindle$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'frtime
                      #:name-rx #px"^frtime$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'rosette
                      #:name-rx #px"^rosette(?:/safe)?$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'pie
                      #:name-rx #px"^pie$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'at-exp
                      #:name-rx #px"^at-exp$"
                      #:suffixes '()
                      #:body-mode 'non-sexp
                      #:format? #f)
    (Language-Spec~kw #:name 's-exp
                      #:name-rx #px"^s-exp$"
                      #:suffixes '()
                      #:body-mode 'sexp
                      #:format? #t)
    (Language-Spec~kw #:name 'scribble
                      #:name-rx #px"^scribble(?:/.*)?$"
                      #:suffixes '("scrbl")
                      #:body-mode 'non-sexp
                      #:format? #f)
    (Language-Spec~kw #:name 'rhombus
                      #:name-rx #px"^rhombus(?:/.*)?$"
                      #:suffixes '("rhm")
                      #:body-mode 'non-sexp
                      #:format? #f)
    (Language-Spec~kw #:name 'racket-data
                      #:name-rx #f
                      #:suffixes '("rktd")
                      #:body-mode 'unknown
                      #:format? #f
                      #:require-header? #f
                      #:expand? #f)))

(define pass-through-language-names
  ;; Languages that wrap another without changing editor behavior.
  ;; They are skipped when picking the policy language.
  ;; e.g. `#lang errortrace racket/base` behaves like `#lang racket/base`.
  '("errortrace"))

;; ---- shared text/token helpers -----------------

(define (span-text text span)
  (substring text
             (LexerTokenSpan-start span)
             (LexerTokenSpan-end span)))

(define (token-node-text text node)
  (substring text
             (token-node-start node)
             (token-node-end node)))

(define (span->range span)
  (CharRange (LexerTokenSpan-start span)
             (LexerTokenSpan-end span)))

;; ---- header parsing -------------------------------------------------------

;; Parses the text of a `#lang` directive.
;;
;; `#lang reader` owns everything after `reader`, so the payload is
;; captured verbatim — it is not parsed as a module path here.
;; The `\\s` regex is intentionally permissive because the lexer may
;; split `reader` and its payload across lines.
(define (parse-lang-directive-text text span header-end)
  (match text
    [(regexp #px"^#lang\\s+reader\\s+(\\S.*)$"
             (list _ reader-payload))
     (HashLangReader-Header reader-payload (span->range span) header-end)]
    [(regexp #px"^#lang\\s+reader\\s*$")
     (Incomplete-Header 'hash-lang-reader (span->range span) header-end)]
    [(regexp #px"^#lang\\s+(\\S.*)$"
             (list _ chain-text))
     (define language-chain (string-split chain-text))
     (and (not (null? language-chain))
          (HashLang-Header language-chain (span->range span) header-end))]
    [_ #f]))

;; True when `text` starts with `#lang` but couldn't be parsed as a
;; directive.
(define (malformed-lang-directive-text? text)
  (and (string? text)
       (string-prefix? text "#lang")))

;; Parses a `#lang` token node. Well-formed directives arrive as
;; `lang-directive` tokens; malformed or unknown ones arrive as `error`
;; tokens. The text parser is tried on both so that a complete but
;; unknown language (e.g. `#lang my-custom-lang`) still produces a
;; `HashLang-Header`. Only a bare, unparseable `#lang` prefix becomes
;; an `Incomplete-Header`.
(define (parse-lang-directive-node text node next-idx)
  (match node
    [(Token-Leaf span)
     #:when (eq? 'lang-directive (LexerTokenSpan-type span))
     (parse-lang-directive-text (span-text text span) span next-idx)]
    [(Token-Leaf span)
     #:when (eq? 'error (LexerTokenSpan-type span))
     (define token-text (span-text text span))
     (or (parse-lang-directive-text token-text span next-idx)
         (and (malformed-lang-directive-text? token-text)
              (Incomplete-Header 'hash-lang (span->range span) next-idx)))]
    [_ #f]))

(define (node->range node)
  (CharRange (token-node-start node)
             (token-node-end node)))

(define (leaf-symbol-text text node)
  (match node
    [(Token-Leaf span)
     #:when (eq? 'symbol (LexerTokenSpan-type span))
     (span-text text span)]
    [_ #f]))

;; Parses a `#reader` directive. Reads the next non-skippable node as
;; the reader module; if none follows, the header is incomplete.
(define (parse-reader-directive-node text spans node next-idx)
  (match node
    [(Token-Leaf span)
     #:when (eq? 'reader-directive (LexerTokenSpan-type span))
     (define-values (reader-nodes reader-idx)
       (read-next-non-skippable-nodes/spans spans next-idx 1))
     (match reader-nodes
       [(list reader-node)
        (Reader-Header (token-node-text text reader-node)
                       (CharRange (LexerTokenSpan-start span)
                                  (token-node-end reader-node))
                       reader-idx)]
       ['()
        (Incomplete-Header 'reader (span->range span) reader-idx)])]
    [_ #f]))

;; Parses a raw `(module name lang ...)` form. Reads three
;; non-skippable children — the `module` keyword, the module id,
;; and the language path — and uses the language path as the policy
;; selector.
(define (parse-raw-module-node text node start-idx)
  (cond
    [(Token-List? node)
     (define children (token-node-children node))
     (define (node-symbol-text node)
       (leaf-symbol-text text node))
     (match (read-next-non-skippable-nodes children 3)
       [(list (app node-symbol-text "module") _mod-id mod-path-node)
        (Module-Header (token-node-text text mod-path-node)
                       (CharRange (token-node-start node)
                                  (token-node-end mod-path-node))
                       start-idx)]
       [(list (app node-symbol-text "module") _ ...)
        (Incomplete-Header 'module (node->range node) start-idx)]
       [_ #f])]
    [else #f]))

;; Top-level parser. Skips leading whitespace/comments, then tries
;; `#lang`, `#reader`, and raw `(module ...)` in order. First match
;; wins; returns `Missing-Header` if none match.
(define (parse-language-header-from-spans text spans [idx 0])
  (define-values (_skippable-nodes start-idx)
    (parse-skippable-node spans idx))
  (cond
    [(not (span-at spans start-idx)) (Missing-Header)]
    [else
     (define-values (node next-idx)
       (parse-token-node spans start-idx))
     (or (parse-lang-directive-node text node next-idx)
         (parse-reader-directive-node text spans node next-idx)
         (parse-raw-module-node text node start-idx)
         (Missing-Header))]))

(define (source->text+spans source)
  (cond
    [(LexerSnapshot? source)
     (values (LexerSnapshot-text source) (LexerSnapshot-tokens source))]
    [(string? source)
     (values source (text->lexer-token-spans source))]))

;; Parse the language header from a source string or lexer snapshot.
(define/contract (parse-language-header source [idx 0])
  (->* ((or/c string? LexerSnapshot?))
       (exact-nonnegative-integer?)
       Language-Header/c)
  (define-values (text spans)
    (source->text+spans source))
  (parse-language-header-from-spans text spans idx))

;; ---- language matching ----------------------------------------------------

(define (pass-through-language-name? name)
  (and (member name pass-through-language-names)
       #t))

;; Returns the first non-pass-through language in the chain.
;; Falls back to the first element if every language is a pass-through
;; (unusual, but guarantees a name is always available).
(define (first-non-pass-through language-chain)
  (or (for/or ([language-name (in-list language-chain)]
               #:unless (pass-through-language-name? language-name))
        language-name)
      (first language-chain)))

(define (match-by-language-name name)
  (and name
       (for/or ([spec (in-list language-specs)])
         (and (Language-Spec-name-rx spec)
              (regexp-match? (Language-Spec-name-rx spec) name)
              spec))))

(define (uri->suffix uri)
  (define extension (and uri (path-get-extension (uri->path uri))))
  (and extension
       (bytes->string/utf-8 (subbytes extension 1))))

(define (match-by-suffix uri)
  (define maybe-suffix (uri->suffix uri))
  (and maybe-suffix
       (for/or ([spec (in-list language-specs)])
         (and (member maybe-suffix (Language-Spec-suffixes spec))
              spec))))

;; Maps a parsed header + optional URI to a `Language-Spec`, or `#f`.
;; Suffix fallback only applies to `Missing-Header`; incomplete and
;; reader-defined headers always return `#f`.
(define/contract (header->language-match header uri)
  (-> Language-Header/c (or/c #f string?)
      (or/c Language-Spec? #f))
  (cond
    [(Missing-Header? header)
     (match-by-suffix uri)]
    [(Incomplete-Header? header)
     #f]
    [(HashLang-Header? header)
     (match-by-language-name
       (first-non-pass-through (HashLang-Header-language-chain header)))]
    [(HashLangReader-Header? header)
     #f]
    [(Reader-Header? header)
     (match-by-language-name (Reader-Header-reader-module header))]
    [(Module-Header? header)
     (match-by-language-name (Module-Header-language header))]))

;; ---- policy assembly ------------------------------------------------------

;; Chooses the policy language symbol. Returns the spec's name if
;; matched, `'unrecognized-language` for a complete header with no
;; matching spec, or `#f` for missing/incomplete headers.
(define (header+language-match->policy-language header language-match)
  (cond
    [(Language-Spec? language-match)
     (Language-Spec-name language-match)]
    [(eq? 'complete (header-status header))
     'unrecognized-language]
    [else #f]))

(define (language-match->body-mode language-match)
  (if (Language-Spec? language-match)
      (Language-Spec-body-mode language-match)
      'unknown))

(define (language-match-format? language-match)
  (and (Language-Spec? language-match)
       (Language-Spec-format? language-match)))

;; Defaults to `#t` when no spec matched. This means languages not in
;; the predefined list still require a header by default.
(define (language-match-require-header? language-match)
  (if (Language-Spec? language-match)
      (Language-Spec-require-header? language-match)
      #t))

(define (language-match-expand? language-match)
  (if (Language-Spec? language-match)
      (Language-Spec-expand? language-match)
      #t))

(define (header-status header)
  (cond
    [(Missing-Header? header) 'missing]
    [(Incomplete-Header? header) 'incomplete]
    [else 'complete]))

(define (header-range header)
  (cond
    [(Missing-Header? header) #f]
    [(Incomplete-Header? header) (Incomplete-Header-range header)]
    [(HashLang-Header? header) (HashLang-Header-range header)]
    [(HashLangReader-Header? header) (HashLangReader-Header-range header)]
    [(Reader-Header? header) (Reader-Header-range header)]
    [(Module-Header? header) (Module-Header-range header)]))

(define (header-body-start-idx header)
  (cond
    [(Missing-Header? header) 0]
    [(Incomplete-Header? header) (Incomplete-Header-body-start-idx header)]
    [(HashLang-Header? header) (HashLang-Header-body-start-idx header)]
    [(HashLangReader-Header? header) (HashLangReader-Header-body-start-idx header)]
    [(Reader-Header? header) (Reader-Header-body-start-idx header)]
    [(Module-Header? header) (Module-Header-body-start-idx header)]))

;; Parse a source and derive the full `Language-Policy`. The optional
;; `uri` enables suffix-based fallback when the header is missing.
(define/contract (source->language-policy source [uri #f])
  (->* ((or/c string? LexerSnapshot?))
       ((or/c #f string?))
       Language-Policy?)
  (define header (parse-language-header source))
  (define language-match
    (header->language-match header uri))
  (Language-Policy
    (header-status header)
    (header-range header)
    (header-body-start-idx header)
    (header+language-match->policy-language header language-match)
    (language-match->body-mode language-match)
    (language-match-format? language-match)
    (language-match-require-header? language-match)
    (language-match-expand? language-match)))

;; Same as `source->language-policy` but reuses already-lexed spans.
(define/contract (lexer-language-policy text spans [uri #f])
  (->* (string? (vectorof LexerTokenSpan?))
       ((or/c #f string?))
       Language-Policy?)
  (source->language-policy (LexerSnapshot text spans) uri))

(define (indenter-load-failure? e)
  (or (exn:fail:read? e)
      (exn:missing-module? e)
      (string-contains? (exn-message e) "Gtk initialization failed for display")))

(define/contract (get-indenter text)
  (-> string? (or/c procedure? #f))
  (with-handlers ([indenter-load-failure? (lambda (_e) #f)])
    (define maybe-reader-info
      (read-language (open-input-string text) (lambda () #f)))
    (and (procedure? maybe-reader-info)
         (maybe-reader-info 'drracket:indentation #f))))

(provide (struct-out Missing-Header)
         (struct-out Incomplete-Header)
         (struct-out HashLang-Header)
         (struct-out HashLangReader-Header)
         (struct-out Reader-Header)
         (struct-out Module-Header)
         (struct-out Language-Policy)
         parse-language-header
         source->language-policy
         lexer-language-policy
         get-indenter)
