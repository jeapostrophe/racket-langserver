#lang racket/base

(require "../common/path-util.rkt"
         "lexer.rkt"
         "lexer/shared.rkt"
         "lexer/token-tree.rkt"
         racket/contract
         racket/list
         racket/match
         racket/path
         racket/string)

(define language-node-source/c
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

(struct/contract Language-Node
  ([source language-node-source/c]
   [text string?]
   [start exact-nonnegative-integer?]
   [end exact-nonnegative-integer?])
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
    (Known-Language~kw #:name 'scribble
                       #:sexp? #f
                       #:suffixes '("scrbl")
                       #:name-rx #px"^scribble(?:/.*)?$")
    (Known-Language~kw #:name 'rhombus
                       #:sexp? #f
                       #:suffixes '("rhm")
                       #:name-rx #px"^rhombus(?:/.*)?$")))

(define (source->snapshot source)
  (cond
    [(LexerSnapshot? source) source]
    [(string? source) (build-lexer-snapshot source)]))

(define (span-text snapshot span)
  (substring (LexerSnapshot-text snapshot)
             (LexerTokenSpan-start span)
             (LexerTokenSpan-end span)))

(define (token-node-text snapshot node)
  (substring (LexerSnapshot-text snapshot)
             (token-node-start node)
             (token-node-end node)))

(define (make-language-node source text span)
  (Language-Node source
                 text
                 (LexerTokenSpan-start span)
                 (LexerTokenSpan-end span)))

(define (parse-lang-directive-text text span)
  (or (parse-reader-lang-directive-text text span)
      (parse-plain-lang-directive-text text span)))

(define (parse-reader-lang-directive-text text span)
  ;; `#lang reader` is the chaining-reader meta-language, so the payload after
  ;; `reader` is a module path, not just a single language name token. Once the
  ;; first line is known to be a `#lang` directive, keep the full text after
  ;; `reader`, even when `module-lexer` reports the line as `error` in our
  ;; snapshot setup.
  (cond
    [(regexp-match #px"^#lang\\s+reader[ \t]+([^\r\n]+)$" text)
     => (lambda (match-data)
          (define reader-payload (second match-data))
          (make-language-node 'reader-lang reader-payload span))]
    [else #f]))

(define (parse-plain-lang-directive-text text span)
  (cond
    [(regexp-match #px"^#lang\\s+(\\S+)$" text)
     => (lambda (match-data)
          (define language-name (second match-data))
          (make-language-node 'lang-directive language-name span))]
    [else #f]))

(define (malformed-lang-directive-text? text)
  (and (string? text)
       (string-prefix? text "#lang")))

(define (parse-lang-directive-node snapshot node)
  ;; `module-lexer` uses `read-language` to classify language lines: resolved
  ;; `#lang` forms become `lang-directive`, while failed resolution stays
  ;; `error`. Preserve that distinction for plain `#lang`, but still recover
  ;; `#lang reader ...` from `error` tokens because its payload is a module path
  ;; that may be useful even when the chained reader cannot be loaded.
  (match node
    [(Token-Leaf span)
     #:when (eq? 'lang-directive (LexerTokenSpan-type span))
     (parse-lang-directive-text (span-text snapshot span) span)]
    [(Token-Leaf span)
     #:when (eq? 'error (LexerTokenSpan-type span))
     (define text (span-text snapshot span))
     (or (parse-lang-directive-text text span)
         ;; Explicitly recognize `#lang` error tokens as malformed language
         ;; directives.
         (and (malformed-lang-directive-text? text)
              (make-language-node 'malformed-lang-directive "" span)))]
    [_ #f]))

(define (leaf-symbol-text snapshot node)
  (match node
    [(Token-Leaf span)
     #:when (eq? 'symbol (LexerTokenSpan-type span))
     (span-text snapshot span)]
    [_ #f]))

(define (parse-reader-directive-node snapshot spans node next-idx)
  ;; `read-language` is only for `#lang` lines, so `#reader` lines are handled
  ;; syntactically. The lexer exposes the marker and the following reader name
  ;; separately; keep the next non-skippable node as the reader text.
  (match node
    [(Token-Leaf span)
     #:when (eq? 'reader-directive (LexerTokenSpan-type span))
     (define-values (reader-nodes _reader-idx)
       (read-next-non-skippable-nodes/spans spans next-idx 1))
     (match reader-nodes
       [(list reader-node)
        (Language-Node
          'reader-directive
          (token-node-text snapshot reader-node)
          (LexerTokenSpan-start span)
          (token-node-end reader-node))]
       ['()
        (make-language-node 'malformed-reader-directive "" span)])]
    [_ #f]))

(define (parse-raw-module-node snapshot node)
  ;; Raw `(module name lang ...)` forms do not go through `read-language` either,
  ;; so the language position is syntactic. If the first significant child is
  ;; `module`, keep the third significant child as the language text even when it
  ;; names an unknown module path; `parse-language` will decide whether it is
  ;; known.
  (match node
    [(Token-Tree open-span children _close-span)
     (define (node-symbol-text node)
       (leaf-symbol-text snapshot node))

     (match (read-next-non-skippable-nodes children 3)
       [(list (app node-symbol-text "module") _mod-id mod-path-node)
        (Language-Node
          'raw-module
          (token-node-text snapshot mod-path-node)
          (LexerTokenSpan-start open-span)
          (token-node-end mod-path-node))]
       [(list (app node-symbol-text "module") _ ...)
        (Language-Node
          'malformed-raw-module
          ""
          (token-node-start node)
          (token-node-end node))]
       [_ #f])]
    [_ #f]))

(define (find-language-by-text text)
  (for/or ([language (in-list known-languages)])
    (and (regexp-match? (Known-Language-name-rx language) text)
         language)))

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

(define/contract (parse-language-node source [idx 0])
  (->* ((or/c string? LexerSnapshot?))
       (exact-nonnegative-integer?)
       (or/c Language-Node? #f))
  (define snapshot (source->snapshot source))
  (define spans (LexerSnapshot-tokens snapshot))
  (define-values (_skippable-nodes start-idx)
    (parse-skippable-node spans idx))
  (cond
    [(not (span-at spans start-idx)) #f]
    [else
     (define-values (node next-idx)
       (parse-token-node spans start-idx))
     (or (parse-lang-directive-node snapshot node)
         (parse-reader-directive-node snapshot spans node next-idx)
         (parse-raw-module-node snapshot node))]))

(define/contract (parse-language source [uri #f])
  (->* ((or/c string? LexerSnapshot?))
       ((or/c #f string?))
       (or/c Known-Language? 'unrecognized-language #f))
  (define language-node* (parse-language-node source))
  (cond
    [language-node*
     (or (find-language-by-text (Language-Node-text language-node*))
         'unrecognized-language)]
    [else (guess-language-by-uri uri)]))

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

(provide (struct-out Language-Node)
         (struct-out Known-Language)
         Known-Language~kw
         known-languages
         find-language-by-text
         parse-language-node
         parse-language
         guess-language-by-uri
         sexp-language?
         get-indenter)
