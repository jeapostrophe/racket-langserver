#lang racket/base

;; Shared helpers for `tests/lib/doc-*-test.rkt`.
(require "../silent-logger.rkt"
         racket/class
         racket/set
         "../../common/interfaces.rkt"
         "../../common/path-util.rkt"
         "../../common/source.rkt"
         "../../doclib/check-syntax.rkt"
         "../../doclib/doc.rkt"
         "../../doclib/doc-trace.rkt"
         "../../doclib/editor.rkt"
         "../../doclib/lexer.rkt")

(provide make-trace
         check-syntax-diagnostics
         typed-racket-available?
         make-expanded-doc)

;; Keep the source, populated editor, and lexer on the same text snapshot.
(define (make-trace path text)
  (define doc-text (new lsp-editor%))
  (send doc-text insert text 0)
  (new build-trace%
    [source (path->source path)]
    [doc-text doc-text]
    [lexer-state (build-lexer-state text (path->uri path))]))

(define (check-syntax-diagnostics source text)
  (define uri (source-uri source))
  (define doc-text (new lsp-editor%))
  (send doc-text insert text 0)
  (define lexer-state (build-lexer-state text uri))
  (set->list (send (CSResult-trace (check-syntax source doc-text lexer-state)) get-warn-diags)))

(define (typed-racket-available?)
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (dynamic-require 'typed/racket #f)
    #t))

(define (make-expanded-doc [uri "file:///tmp/doc-test.rkt"]
                           [text "#lang racket\n(define x 1)\nx"])
  (define d (make-doc uri text))
  (doc-expand! d)
  d)
