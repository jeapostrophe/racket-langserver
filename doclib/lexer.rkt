#lang racket/base

(require "doc-lang.rkt"
         "lexer/snapshot.rkt"
         "lexer/state.rkt")

;; Public lexer facade. Lower modules expose parser and tree internals for
;; focused tests, but ordinary callers should use this stable API.

(provide (struct-out LexerTokenSpan)
         (struct-out LexerSnapshot)
         build-lexer-snapshot
         build-lexer-state
         LexerState?
         LexerState-snapshot
         LexerState-language-policy
         lexer-state-body-mode
         lexer-state-token-at
         lexer-state-symbol-at
         lexer-state-containing-open-paren
         lexer-state-form-head-at
         lexer-state-sexp-comment-spans
         lexer-snapshot-span->entry
         lexer-token-span-contains-pos?
         in-lexer-snapshot
         for-each-lexer-snapshot-entry
         lexer-snapshot-token-at
         lexer-snapshot-symbol-at
         lexer-language-policy
         (struct-out Language-Policy))
