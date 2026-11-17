#lang racket/base

(provide hint->triple
         inlay-hints-for-range)

(require "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../json-dsl.rkt"
         "with-document.rkt")

(define (hint->triple hint)
  (define position (hash-ref hint 'position))
  (list (hash-ref position 'line)
        (hash-ref position 'character)
        (hash-ref hint 'label)))

(define (inlay-hints-for-range lsp uri [start-line 0] [end-line 30])
  (lsp-result! lsp
               "textDocument/inlayHint"
               uri
               (json {#:range (->jsexpr (Range (Pos start-line 0) (Pos end-line 0)))})))
