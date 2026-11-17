#lang racket/base

(module+ test
  (require rackunit
           "../../../common/interfaces.rkt"
           "../../../common/json-util.rkt"
           "../../json-dsl.rkt"
           "../with-document.rkt")

  (define uri "file:///test.rkt")

  (test-case "textDocument/completion returns non-empty labeled items after edit"
    (with-document uri "#lang racket/base\n"
      (lambda (lsp)
        ;; Completion requires an edit at the cursor line; moving cursor alone is insufficient.
        (notify lsp "textDocument/didChange"
                (json {#:textDocument (->jsexpr (DocIdentifier 1 uri))
                       #:contentChanges [(->jsexpr
                                           (ContentChangeEvent-Incremental
                                             (Range (Pos 0 17) (Pos 0 17))
                                             0
                                             "\n()"))]}))
        (client-wait-notification lsp)

        (define result
          (lsp-result! lsp
                       "textDocument/completion"
                       uri
                       (json {#:position (->jsexpr (Pos 1 1))
                              #:context {#:triggerCharacter "(" #:triggerKind 2}})))
        (define completion-list (jsexpr->CompletionList result))
        (define items (CompletionList-items completion-list))
        (check-false (null? items))
        (for ([item (in-list items)])
          (check-false (string=? "" (CompletionItem-label item))))))))
