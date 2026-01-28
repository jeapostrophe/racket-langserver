#lang racket

(require "../with-document.rkt"
         "../../../json-util.rkt"
         chk
         json)

(define uri "file:///test.rkt")

(define code
  #<<END
#lang racket/base
END
  )

(module+ test
  (with-document uri code
    (λ (lsp)

      ;; completion requires a document change.
      ;; only move cursor to that position is not enough.
      (define didchange-req (read-json (open-input-file "change-req.json")))
      (client-send lsp didchange-req)
      (client-wait-response lsp)

      (define comp-req (read-json (open-input-file "comp-req.json")))
      (client-send lsp comp-req)
      ;; we only verify the returned completion item list
      ;; meet some conditions
      (let ([resp (client-wait-response lsp)])
        (chk (jsexpr-has-key? resp '(result)))
        (define completion-list (jsexpr-ref resp '(result)))
        (chk (jsexpr-has-key? completion-list '(isIncomplete)))
        (chk (jsexpr-has-key? completion-list '(items)))
        (define result (jsexpr-ref completion-list '(items)))
        (chk (list? result))
        (chk (for/and ([item result])
               (jsexpr-has-key? item '(label))))
        (chk (for/and ([item result])
               (string? (jsexpr-ref item '(label)))))
        (chk (for/and ([item result])
               (not (string=? "" (jsexpr-ref item '(label))))))))))
