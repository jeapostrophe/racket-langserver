#lang racket/base

;; Failed expansion preserves adjusted hints whose source groups survived edits.
;; Edit-aware adjustment itself is covered by tests/lib/inlay-hint-edit-test.rkt.
(module+ test
  (require rackunit
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../json-dsl.rkt"
           "inlay-hint-support.rkt"
           "with-document.rkt")

  (define uri "file:///test.rkt")
  (define code "#lang racket\n\n(struct point (x y))\n\n(point 1 2)\n")

  (test-case "inlay hints track shifts across failing edits and invalidate disturbed groups"
    (with-document uri code
      (lambda (lsp)
        (define (replace! version start-line start-char end-line end-char text)
          (notify lsp "textDocument/didChange"
                  (json {#:textDocument (->jsexpr (DocIdentifier version uri))
                         #:contentChanges [(->jsexpr
                                             (ContentChangeEvent-Incremental
                                               (Range (Pos start-line start-char)
                                                      (Pos end-line end-char))
                                               omitted
                                               text))]}))
          (client-wait-notification lsp))

        (define (hints)
          (map hint->triple (inlay-hints-for-range lsp uri)))

        (check-equal? (hints) '((4 7 "x ") (4 9 "y ")))

        ;; An unrelated incomplete edit moves the surviving hints.
        (replace! 1 1 0 1 0 "(define\n")
        (check-equal? (hints) '((5 7 "x ") (5 9 "y ")))

        ;; Editing a constructor call invalidates that call's hint group.
        (replace! 2 5 7 5 8 "100")
        (check-equal? (inlay-hints-for-range lsp uri) '())

        (change! lsp uri code 3)
        (client-wait-notification lsp)
        (check-equal? (hints) '((4 7 "x ") (4 9 "y ")))))))
