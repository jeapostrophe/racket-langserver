#lang racket

;; Nonempty inlay hints require successful analysis of the current revision.
;; Incomplete source returns empty hints; a repaired source recovers.
;; Edit-aware adjustment itself is covered by tests/lib/inlay-hint-edit-test.rkt.

(define uri "file:///test.rkt")

(define code
#<<END
#lang racket

(struct point (x y))

(point 1 2)
END
  )

(module+ test
  (require rackunit
           "with-document.rkt")

  (define (hint->triple hint)
    (define position (hash-ref hint 'position))
    (list (hash-ref position 'line)
          (hash-ref position 'character)
          (hash-ref hint 'label)))

  (with-document uri code
    (λ (lsp)

      (define (insert! version line character text)
        (client-send lsp
                     (make-notification
                       "textDocument/didChange"
                       (hasheq 'textDocument
                               (hasheq 'uri uri 'version version)
                               'contentChanges
                               (list (hasheq 'range
                                             (hasheq 'start
                                                     (hasheq 'line line
                                                             'character character)
                                                     'end
                                                     (hasheq 'line line
                                                             'character character))
                                             'rangeLength 0
                                             'text text)))))
        ;; Await the current failure before querying its settled outcome.
        (client-wait-notification lsp))

      (define (hints-response)
        (define req
          (make-request lsp
                        "textDocument/inlayHint"
                        (hasheq 'textDocument
                                (hasheq 'uri uri)
                                'range
                                (hasheq 'start (hasheq 'line 0 'character 0)
                                        'end (hasheq 'line 30 'character 0)))))
        (client-send lsp req)
        (client-wait-response req))

      (define (hints)
        (map hint->triple (hash-ref (hints-response) 'result)))

      (define (check-empty)
        (check-equal? (hash-ref (hints-response) 'result) '()))

      (check-equal? (hints)
                    (list '(4 7 "x ")
                          '(4 9 "y ")))

      ;; The incomplete source cannot supply exact-analysis hints.
      (insert! 1 1 0 "(define\n")
      (check-empty)

      ;; Further edits must not revive the prior successful result.
      (insert! 2 5 7 "0 ")
      (check-empty)

      (client-send lsp
                   (make-notification "textDocument/didChange"
                                      (hasheq 'textDocument (hasheq 'uri uri 'version 4)
                                              'contentChanges (list (hasheq 'text code)))))
      (client-wait-notification lsp)
      (check-equal? (hints)
                    (list '(4 7 "x ") '(4 9 "y "))))))
