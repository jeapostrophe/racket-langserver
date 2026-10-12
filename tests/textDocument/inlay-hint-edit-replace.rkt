#lang racket

;; Nonempty inlay hints require successful analysis of the current revision.
;; Incomplete source returns empty hints; a repaired source recovers.
;; Edit-aware adjustment itself is covered by tests/lib/inlay-hint-edit-test.rkt.

(define uri "file:///test.rkt")

(define code
#<<END
#lang racket

(struct point (x y))
(struct size (w h))

(point 1 2)
(size 3 4)
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

      (define (replace! version start-line start-char end-line end-char text)
        (client-send lsp
                     (make-notification
                       "textDocument/didChange"
                       (hasheq 'textDocument
                               (hasheq 'uri uri 'version version)
                               'contentChanges
                               (list (hasheq 'range
                                             (hasheq 'start
                                                     (hasheq 'line start-line
                                                             'character start-char)
                                                     'end
                                                     (hasheq 'line end-line
                                                             'character end-char))
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
                    (list '(5 7 "x ")
                          '(5 9 "y ")
                          '(6 6 "w ")
                          '(6 8 "h ")))

      ;; Each incomplete revision must return empty hints independently.
      (replace! 1 1 0 1 0 "(define\n")
      (check-empty)

      ;; Rename a field of `point`, one character for one. The document is the
      ;; same length as before, so it neither expands nor contracts, but the
      ;; call no longer fills the fields the hints name.
      (replace! 2 3 15 3 16 "z")
      (check-empty)

      ;; Rewrite the whole declaration of `size` with a longer one. The
      ;; characters it gains arrive at the end of the form, which is where
      ;; typing after the form would put them too.
      (replace! 3 4 0 4 19 "(struct size (a b c))")
      (check-empty)

      (client-send lsp
                   (make-notification "textDocument/didChange"
                                      (hasheq 'textDocument (hasheq 'uri uri 'version 4)
                                              'contentChanges (list (hasheq 'text code)))))
      (client-wait-notification lsp)
      (check-equal? (hints)
                    (list '(5 7 "x ") '(5 9 "y ") '(6 6 "w ") '(6 8 "h "))))))
