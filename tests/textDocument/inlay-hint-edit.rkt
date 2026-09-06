#lang racket

;; Hints are read from the last analysis, so what editing does to them matters:
;; they follow the text they name, and they go when that text changes, even
;; while the document does not expand at all.

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
        ;; the diagnostics of the failed analysis, so the next request is
        ;; answered by an edited document rather than a running one
        (client-wait-notification lsp))

      (define (hints)
        (define req
          (make-request lsp
                        "textDocument/inlayHint"
                        (hasheq 'textDocument
                                (hasheq 'uri uri)
                                'range
                                (hasheq 'start (hasheq 'line 0 'character 0)
                                        'end (hasheq 'line 30 'character 0)))))
        (client-send lsp req)
        (map hint->triple (hash-ref (client-wait-response req) 'result)))

      (check-equal? (hints)
                    (list '(4 7 "x ")
                          '(4 9 "y ")))

      ;; An unfinished form: nothing expands from here on, so every hint from
      ;; now on is one that was read before this edit.
      (insert! 1 1 0 "(define\n")
      (check-equal? (hints)
                    (list '(5 7 "x ")
                          '(5 9 "y ")))

      ;; A new argument in the call. Which field each argument fills is no
      ;; longer what the hints say, so the call has none until it expands again.
      (insert! 2 5 7 "0 ")
      (check-equal? (hints) '()))))
