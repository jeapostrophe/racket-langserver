#lang racket

;; A hint goes when the text it was read from is rewritten, including the two
;; rewrites the document's length does not show: one that keeps the length, and
;; one that grows it at the end of the form it rewrote.

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
                    (list '(5 7 "x ")
                          '(5 9 "y ")
                          '(6 6 "w ")
                          '(6 8 "h ")))

      ;; An unfinished form: nothing expands from here on, so every hint from
      ;; now on is one that was read before this edit. It also pushes the rest
      ;; of the document down a line.
      (replace! 1 1 0 1 0 "(define\n")
      (check-equal? (hints)
                    (list '(6 7 "x ")
                          '(6 9 "y ")
                          '(7 6 "w ")
                          '(7 8 "h ")))

      ;; Rename a field of `point`, one character for one. The document is the
      ;; same length as before, so it neither expands nor contracts, but the
      ;; call no longer fills the fields the hints name.
      (replace! 2 3 15 3 16 "z")
      (check-equal? (hints)
                    (list '(7 6 "w ")
                          '(7 8 "h ")))

      ;; Rewrite the whole declaration of `size` with a longer one. The
      ;; characters it gains arrive at the end of the form, which is where
      ;; typing after the form would put them too.
      (replace! 3 4 0 4 19 "(struct size (a b c))")
      (check-equal? (hints) '()))))
