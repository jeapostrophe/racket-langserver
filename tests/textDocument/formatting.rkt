#lang racket

(require "with-document.rkt"
         chk)

(define uri "file:///test.rkt")

(define code
  #<<END
#lang racket/base

(define (bob)
  (+ 1 2))
END
  )

(module+ test
  (with-document "../../main.rkt" uri code
    (λ (lsp)
      ;; Insert a new line with indentation after line 2
      (let ([notif (make-notification
                    "textDocument/didChange"
                    (hasheq 'textDocument
                            (hasheq 'uri uri)
                            'contentChanges
                            (list
                             (hasheq 'range
                                     (hasheq 'start
                                             (hasheq 'line 2
                                                     'character 13)
                                             'end
                                             (hasheq 'line 2
                                                     'character 13))
                                     'rangeLength 0
                                     'text "\n"))))])
        (client-send lsp notif)
        (client-wait-response lsp))

      ;; Format on type for pre-indented new line 3
      (let* ([req (make-request lsp
                                "textDocument/onTypeFormatting"
                                (hasheq 'textDocument
                                        (hasheq 'uri uri)
                                        'position
                                        (hasheq 'line 3
                                                'character 0)
                                        'ch "\n"))]
             [res (make-response req
                                 (list
                                  (hasheq 'range
                                          (hasheq 'start
                                                  (hasheq 'line 3
                                                          'character 0)
                                                  'end
                                                  (hasheq 'line 3
                                                          'character 0))
                                          'newText "")
                                  (hasheq 'range
                                          (hasheq 'start
                                                  (hasheq 'line 3
                                                          'character 0)
                                                  'end
                                                  (hasheq 'line 3
                                                          'character 0))
                                          'newText "  ")))])
        (client-send lsp req)
        (chk #:= (client-wait-response lsp) res)))))
