#lang racket
(require "../client.rkt"
         chk
         json)

(module+ test
  (with-racket-lsp "../../main.rkt"
    (λ (lsp)
      (define noti
        (make-notification "workspace/didChangeWatchedFiles"
                          (hasheq 'changes
                                  (list (hasheq 'uri "file:///tmp/test.rkt" 'type 1)   ; created
                                        (hasheq 'uri "file:///tmp/other.rkt" 'type 2)  ; changed
                                        (hasheq 'uri "file:///tmp/old.rkt" 'type 3))))) ; deleted
      (client-send lsp noti)
      )))
