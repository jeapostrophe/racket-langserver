#lang racket
(require "../client.rkt"
         chk
         json)

(module+ test
  (with-racket-lsp
      (λ (lsp)
        (define did-rename-notification
          (make-notification "workspace/didRenameFiles"
                             (hasheq 'files
                                     (list (hasheq 'oldUri "a.rkt" 'newUri "a1.rkt")))))
        (client-send lsp did-rename-notification)
        )))
