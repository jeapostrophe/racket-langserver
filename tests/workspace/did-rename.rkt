#lang racket

(module+ test
  (require "../client.rkt")
  (with-racket-lsp
    (λ (lsp)
      (define did-rename-notification
        (make-notification "workspace/didRenameFiles"
                           (hasheq 'files
                                   (list (hasheq 'oldUri "a.rkt" 'newUri "a1.rkt")))))
      (client-send lsp did-rename-notification))))

