#lang racket

(module+ test
  (require "../client.rkt")
  (with-racket-lsp
    (λ (lsp)
      (define noti
        (make-notification "workspace/didChangeWorkspaceFolders"
                           (hasheq 'event
                                   (hasheq 'added (list (hasheq 'uri "file:///tmp/project_a" 'name "projectA"))
                                           'removed (list)))))
      (client-send lsp noti))))

