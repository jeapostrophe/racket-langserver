#lang racket
(require "../client.rkt"
         chk
         json)

(module+ test
  (with-racket-lsp "../../main.rkt"
    (λ (lsp)
      (define noti
        (make-notification "workspace/didChangeWorkspaceFolders"
                          (hasheq 'event
                                  (hasheq 'added (list (hasheq 'uri "/tmp/project_a" 'name "projectA"))
                                          'removed (list)))))
      (client-send lsp noti)
      )))
