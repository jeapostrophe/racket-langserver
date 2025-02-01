#lang racket
(require "../client.rkt"
         chk
         json)

(module+ main
  (with-racket-lsp "../../main.rkt"
    (λ (lsp)
      (define workspace-req
        (make-notification "workspace/didChangeWorkspaceFolders"
                          (hasheq 'event
                                  (hasheq 'added (list (hasheq 'uri "/tmp/project_a" 'name "projectA"))
                                          'removed (list)))))
      (client-send lsp workspace-req)
      ; (client-should-no-response lsp)
      )))
