#lang racket/base
(require json
         racket/gui
         "error-codes.rkt"
         "json-util.rkt"
         "responses.rkt")

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#fileRename
(define-json-expander FileRename
  [oldUri string?]
  [newUri string?])

;;
;; Methods
;;;;;;;;;;;;

;; Will Rename Files request
;; spec: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_willRenameFiles
;; Response:
;;   result: WorkspaceEdit | null
;;   error: code and message set in case an exception happens during the workspace/willRenameFiles request.
(define (willRenameFiles id params)
  (match params
    [(hash-table ['files files])
     ; TODO: update all modules that imports the renamed file
     (success-response id null)]
    [_ (error-response id INVALID-PARAMS "workspace/willRenameFiles failed")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [willRenameFiles (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]))
