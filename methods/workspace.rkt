#lang racket/base
(require json
         racket/gui
         "../store/docs.rkt"
         "../store/workspace.rkt"
         "../error-codes.rkt"
         "../json-util.rkt"
         "../path-util.rkt"
         "../interfaces.rkt"
         "../responses.rkt")

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#fileRename
(define-json-expander FileRename
  [oldUri string?]
  [newUri string?])

;;
;; Methods
;;;;;;;;;;;;

;; Will Rename Files request
;;
;; spec: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_willRenameFiles
;;
;; Response:
;;   result: WorkspaceEdit | null
;;   error: code and message set in case an exception happens during the workspace/willRenameFiles request.
(define (willRenameFiles id params)
  (match params
    ; export interface RenameFilesParams {
    ; 	/**
    ; 	 * An array of all files/folders renamed in this operation. When a folder
    ; 	 * is renamed, only the folder will be included, and not its children.
    ; 	 */
    ; 	files: FileRename[];
    ; }
    [(hash-table ['files renamed-files])
     (define files-in-workspace (find-files (λ (p) (path-has-extension? p #".rkt"))
                                            (uri->path (unbox rootUri))))
     (define changes (make-hasheq))
     (for ([file-path files-in-workspace])
       (match-define (doc doc-text doc-trace) (get-doc (path->uri file-path)))
       (for ([renamed-file renamed-files])
         (match-define (hash-table ['oldUri oldUri] ['newUri newUri]) renamed-file)
         (define range-to-update (hash-ref (send doc-trace get-requires-occurs)
                                           (path->string (find-relative-path (path-only file-path) (uri->path oldUri)))
                                           #f))
         (when range-to-update
           (match-define (cons start end) range-to-update)
           (hash-set! changes
                      (path->uri file-path)
                      ; TextEdit[]
                      (list (TextEdit #:range (start/end->Range doc-text start end)
                                      #:newText (string-append "\""
                                                               (path->string (find-relative-path (path-only file-path) (uri->path newUri)))
                                                               "\"")))))))
     (success-response id (hasheq
                           ; { [uri: DocumentUri]: TextEdit[]; }
                           'changes changes))]
    [_ (error-response id INVALID-PARAMS "workspace/willRenameFiles failed")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [willRenameFiles (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]))
