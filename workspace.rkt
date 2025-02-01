#lang racket
(provide didChangeWorkspaceFolders)
(require "json-util.rkt")

(define-json-expander WorkspaceFolder
  [uri string?]
  [name string?])
(define-json-expander WorkspaceFoldersChangeEvent
  [added (listof hash?)]
  [removed (listof hash?)])

(define workspace-folders (mutable-set))

(define (didChangeWorkspaceFolders params)
  (match-define (hash-table ['event (WorkspaceFoldersChangeEvent #:added added #:removed removed)]) params)
  (for ([f added])
    (match-define (WorkspaceFolder #:uri uri #:name _) f)
    (set-add! workspace-folders uri))
  (for ([f removed])
    (match-define (WorkspaceFolder #:uri uri #:name _) f)
    (set-remove! workspace-folders uri)))
