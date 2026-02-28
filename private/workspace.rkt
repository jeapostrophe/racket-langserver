#lang racket/base
(require racket/set)

(define workspace-folders (mutable-set))
(define (add-workspace-folder! path)
  (set-add! workspace-folders path))
(define (remove-workspace-folder! path)
  (set-remove! workspace-folders path))
(define (current-workspace-folders)
  (set->stream workspace-folders))

(provide current-workspace-folders
         add-workspace-folder!
         remove-workspace-folder!)
