#lang racket/base
(require racket/set
         racket/file
         racket/path
         compiler/module-suffix
         "path-util.rkt")

(define workspace-folders (mutable-set))

(define (add-workspace-folder! path)
  (set-add! workspace-folders path))

(define (remove-workspace-folder! path)
  (set-remove! workspace-folders path))

(define (current-workspace-folders)
  (set->stream workspace-folders))

(define (workspace-contains? filepath)
  (for/or ([dir (current-workspace-folders)])
    (directory-contains? dir filepath)))

(provide current-workspace-folders
         add-workspace-folder!
         remove-workspace-folder!
         workspace-contains?)
