#lang racket/base
(require racket/set
         racket/file
         racket/path
         compiler/module-suffix)

(define workspace-folders (mutable-set))
(define (add-workspace-folder! path)
  (set-add! workspace-folders path))
(define (remove-workspace-folder! path)
  (set-remove! workspace-folders path))
(define (current-workspace-folders)
  (set->stream workspace-folders))

(define (workspace-files)
  (define files (mutable-set))

  (for* ([dir (current-workspace-folders)]
         [path (find-files
                 (lambda (path) (for/or ([suffix (get-module-suffixes)]) (path-has-extension? path suffix)))
                 dir)])
    (set-add! files (path->complete-path path)))

  files)

(provide current-workspace-folders
         add-workspace-folder!
         remove-workspace-folder!
         workspace-files)
