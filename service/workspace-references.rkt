#lang racket/base

(require "interface.rkt"
         "../private/workspace.rkt"
         "../path-util.rkt"
         "../interfaces.rkt"
         racket/class
         racket/set
         racket/file
         racket/path
         racket/list
         drracket/check-syntax
         compiler/module-suffix)

(provide workspace-references%
         find-workspace-bindings
         reset-workspace-references!)

(define (project-files)
  (define files (mutable-set))

  (for* ([dir (current-workspace-folders)]
         [path (find-files
                 (lambda (path) (for/or ([suffix (get-module-suffixes)]) (path-has-extension? path suffix)))
                 dir)])
    (set-add! files (path->complete-path path)))

  files)

(define workspace-references (make-hash))
(define (reset-workspace-references!)
  (hash-clear! workspace-references))
(define (find-workspace-bindings uri symbol)
  (define r
    (hash-ref workspace-references (list (uri->path uri) symbol)
              #f))
  (if r
      (set->list r)
      (list)))

(define workspace-references%
  (class base-service%
    (init-field src doc-text)
    (super-new)

    ; Check https://docs.racket-lang.org/drracket-tools/Accessing_Check_Syntax_Programmatically.html#%28meth._%28%28%28lib._drracket%2Fcheck-syntax..rkt%29._syncheck-annotations~3c~25~3e%29._syncheck~3aadd-jump-to-definition%2Fphase-level%2Bspace%29%29
    ;
    ; Basically here means, in file `src` absolute position from `start` to `end`
    ; references to a definition with name `id` in the file `filepath`.
    (define/override (syncheck:add-jump-to-definition _src-obj start end id filepath _submods)
      ;; NOTE start <= end. In some situations, it may be that start = end.
      (define end- (if (= start end) (add1 end) end))
      (when (set-member? (project-files) filepath)
        (define (abs->Pos p)
          (define lc (send doc-text pos->line/char p))
          (Pos #:line (first lc) #:char (second lc)))
        (hash-update! workspace-references
                      (list (path->string filepath) id)
                      (lambda (refs)
                        (set-add refs
                                 (Location #:uri (path->uri src)
                                           #:range (Range #:start (abs->Pos start) #:end (abs->Pos end-)))))
                      (set))))))
