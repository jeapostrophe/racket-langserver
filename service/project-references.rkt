#lang racket/base

(require "interface.rkt"
         "../struct.rkt"
         racket/class
         racket/set
         racket/file
         racket/path
         drracket/check-syntax)

(provide project-references%
         find-nonlocal-references)

(define (project-files)
  (define files (mutable-set))

  ;; FIXME: use loaded sufficies
  ;; FIXME: use workspace folders
  (for ([path (find-files (lambda (path) (path-has-extension? path #".rkt")) (current-directory))])
    (set-add! files (path->complete-path path)))

  files)

(define projectwise-references (make-hash))
(define (find-nonlocal-references uri symbol)
  (hash-ref projectwise-references (list uri symbol)))

(define project-references%
  (class base-service%
    (init-field src)
    (super-new)

    (define/override (syncheck:add-jump-to-definition _src-obj start end id filename _submods)
      ;; NOTE start <= end. In some situations, it may be that start = end.
      (define end- (if (= start end) (add1 end) end))
      (when (set-member? (project-files) filename)
        (println "Reference to other file!")
        (hash-update! projectwise-references
          (list filename id)
          (lambda (refs) (set-add refs (list src start end-)))
          (set))))))
