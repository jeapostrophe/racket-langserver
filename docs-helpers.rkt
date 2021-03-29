#lang racket/base

(require scribble/blueboxes
         setup/xref
         racket/set
         racket/class
         racket/list
         setup/collects
         racket/string
         scribble/xref
         "msg-io.rkt"
         racket/format
         "responses.rkt")

(define the-bluebox-cache (make-blueboxes-cache #t))
(define pkg-cache (make-hash))

(define (find-containing-paren pos text)
  (define l (string-length text))
  (cond
    [(>= pos l) #f]
    [else
     (let loop ([i pos] [p 0])
       (cond
         [(< i 0) #f]
         [(or (char=? (string-ref text i) #\() (char=? (string-ref text i) #\[))
          (if (> p 0) (loop (- i 1) (- p 1)) i)]
         [(or (char=? (string-ref text i) #\)) (char=? (string-ref text i) #\]))
          (loop (- i 1) (+ p 1))]
         [else (loop (- i 1) p)]))]))

(define (id-to-tag id trace)
  ;; partial reimplementation of private method compute-tag+rng
  ;; in drracket/private/syncheck/blueboxes-gui.rkt
  (define xref (load-collections-xref))
  (define mps
    (for/list ([require-candidate (in-set (send trace get-requires))])
      (path->module-path require-candidate #:cache pkg-cache)))
  (for/or ([mp (in-list mps)])
    (define definition-tag (xref-binding->definition-tag xref (list mp (string->symbol id)) #f))
    (cond
          [definition-tag
            (define-values (path url-tag) (xref-tag->path+anchor xref definition-tag))
            (if path definition-tag #f)]
          [else #f])))

(define (get-docs-for-tag tag)
  (define strs (drop (fetch-blueboxes-strs tag #:blueboxes-cache the-bluebox-cache) 1))
  (define index (let loop ((strs strs) (i 0))
    (cond
      [(>= i (length strs)) #f]
      [(string-prefix? (list-ref strs i) "(") (loop strs (+ i 1))]
      [else i])))
  (cond [index (list (take strs index) (string-join (if index (drop strs index) strs) "\n"))]
        [else (list strs #f)]))

(provide find-containing-paren
         get-docs-for-tag
         id-to-tag)