#lang racket/base

(require scribble/blueboxes
         racket/string)

(define the-bluebox-cache (make-blueboxes-cache #f))

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

(define (get-docs-for-tag tag)
  (define strs (fetch-blueboxes-strs tag #:blueboxes-cache the-bluebox-cache))
  (string-join (list-tail strs 1) "\n"))

(provide find-containing-paren
         get-docs-for-tag)