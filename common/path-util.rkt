#lang racket/base

(provide path->uri
         uri->path
         directory-contains?)

(require net/url
         racket/list
         racket/path)

(define path->uri (compose url->string path->url))

(define uri->path (compose path->string url->path string->url))

(define (directory-contains? dir filepath)
  (define dir-parts (explode-path (simple-form-path dir)))
  (define file-parts (explode-path (simple-form-path filepath)))
  (and (>= (length file-parts) (length dir-parts))
       (equal? dir-parts (take file-parts (length dir-parts)))))
