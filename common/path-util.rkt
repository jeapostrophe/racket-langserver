#lang racket/base

(provide path->uri
         uri->path
         directory-contains?)

(require net/url
         racket/string
         racket/list
         racket/path)

(define path->uri (compose url->string path->url))

(define (uri->path uri)
  (cond [(string-prefix? uri "file:") (path->string (url->path (string->url uri)))]
        [else (uri->path (regexp-replace #rx".*?:" uri "file:"))]))

(define (directory-contains? dir filepath)
  (define dir-parts (explode-path (simple-form-path dir)))
  (define file-parts (explode-path (simple-form-path filepath)))
  (and (>= (length file-parts) (length dir-parts))
       (equal? dir-parts (take file-parts (length dir-parts)))))
