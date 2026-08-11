#lang racket/base

;; Keep filesystem paths as path? inside the program; URI strings stay at the
;; LSP / protocol edge. Converting to string in uri->path would mix two
;; representations for the same path. path? is type-safer and a little cheaper
;; for path ops.

(provide path->uri
         uri->path
         directory-contains?)

(require net/url
         racket/contract
         racket/list
         racket/path)

(define/contract (path->uri path)
  (-> path? string?)
  (url->string (path->url path)))

(define/contract (uri->path uri)
  (-> string? path?)
  (url->path (string->url uri)))

(define/contract (directory-contains? dir filepath)
  (-> path-string? path-string? boolean?)
  (define dir-parts (explode-path (simple-form-path dir)))
  (define file-parts (explode-path (simple-form-path filepath)))
  (and (>= (length file-parts) (length dir-parts))
       (equal? dir-parts (take file-parts (length dir-parts)))))
