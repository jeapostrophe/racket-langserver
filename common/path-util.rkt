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
  (define base-url (path->url path))
  (define elements (explode-path path))
  (define-values (_base name directory?) (split-path path))
  ;; path->url simplifies dot segments. Keep those segments because paths are
  ;; exact source identities, including the key close must remove.
  (define root-segments
    (if (absolute-path? path)
        (drop-right (url-path (path->url (car elements))) 1)
        '()))
  (define segments
    (append root-segments
            (for/list ([element (in-list (if (absolute-path? path) (cdr elements) elements))])
              (path/param (if (symbol? element) element (path-element->string element)) '()))
            ;; A final same/up already marks a directory. A trailing slash
            ;; would append another same segment when the URI is decoded.
            (if (and directory? (path? name)) (list (path/param "" '())) '())))
  (url->string (struct-copy url base-url [path segments])))

(define/contract (uri->path uri)
  (-> string? path?)
  (url->path (string->url uri)))

(define/contract (directory-contains? dir filepath)
  (-> path-string? path-string? boolean?)
  (define dir-parts (explode-path (simple-form-path dir)))
  (define file-parts (explode-path (simple-form-path filepath)))
  (and (>= (length file-parts) (length dir-parts))
       (equal? dir-parts (take file-parts (length dir-parts)))))
