#lang racket

(provide path->uri
         uri->path)

(require net/url)

(define path->uri (compose url->string path->url))

(define (uri->path uri)
  (cond [(string-prefix? uri "file:") (path->string (url->path (string->url uri)))]
        [else (uri->path (regexp-replace #rx".*?:" uri "file:"))]))

