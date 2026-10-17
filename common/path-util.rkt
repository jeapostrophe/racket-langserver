#lang racket/base

;; File URI encoding of exact path segments, URI filename suffixes, and
;; folder membership.

(require net/url
         racket/contract
         racket/list
         racket/path
         (only-in racket/string non-empty-string?))

(provide path->uri
         uri->path
         directory-contains?
         file-uri?
         uri->suffix)

;; Encode a path as a file URI without collapsing `.` or `..`.
;; `path->url` then `url->string` simplify those segments, so decoding
;; would yield a different path.
(define/contract (path->uri path)
  (-> path? string?)
  (define base-url (path->url path))
  (define elements (explode-path path))
  (define-values (_base name directory?) (split-path path))
  (define absolute? (absolute-path? path))
  (define root-segments
    (if absolute?
        ;; The root URL ends with an empty directory segment. Drop it so the
        ;; remaining path replaces that slot rather than appending after it.
        (drop-right (url-path (path->url (car elements))) 1)
        '()))
  (define rest-segments
    (for/list ([element (in-list (if absolute? (cdr elements) elements))])
      (path/param (if (symbol? element) element (path-element->string element)) '())))
  (define segments
    (append root-segments
            rest-segments
            ;; A trailing slash is only for a real directory name. A final
            ;; `'same` or `'up` already names a directory; an extra empty
            ;; segment would decode as another `'same`.
            (if (and directory? (path? name)) (list (path/param "" '())) '())))
  (url->string (struct-copy url base-url [path segments])))

(define/contract (uri->path uri)
  (-> string? path?)
  (url->path (string->url uri)))

;; Folder membership, not source identity. Containment may collapse `.` and
;; `..`; source identity must keep those segments.
(define/contract (directory-contains? dir path)
  (-> path-string? path-string? boolean?)
  (list-prefix? (explode-path (simple-form-path dir))
                (explode-path (simple-form-path path))))

;; (-> string? boolean?)
(define (file-uri? uri)
  (equal? (url-scheme (string->url uri)) "file"))

;; Last URI path segment, not a filesystem path, so virtual URIs still match.
;; Query and fragment are not the filename.
;; (-> string? (or/c string? #f))
(define (uri->suffix uri)
  (define segments (url-path (string->url uri)))
  (define name (and (pair? segments) (path/param-path (last segments))))
  (define extension (and (non-empty-string? name)
                         (path-get-extension (string->path name))))
  (and extension (bytes->string/utf-8 (subbytes extension 1))))
