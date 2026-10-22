#lang racket/base

;; Lexically normalized file paths, standard file URI encoding, suffixes, and
;; folder membership.

(require net/url
         racket/contract
         racket/list
         racket/path
         (only-in racket/string non-empty-string?))

(provide normalize-source-path
         normalized-path->uri
         path->uri
         uri->path
         directory-contains?
         file-uri?
         uri->suffix)

;; Complete relative paths at the caller's current directory and normalize only
;; lexically: identity never resolves symlinks or folds case.
(define/contract (normalize-source-path path)
  (-> path? complete-path?)
  (simplify-path (path->complete-path path) #f))

;; Encode a path that `normalize-source-path` has already produced.
;; Callers that still have a relative or dot-segment path use `path->uri`.
(define/contract (normalized-path->uri path)
  (-> complete-path? string?)
  (url->string (path->url path)))

(define/contract (path->uri path)
  (-> path? string?)
  (normalized-path->uri (normalize-source-path path)))

;; Raw URI decoding also serves virtual dependency contexts. File identity
;; normalization is `file-uri->identity-path`, which `uri->source` uses.
;; This scheme-agnostic decoder does not normalize.
(define/contract (uri->path uri)
  (-> string? path?)
  (url->path (string->url uri)))

;; Prefix of the paths as given. Callers decode a URI and normalize before
;; entry; `simple-form-path` would resolve `..` through a symlink.
(define/contract (directory-contains? dir path)
  (-> path-string? path-string? boolean?)
  (list-prefix? (explode-path dir) (explode-path path)))

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
