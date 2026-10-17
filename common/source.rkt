#lang racket/base

;; Original URI address plus a stable identity. File URI aliases share an
;; identity when they decode to the same exact path. This module converts
;; between them and never holds text or lifetime. Tables key on `source-id`;
;; `source-file-path` is absent for virtual sources.
(require racket/contract
         "path-util.rkt")

(provide Source?
         Source-Id?
         (contract-out
           [uri->source (-> string? Source?)]
           [path->source (-> path? Source?)]
           [source-id (-> Source? Source-Id?)]
           [source-uri (-> Source? string?)]
           [source-file-path (-> Source? (or/c path? #f))]
           [source-syntax-source (-> Source? (or/c path? string?))]
           [source-location-uri (-> Source? (or/c path? string?) string?)]))

;; Constructors stay private. IDs support `equal?` and hashing. A descriptor
;; retains the original address and is not a table key.
(struct Source-Id (value) #:transparent)
(struct Source (uri id) #:transparent)

;; File identity is the decoded exact path. Any other scheme uses the complete
;; URI, so query and fragment distinguish virtual sources.
(define (uri->source uri)
  (define address (string->immutable-string uri))
  (Source address (Source-Id (if (file-uri? address) (uri->path address) address))))

;; Build a URI from a filesystem path. Completing the path for the URI must
;; not rewrite the caller's ID, including an exact relative path. Callers that
;; already have a protocol URI should use `uri->source`.
(define (path->source path)
  (Source (string->immutable-string (path->uri (path->complete-path path))) (Source-Id path)))

(define source-id Source-id)
(define source-uri Source-uri)

;; Check Syntax `syntax-source`: a file path, or the whole URI when virtual.
(define (source-syntax-source source)
  (Source-Id-value (source-id source)))

(define (source-file-path source)
  (define value (source-syntax-source source))
  (and (path? value) value))

;; Keep this source's URI when `location` is its syntax-source, so aliases
;; such as `file://localhost/...` survive. Foreign file paths get a URI built
;; from the path; other values pass through.
(define (source-location-uri source location)
  (cond
    [(equal? location (source-syntax-source source)) (source-uri source)]
    [(path? location) (path->uri (path->complete-path location))]
    [else location]))
