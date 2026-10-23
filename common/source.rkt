#lang racket/base

;; Original URI address plus a stable identity. File URI aliases share an
;; identity when their decoded paths normalize to the same complete path.
;; This module converts between them and never holds text or lifetime.
;; Tables key on `source-id`; `source-file-path` is absent for virtual
;; sources.
(require racket/contract
         "path-util.rkt")

(provide Source?
         Source-Id?
         (contract-out
           [uri->source (-> string? Source?)]
           [path->source (-> path? Source?)]
           [file-uri->identity-path (-> (and/c string? file-uri?) complete-path?)]
           [source-id (-> Source? Source-Id?)]
           [source-uri (-> Source? string?)]
           [source-file-path (-> Source? (or/c path? #f))]
           [source-syntax-source (-> Source? (or/c path? string?))]
           [source-location-uri (-> Source? (or/c path? string?) string?)]))

;; Constructors stay private. IDs support `equal?` and hashing. A descriptor
;; retains the original address and is not a table key.
(struct Source-Id (value) #:transparent)
(struct Source (uri id) #:transparent)

;; Identity path of a file URI. Protocol code uses this when a later API
;; takes a path. The original URI stays on the Source from `uri->source`.
(define (file-uri->identity-path uri)
  (normalize-source-path (uri->path uri)))

;; File identity is a complete lexically normalized path. Any other scheme
;; uses the complete URI, so query and fragment distinguish virtual sources.
(define (uri->source uri)
  (define address (string->immutable-string uri))
  (Source address
          (Source-Id (if (file-uri? address)
                         (file-uri->identity-path address)
                         address))))

;; Relative paths use current-directory; callers with another base must complete
;; them there before entry. Protocol callers retain their address with uri->source.
(define (path->source path)
  (define normalized (normalize-source-path path))
  (Source (string->immutable-string (normalized-path->uri normalized))
          (Source-Id normalized)))

(define source-id Source-id)
(define source-uri Source-uri)

;; Check Syntax `syntax-source`: a file path, or the whole URI when virtual.
(define (source-syntax-source source)
  (Source-Id-value (source-id source)))

(define (source-file-path source)
  (define value (source-syntax-source source))
  (and (path? value) value))

;; Keep this source's URI when `location` matches its syntax-source after file
;; path normalization, so aliases such as `file://localhost/...` survive.
;; Foreign file paths get standard file URIs; other values pass through.
(define (source-location-uri source location)
  (cond
    [(equal? (if (path? location) (normalize-source-path location) location)
             (source-syntax-source source))
     (source-uri source)]
    [(path? location) (path->uri location)]
    [else location]))
