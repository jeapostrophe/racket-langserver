#lang racket/base

;; Filesystem locations stay as paths. Source descriptors retain the original
;; protocol URI independently from the backend location.

(provide path->uri
         uri->path
         directory-contains?
         file-uri?
         uri->source-id
         uri->suffix
         (struct-out File-Source)
         (struct-out Virtual-Source)
         (struct-out Virtual-Id)
         source?
         source-id?
         make-source
         source-id
         source-uri
         source-backend-path
         source-location->uri)

(require net/url
         racket/contract
         racket/list
         racket/path
         racket/match)

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

;; File identities retain their exact path spelling. Virtual identities retain
;; the complete URI; virtual backend paths never identify documents in server state.
(struct/contract File-Source ([uri string?] [path path?]) #:transparent)
(struct/contract Virtual-Id ([uri string?]) #:transparent)
(struct/contract Virtual-Source ([uri string?] [path complete-path?]) #:transparent)

(define (source? value)
  (or (File-Source? value) (Virtual-Source? value)))

(define (source-id? value)
  (or (path? value) (Virtual-Id? value)))

(define (file-uri? uri)
  (equal? (url-scheme (string->url uri)) "file"))

(define (uri->source-id uri)
  (if (file-uri? uri) (uri->path uri) (Virtual-Id uri)))

(define (make-source uri)
  (if (file-uri? uri)
      (File-Source uri (uri->path uri))
      (Virtual-Source
        uri
        (build-path (find-system-path 'temp-dir)
                    (format "racket-langserver-~a-~a" (current-inexact-milliseconds) (gensym))
                    "buffer.rkt"))))

(define (source-id source)
  (match source
    [(File-Source _ path) path]
    [(Virtual-Source uri _) (Virtual-Id uri)]))

(define (source-uri source)
  (match source
    [(File-Source uri _) uri]
    [(Virtual-Source uri _) uri]))

(define (source-backend-path source)
  (match source
    [(File-Source _ path) path]
    [(Virtual-Source _ path) path]))

;; Use this at result construction, where a backend location crosses into LSP.
(define (source-location->uri source path)
  (if (equal? path (source-backend-path source))
      (source-uri source)
      (path->uri path)))

(define (uri->suffix uri)
  (define segments (and uri (url-path (string->url uri))))
  (define extension
    (and segments (pair? segments)
         (let ([name (path/param-path (last segments))])
           (and (string? name) (not (equal? name ""))
                (path-get-extension (string->path name))))))
  (and extension (bytes->string/utf-8 (subbytes extension 1))))
