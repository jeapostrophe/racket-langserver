#lang racket/base

(require net/url
         racket/contract
         "interfaces.rkt"
         (only-in "json-util.rkt" Nothing?))

(provide get-resyntax-enabled
         set-resyntax-enabled!
         default-resyntax-enabled
         (struct-out Formatting-Settings)
         default-formatting-settings
         current-formatting-settings
         set-formatting-settings!
         current-document-exclusions
         initialization-options->document-exclusions
         ignored-document-uri?)

(define default-resyntax-enabled #t)

(define resyntax-enabled? default-resyntax-enabled)

(define (get-resyntax-enabled)
  resyntax-enabled?)

(define (set-resyntax-enabled! val)
  (set! resyntax-enabled? val))

;; Document formatting may reflow a complete buffer, while indentation
;; formatting must remain safe for ranges and on-type requests. Keep the two
;; selections distinct instead of silently substituting one for the other.
(struct/contract Formatting-Settings
  ([document-formatter (or/c 'fixw 'drracket 'fmt)]
   [indentation-formatter (or/c 'fixw 'drracket)]
   [fmt-settings Fmt-Settings?])
  #:transparent)

(define default-formatting-settings
  (Formatting-Settings 'fixw 'fixw (jsexpr->Fmt-Settings (hasheq))))

;; Process-wide, like `resyntax-enabled?`.
(define current-formatting-settings default-formatting-settings)

(define/contract (set-formatting-settings! settings)
  (-> Formatting-Settings? void?)
  (set! current-formatting-settings settings))

;; Each server captures this at initialization. Ordinary configuration updates
;; cannot change the routing of already-open documents.
(define current-document-exclusions
  (make-parameter (jsexpr->Document-Exclusions (hasheq))))

(define (initialization-options->document-exclusions options)
  (if (Document-Exclusions-js? options)
      (jsexpr->Document-Exclusions options)
      (jsexpr->Document-Exclusions (hasheq))))

(define (ignored-document-uri? uri)
  (define exclusions (current-document-exclusions))
  (define uris (Document-Exclusions-ignored-uris exclusions))
  (define schemes (Document-Exclusions-ignored-uri-schemes exclusions))
  (or (and (not (Nothing? uris)) (member uri uris) #t)
      (and (not (Nothing? schemes))
           (let ([scheme (url-scheme (string->url uri))])
             (and scheme
                  (for/or ([ignored (in-list schemes)])
                    (string-ci=? scheme ignored)))))))
