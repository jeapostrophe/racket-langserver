#lang racket/base
(provide get-resyntax-enabled
         set-resyntax-enabled!)
(require "server-request.rkt"
         "json-util.rkt")

(define-json-expander ConfigurationItem
  [scopeUri string?]
  [section string?])

(define (fetch-enable-resyntax)
  (send-request 0 "workspace/configuration"
    (hasheq 'items (list (ConfigurationItem #:scopeUri "racket-langserver" #:section "enable-resyntax")))
    (λ (result)
      (when (boolean? result)
        (set-resyntax-enabled! result)))))

(define resyntax-enabled? #t)

(define (get-resyntax-enabled)
  (fetch-enable-resyntax)
  resyntax-enabled?)

(define (set-resyntax-enabled! val)
  (set! resyntax-enabled? val))
