#lang racket/base
(provide get-doc
         add-doc
         remove-doc)
(require racket/class
         framework
         "../path-util.rkt"
         "../check-syntax.rkt"
         "../interfaces.rkt")

(define open-docs (make-hasheq))

(define (get-doc uri) (hash-ref open-docs (string->symbol uri)))

(define (add-doc uri)
  (define path (uri->path uri))
  (define doc-text (new racket:text%))
  (send doc-text load-file path)
  (define trace (check-syntax path doc-text #f))
  (hash-set! open-docs (string->symbol uri) (doc doc-text trace)))

(define (remove-doc uri) (hash-remove! open-docs (string->symbol uri)))
