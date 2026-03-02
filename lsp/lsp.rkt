#lang racket/base

(require racket/contract
         "safedoc.rkt"
         "scheduler.rkt")

(define open-docs (make-hasheq))

(define/contract lsp-get-doc
  (case->
    (-> string? SafeDoc?)
    (-> string? any/c any/c))
  (case-lambda
    [(uri)
     (hash-ref open-docs (string->symbol uri))]
    [(uri default)
     (hash-ref open-docs (string->symbol uri) default)]))

(define/contract (lsp-open-doc! uri text version)
  (-> string? string? exact-nonnegative-integer? SafeDoc?)
  (lsp-close-doc! uri)
  (define safe-doc (new-safedoc uri text version))
  (hash-set! open-docs (string->symbol uri) safe-doc)
  safe-doc)

(define/contract (lsp-close-doc! uri)
  (-> string? void?)
  (define uri-sym (string->symbol uri))
  (define safe-doc (lsp-get-doc uri #f))
  (when safe-doc
    (define token (SafeDoc-token safe-doc))
    (scheduler-close-doc! token)
    (clear-old-queries/doc-close token))
  (hash-remove! open-docs uri-sym))

(provide lsp-get-doc
         lsp-open-doc!
         lsp-close-doc!)

