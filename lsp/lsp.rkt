#lang racket/base

(require racket/contract
         "../common/path-util.rkt"
         "../workspace/current.rkt"
         "../workspace/state.rkt"
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
    (define invalidate? (safedoc-close! safe-doc))
    (hash-remove! open-docs uri-sym)
    (when invalidate?
      (lsp-invalidate-path! (uri->path uri)))
    (clear-old-queries/doc-close token)))

;; Open buffers remain authoritative. Defer cache invalidation until the last
;; open URI for this decoded path closes, including percent-encoded aliases.
(define/contract (lsp-invalidate-path! path)
  (-> path? void?)
  (define open? #f)
  (for ([(uri safe-doc) (in-hash open-docs)]
        #:when (equal? path (uri->path (symbol->string uri))))
    (when (safedoc-disk-changed! safe-doc path)
      (set! open? #t)))
  (unless open?
    (workspace-remove-path! current-workspace path)))

;; Callbacks may acquire a SafeDoc lock. Snapshot the registry so callback
;; mutations cannot invalidate hash iteration.
(define/contract (lsp-for-each-open-doc proc)
  (-> (-> SafeDoc? any/c) void?)
  (for ([safe-doc (in-list (hash-values open-docs))])
    (proc safe-doc)))

(provide lsp-get-doc
         lsp-open-doc!
         lsp-close-doc!
         lsp-invalidate-path!
         lsp-for-each-open-doc)
