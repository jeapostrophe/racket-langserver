#lang racket/base

;; Open-document registry for one session. The table locates SafeDoc buffers;
;; source revisions and observed disk text stay in the source store.
;; server% installs this registry during dispatch. Direct helper callers use
;; the default table.

(require racket/contract
         "../common/path-util.rkt"
         "../doclib/doc.rkt"
         "safedoc.rkt"
         "source-controller.rkt")

;; A deferred response thread inherits the parameterization of the dispatch
;; that started it, so it must see this table rather than the default.
(define current-open-docs (make-parameter (make-hasheq)))

(define/contract lsp-get-doc
  (case->
    (-> string? SafeDoc?)
    (-> string? any/c any/c))
  (case-lambda
    [(uri)
     (hash-ref (current-open-docs) (string->symbol uri))]
    [(uri default)
     (hash-ref (current-open-docs) (string->symbol uri) default)]))

(define/contract (lsp-open-doc! uri text version)
  (-> string? string? exact-nonnegative-integer? SafeDoc?)
  (define source-id (uri->source-id uri))
  ;; One exact source path has one registered buffer. Retire the previous URI;
  ;; a delayed close using a different spelling cannot close its replacement.
  (lsp-for-each-open-doc
    (lambda (safe-doc)
      (define old-uri (with-read-doc safe-doc Doc-uri))
      (when (equal? source-id (uri->source-id old-uri))
        (lsp-close-doc! old-uri))))
  (define safe-doc (new-safedoc uri text version))
  (source-controller-open! (current-source-controller) source-id safe-doc)
  (hash-set! (current-open-docs) (string->symbol uri) safe-doc)
  safe-doc)

(define/contract (lsp-close-doc! uri)
  (-> string? void?)
  (define uri-sym (string->symbol uri))
  (define safe-doc (lsp-get-doc uri #f))
  (when safe-doc
    (source-controller-close! (current-source-controller) (uri->source-id uri) safe-doc))
  (hash-remove! (current-open-docs) uri-sym))

;; Callbacks may acquire a SafeDoc lock. Snapshot the registry so callback
;; mutations cannot invalidate hash iteration.
(define/contract (lsp-for-each-open-doc proc)
  (-> (-> SafeDoc? any/c) void?)
  (for ([safe-doc (in-list (hash-values (current-open-docs)))])
    (proc safe-doc)))

(provide current-open-docs
         lsp-get-doc
         lsp-open-doc!
         lsp-close-doc!
         lsp-for-each-open-doc)
