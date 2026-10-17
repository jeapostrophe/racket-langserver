#lang racket/base

;; Open-document registry for one session. The table locates SafeDoc buffers
;; by URI. Source revisions, disk observation, and analysis belong to the
;; source controller.

(require racket/contract
         "../common/source.rkt"
         "../doclib/doc.rkt"
         "safedoc.rkt"
         "source-controller.rkt")

;; `server%` binds a session table in `process-message` and
;; `process-source-event`. Deferred response threads inherit that binding;
;; helpers without it share this default table.
(define current-open-docs (make-parameter (make-hasheq)))

;; Intern URIs as symbols so `hasheq` hits with `eq?`. Two equal URI strings
;; need not be `eq?`.
(define (open-doc-key uri)
  (string->symbol uri))

(define/contract lsp-get-doc
  (case->
    (-> string? SafeDoc?)
    (-> string? failure-result/c any/c))
  (case-lambda
    [(uri)
     (hash-ref (current-open-docs) (open-doc-key uri))]
    [(uri default)
     (hash-ref (current-open-docs) (open-doc-key uri) default)]))

(define/contract (lsp-open-doc! uri text version)
  (-> string? string? exact-nonnegative-integer? SafeDoc?)
  (define source (uri->source uri))
  ;; One exact source path has one registered buffer. Retire the previous URI
  ;; first so a delayed `didClose` of a different spelling cannot close this
  ;; replacement.
  (lsp-for-each-open-doc
    (lambda (safe-doc)
      (define old-uri (with-read-doc safe-doc Doc-uri))
      (when (equal? (source-id source)
                    (with-read-doc safe-doc (lambda (doc) (source-id (Doc-source doc)))))
        (lsp-close-doc! old-uri))))
  (define safe-doc (new-safedoc uri text version))
  (source-controller-open! (current-source-controller) source safe-doc)
  (hash-set! (current-open-docs) (open-doc-key uri) safe-doc)
  safe-doc)

(define/contract (lsp-close-doc! uri)
  (-> string? void?)
  (define uri-sym (open-doc-key uri))
  (define safe-doc (lsp-get-doc uri #f))
  (when safe-doc
    (source-controller-close! (current-source-controller) (uri->source uri) safe-doc))
  (hash-remove! (current-open-docs) uri-sym))

;; Chosen over `in-hash-values`: `proc` may close a document and invalidate
;; a live hash sequence.
(define/contract (lsp-for-each-open-doc proc)
  (-> (-> SafeDoc? any/c) void?)
  (for ([safe-doc (in-list (hash-values (current-open-docs)))])
    (proc safe-doc)))

(provide current-open-docs
         lsp-get-doc
         lsp-open-doc!
         lsp-close-doc!
         lsp-for-each-open-doc)
