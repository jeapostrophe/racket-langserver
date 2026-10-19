#lang racket/base

;; textDocument notification and request handlers. An edit records the new
;; source revision inside the SafeDoc write lock before Check Syntax runs.
(require json
         racket/match
         racket/list
         racket/contract
         "../common/interfaces.rkt"
         "responses.rkt"
         "safedoc.rkt"
         "../common/settings.rkt"
         "../doclib/doc.rkt"
         (only-in "../doclib/formatting.rkt" exn:fail:fmt?)
         "../workspace/current.rkt"
         "compose/references.rkt"
         "compose/definition.rkt"
         "compose/hover.rkt"
         "semantic-token-lsp.rkt"
         "analysis-query.rkt"
         "source-controller.rkt"
         (submod "source-controller.rkt" owner)
         "../common/source.rkt"
         "lsp.rkt"
         "workspace.rkt")

(define client-capability-hierarchical-document-symbol? (make-parameter #f))

;;
;; Methods
;;;;;;;;;;;;

(define (did-open! request-client params)
  (match-define (hash-table ['textDocument (^DocItem #:uri uri #:version version #:text text)]) params)
  (fetch-configuration request-client uri)
  (lsp-open-doc! uri text version)
  (void))

(define (did-close! params)
  (match-define (hash-table ['textDocument (DocItem-js #:uri uri)]) params)
  (lsp-close-doc! uri))

(define (did-change! params)
  (match-define (hash-table ['textDocument (^DocIdentifier #:version version #:uri uri)]
                            ['contentChanges content-changes]) params)
  (define safe-doc (lsp-get-doc uri))
  (define content-changes*
    (cond [(eq? (json-null) content-changes) empty]
          [(list? content-changes) content-changes]
          [else (list content-changes)]))

  (source-controller-edit! (current-source-controller) safe-doc
                           (λ (doc)
                             (for ([change (in-list content-changes*)])
                               (match change
                                 [(^ContentChangeEvent-Incremental #:range range #:text text)
                                  (doc-apply-edit! doc range text)]
                                 [(^ContentChangeEvent-Full #:text text)
                                  (doc-reset! doc text)]))

                             (doc-update-version! doc version))))

(define (success-from-doc id uri proc)
  (success/enc id (with-read-doc (lsp-get-doc uri) proc)))

;; Hover request
(define (hover id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ['position (as-Pos pos)])
     (define safe-doc (lsp-get-doc uri))
     (hover-query safe-doc id pos)]
    [_
     (error-response id ErrorCode-InvalidParams "textDocument/hover failed")]))

;; Code Action request
(define (code-action id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ['range (as-Range range)]
                 ; TODO: _ctx has three fields
                 ; 1. `diagnostics`
                 ; 2. `only: CodeActionKind[]` server should use this to filter out client-unwanted code action
                 ; 3. `triggerKind?: CodeActionTriggerKind` the reason why code action were requested
                 ['context _ctx])
     (success-from-doc id uri (λ (doc) (doc-code-action doc range)))]
    [_ (error-response id ErrorCode-InvalidParams "textDocument/codeAction failed")]))

;; Signature Help request
(define (signatureHelp id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ['position (as-Pos pos)])
     (success-from-doc id uri (λ (doc) (doc-signature-help doc pos)))]
    [_
     (error-response id ErrorCode-InvalidParams "textDocument/signatureHelp failed")]))

;; Completion Request
(define (completion id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ['position (as-Pos pos)])
     (success-from-doc id uri (λ (doc) (doc-completion doc pos)))]
    [_
     (error-response id ErrorCode-InvalidParams "textDocument/completion failed")]))

;; Definition request

(define (definition id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ['position (as-Pos pos)])
     (define safe-doc (lsp-get-doc uri))
     (definition-query safe-doc id uri pos)]
    [_
     (error-response id ErrorCode-InvalidParams "textDocument/definition failed")]))

;; Reference request
(define (references id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ['position (as-Pos pos)]
                 ['context (hash-table ['includeDeclaration include-decl?])])
     (define safe-doc (lsp-get-doc uri))
     (define document-result
       (with-read-doc safe-doc
         (λ (doc) (doc-references doc uri pos include-decl?))))
     (define result
       (and document-result
            (reference-sources->locations
              (merge-reference-sources
                current-workspace
                document-result
                include-decl?))))
     (success/enc id result)]
    [_
     (error-response id ErrorCode-InvalidParams "textDocument/references failed")]))

;; Document Highlight request
(define (document-highlight id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ['position (as-Pos pos)])
     (success-from-doc id uri (λ (doc) (doc-highlights doc pos)))]
    [_
     (error-response id ErrorCode-InvalidParams "textDocument/documentHighlight failed")]))

;; Rename request
(define (_rename id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ['position (as-Pos pos)]
                 ['newName new-name])
     (success-from-doc id uri (λ (doc) (doc-rename doc uri pos new-name)))]
    [_
     (error-response id ErrorCode-InvalidParams "textDocument/rename failed")]))

;; Prepare rename
(define (prepareRename id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ['position (as-Pos pos)])
     (success-from-doc id uri (λ (doc) (doc-prepare-rename doc pos)))]
    [_
     (error-response id ErrorCode-InvalidParams "textDocument/prepareRename failed")]))

;; Document Symbol request
(define (document-symbol id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)])
     (success-from-doc
       id uri
       (λ (doc)
         (cond
           [(client-capability-hierarchical-document-symbol?)
            (doc-symbols-hierarchical doc)]
           [else (doc-symbols doc uri)])))]
    [_
     (error-response id ErrorCode-InvalidParams "textDocument/documentSymbol failed")]))

;; Inlay Hint
;;
;; Types only exist after expansion, so answering during the first
;; check-syntax run would send an empty list the client never asks about
;; again. Wait for the run, as semantic tokens do.
(define (inlay-hint id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ['range (as-Range range)])
     (define safe-doc (lsp-get-doc uri))

     (analysis-query safe-doc id (lambda (doc) (doc-inlay-hints doc range)) '())]
    [_ (error-response id ErrorCode-InvalidParams "textDocument/inlayHint failed")]))

;; Full document formatting request
(define (formatting! id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ['options (as-FormattingOptions opts)])

     (with-handlers ([exn:fail:fmt?
                      (lambda (exn)
                        (error-response id
                                        ErrorCode-RequestFailed
                                        (exn-message exn)))])
       (define settings current-formatting-settings)
       (define backend (Formatting-Settings-document-formatter settings))
       (success-from-doc
         id uri
         (λ (doc)
           (doc-format-edits doc
                             (Range (doc-abs-pos->pos doc 0)
                                    (doc-abs-pos->pos doc (doc-end-abs-pos doc)))
                             #:backend backend
                             #:fmt-settings (Formatting-Settings-fmt-settings settings)
                             #:formatting-options opts))))]
    [_
     (error-response id ErrorCode-InvalidParams "textDocument/formatting failed")]))

;; Range Formatting request
(define (range-formatting! id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ['range (as-Range range)]
                 ['options (as-FormattingOptions opts)])
     (define backend
       (Formatting-Settings-indentation-formatter current-formatting-settings))
     (success-from-doc
       id uri
       (λ (doc)
         (doc-format-edits doc
                           range
                           #:backend backend
                           #:formatting-options opts)))]
    [_
     (error-response id ErrorCode-InvalidParams "textDocument/rangeFormatting failed")]))

;; On-type formatting request
(define (on-type-formatting! id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ;; LSP `position` is the cursor after inserted `ch`.
                 ;; `doc-on-type-format-edits` maps that to the character.
                 ;; https://github.com/jeapostrophe/racket-langserver/issues/111
                 ['position (as-Pos pos)]
                 ['ch ch]
                 ['options (as-FormattingOptions opts)])
     (define backend
       (Formatting-Settings-indentation-formatter current-formatting-settings))
     (success-from-doc
       id uri
       (λ (doc)
         (doc-on-type-format-edits doc pos ch
                                   #:backend backend
                                   #:formatting-options opts)))]
    [_
     (error-response id ErrorCode-InvalidParams "textDocument/onTypeFormatting failed")]))

(define (full-semantic-tokens id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)])
     (semantic-tokens id (lsp-get-doc uri) 'full)]
    [_ (error-response id ErrorCode-InvalidParams "textDocument/semanticTokens/full failed")]))

(define (range-semantic-tokens id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier-js #:uri uri)]
                 ['range (as-Range range)])
     (semantic-tokens id (lsp-get-doc uri) range)]
    [_ (error-response id ErrorCode-InvalidParams "textDocument/semanticTokens/range failed")]))

(define (semantic-tokens id safe-doc range)
  (analysis-query
    safe-doc id
    (λ (doc)
      ;; Chosen over a request-time Range: this callback may run after analysis.
      (define tokens-range
        (if (eq? range 'full)
            (Range (doc-abs-pos->pos doc 0)
                   (doc-abs-pos->pos doc (doc-end-abs-pos doc)))
            range))
      (hash 'data
            (encode-semantic-tokens
              (λ (pos) (doc-abs-pos->pos doc pos))
              (doc-range-tokens doc tokens-range))))
    (hash 'data '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
  did-open!
  did-change!
  (contract-out
    [did-close! (jsexpr? . -> . void?)]
    [hover (exact-nonnegative-integer? jsexpr? . -> . (or/c jsexpr? (-> jsexpr?)))]
    [code-action (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
    [completion (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
    [signatureHelp (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
    [definition (exact-nonnegative-integer? jsexpr? . -> . (or/c jsexpr? (-> jsexpr?)))]
    [document-highlight (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
    [references (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
    [document-symbol (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
    [inlay-hint (exact-nonnegative-integer? jsexpr? . -> . (or/c jsexpr? (-> jsexpr?)))]
    [rename _rename rename (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
    [prepareRename (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
    [formatting! (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
    [range-formatting! (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
    [on-type-formatting! (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
    [full-semantic-tokens (exact-nonnegative-integer? jsexpr? . -> . (or/c jsexpr? (-> jsexpr?)))]
    [range-semantic-tokens (exact-nonnegative-integer? jsexpr? . -> . (or/c jsexpr? (-> jsexpr?)))])

  client-capability-hierarchical-document-symbol?)
