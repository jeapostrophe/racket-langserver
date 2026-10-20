#lang racket/base

;; Check Syntax products, and the outcomes request threads wait for.
;; `analyze-source` returns an `Analysis-Product`. The source controller
;; keeps that product's trace. Request threads receive an `Analysis-Summary`
;; or an `Analysis-Outcome`. Lifetime and reuse are in `analysis-lifecycle.md`.
(require json
         racket/class
         racket/contract
         racket/set
         "../common/json-util.rkt"
         "../common/interfaces.rkt"
         "../doclib/doc.rkt"
         "../doclib/hover.rkt"
         "../doclib/check-syntax.rkt"
         "../doclib/doc-trace.rkt"
         "../doclib/internal-types.rkt"
         "../workspace/source-store.rkt")

(provide analyze-source
         (struct-out Analysis-Summary)
         (struct-out Analysis-Product)
         (struct-out Analysis-Outcome)
         (struct-out Available-Analysis)
         (struct-out Analysis-Handle))

;; Immutable definition and hover facts copied from a successful trace.
(struct/contract Analysis-Summary
  ([definition-ranges (hash/c Module-Binding? Range? #:immutable #t)]
   [hover-summaries (hash/c Module-Binding? Hover-Code-Summary? #:immutable #t)])
  #:transparent)

;; One Check Syntax run in a worker.
;;
;; succeeded?: True when this product's Check Syntax run reported success.
;;   Worker and acceptance failures are `Analysis-Outcome` values.
;; summary: Definition and hover facts, or `#f` when the run produced none.
;; diagnostics: Diagnostic JSON with immutable strings. A run that produced
;;   no facts still supplies the diagnostics it collected.
;; trace: Mutable trace. The source controller is its only consumer.
;; contribution: Workspace contribution after successful analysis of a file.
;;   `#f` when the run produced no facts or the source has no file path.
(struct/contract Analysis-Product
  ([succeeded? boolean?]
   [summary (or/c #f Analysis-Summary?)]
   [diagnostics (listof jsexpr?)]
   [trace (is-a?/c build-trace%)]
   [contribution (or/c #f Doc-Contribution?)])
  #:transparent)

;; Terminal result of one Check Syntax or Resyntax attempt.
;;
;; state: `succeeded` when Check Syntax produced facts, or when Resyntax
;;   finished. `errors` when Check Syntax finished without facts. `failed`
;;   when the worker raised, exited, or timed out. `stopped` when the attempt
;;   was cancelled or the source was no longer eligible.
;; value: An `Analysis-Summary` after successful Check Syntax. `#f` after
;;   Check Syntax errors or successful Resyntax. The reason after `failed`
;;   or `stopped`.
(struct/contract Analysis-Outcome
  ([state (or/c 'succeeded 'errors 'failed 'stopped)]
   [value any/c])
  #:transparent)

;; A completed product installed for one snapshot. Queries may read it while
;; a later attempt is still running.
;;
;; snapshot: The analyzed snapshot. Queries compare its revision with the
;;   current source view.
;; completion-evt: Broadcast event of the attempt that produced this product
;;   in the current source lifetime. `#f` when the product was restored from
;;   a closed file. Restoration leaves the fresh attempt, scheduled by open,
;;   unfinished.
(struct/contract Available-Analysis
  ([snapshot Source-Snapshot?]
   [product Analysis-Product?]
   [completion-evt (or/c #f evt?)])
  #:transparent)

;; What request threads receive when they acquire analysis. The trace stays
;; with the source controller.
;;
;; reusable-summary: Installed `Analysis-Summary` the caller may use while
;;   `completion-evt` is not yet ready. `#f` when no facts are installed, or
;;   when that event is already ready. A ready outcome, including failure and
;;   stop, is taken from the event.
;; completion-evt: Broadcast event that yields an `Analysis-Outcome`. It is a
;;   newly scheduled attempt, or the installed result's completion when that
;;   success is reused. After using either field, the caller checks that the
;;   source revision is still current.
(struct/contract Analysis-Handle
  ([reusable-summary (or/c #f Analysis-Summary?)]
   [completion-evt evt?])
  #:transparent)

;; Diagnostic JSON may contain mutable strings from language extensions.
;; Make strings immutable, and copy hashes and lists, before the product is
;; shared. Later mutation of the worker's value leaves the shared diagnostics
;; unchanged.
(define (freeze-json value)
  (cond
    [(string? value) (string->immutable-string value)]
    [(hash? value)
     (for/hasheq ([(key field-value) (in-hash value)]) (values key (freeze-json field-value)))]
    [(list? value) (map freeze-json value)]
    [else value]))

;; Run Check Syntax for `snapshot` on a private document. On success, install
;; the trace on that document before copying definition and hover facts.
;; Those extractors read the document after that installation.
(define (analyze-source snapshot)
  (define doc (make-source-doc (Source-Snapshot-source snapshot) (Source-Snapshot-text snapshot)))
  (define check-syntax-result (doc-check-syntax doc))
  (define trace (CSResult-trace check-syntax-result))
  (define succeeded? (CSResult-succeed? check-syntax-result))
  (define contribution (and succeeded? (send trace get-contribution)))
  (when succeeded?
    (doc-update-trace! doc trace contribution (Doc-version doc)))
  (Analysis-Product succeeded?
                    (and succeeded? (Analysis-Summary (doc-definition-ranges doc)
                                                      (doc-hover-summaries doc)))
                    (freeze-json (->jsexpr (set->list (send trace get-warn-diags))))
                    trace contribution))
