#lang racket/base

;; Worker products separate shared immutable facts from the trace transferred
;; exclusively to the consumer. Available results are independent of refresh jobs.
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
         (struct-out Analysis-Facts)
         (struct-out Analysis-Product)
         (struct-out Analysis-Outcome)
         (struct-out Available-Analysis))

(struct/contract Analysis-Facts
  ([definitions (hash/c Module-Binding? Range? #:immutable #t)]
   [hover-summaries (hash/c Module-Binding? Hover-Code-Summary? #:immutable #t)])
  #:transparent)

(struct/contract Analysis-Product
  ([succeeded? boolean?]
   [facts (or/c #f Analysis-Facts?)]
   [diagnostics (listof jsexpr?)]
   [trace (is-a?/c build-trace%)]
   [contribution (or/c #f Doc-Contribution?)])
  #:transparent)

(struct/contract Analysis-Outcome
  ([state (or/c 'succeeded 'unsuccessful 'failed 'stopped)]
   [value any/c])
  #:transparent)

(struct/contract Available-Analysis
  ([snapshot Source-Snapshot?]
   [product Analysis-Product?])
  #:transparent)

;; Diagnostics are protocol values, including strings returned by language
;; extensions. Freeze them before publishing or sharing a completed outcome.
(define (freeze-json value)
  (cond
    [(string? value) (string->immutable-string value)]
    [(hash? value) (for/hasheq ([(key val) (in-hash value)]) (values key (freeze-json val)))]
    [(list? value) (map freeze-json value)]
    [else value]))

(define (analyze-source snapshot)
  (define doc (make-source-doc (Source-Snapshot-source snapshot) (Source-Snapshot-text snapshot)))
  (define result (doc-check-syntax doc))
  (define trace (CSResult-trace result))
  (define succeeded? (CSResult-succeed? result))
  (define contribution (and succeeded? (send trace get-contribution)))
  (when succeeded?
    (doc-update-trace! doc trace contribution 0))
  (Analysis-Product succeeded?
                    (and succeeded? (Analysis-Facts (doc-definition-ranges doc)
                                                    (doc-hover-summaries doc)))
                    (freeze-json (->jsexpr (set->list (send trace get-warn-diags))))
                    trace contribution))
