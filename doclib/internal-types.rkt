#lang racket/base

;; internal structs ;;
;;
;; Placement rule:
;; - Keep internal runtime/domain structs here (analysis state, sentinels,
;;   and other non-protocol helpers).
;; - Do not place protocol JSON payload structs here; those belong in
;;   `interfaces.rkt` as `define-json-struct` types.

(require "check-syntax-compat.rkt"
         "../common/interfaces.rkt"
         racket/contract
         racket/dict
         racket/logging
         data/interval-map)

(provide
  ExpandResult
  ExpandResult?
  ExpandResult-logs
  (struct-out Module-Binding)
  (struct-out Doc-Contribution)
  (struct-out Reference-Source)
  (struct-out Document-Reference-Result)
  interval-map-of
  ExpandResult-pre-syntax
  ExpandResult-post-syntax
  ExpandResult-pre-exn
  ExpandResult-post-exn
  ExpandResult-all-succeed?)

;; Struct to hold the result of an expansion.
;; pre-stx: the syntax before expansion, result of `read-syntax`
;; post-stx: the syntax after expansion, result of `expand`
;; logs: the log collected during expansion
(struct/contract ExpandResult
  ([pre-stx (or/c syntax? exn? eof-object?)]
   [post-stx (or/c syntax? exn? #f)]
   [logs (listof (vector/c log-level/c string? any/c (or/c symbol? #f)))])
  #:transparent)

(define (maybe-syntax x)
  (and (syntax? x) x))

(define (maybe-exn x)
  (and (exn? x) x))

(define/contract (ExpandResult-pre-syntax er)
  (-> ExpandResult? (or/c syntax? #f))
  (maybe-syntax (ExpandResult-pre-stx er)))

(define/contract (ExpandResult-post-syntax er)
  (-> ExpandResult? (or/c syntax? #f))
  (maybe-syntax (ExpandResult-post-stx er)))

(define/contract (ExpandResult-pre-exn er)
  (-> ExpandResult? (or/c exn? #f))
  (maybe-exn (ExpandResult-pre-stx er)))

(define/contract (ExpandResult-post-exn er)
  (-> ExpandResult? (or/c exn? #f))
  (maybe-exn (ExpandResult-post-stx er)))

(define/contract (ExpandResult-all-succeed? er)
  (-> ExpandResult? boolean?)
  (and (ExpandResult-pre-syntax er)
       (ExpandResult-post-syntax er)
       #t))

;; Unique identity for a module-backed binding: filepath, submods, phase+space,
;; and id.
(struct/contract Module-Binding
  ([filepath path?]
   [submods (listof symbol?)]
   [phase+space phase+space-shift?]
   [id symbol?])
  #:transparent)

;; Immutable cross-file facts derived from one completed document analysis.
(struct/contract Doc-Contribution
  ([path path?]
   [references (hash/c Module-Binding? (listof Location?) #:immutable #t)]
   [definitions (hash/c Module-Binding? Location? #:immutable #t)])
  #:transparent)

;; Reference locations supplied by exactly one document.
(struct/contract Reference-Source
  ([path path?]
   [locations (listof Location?)])
  #:transparent)

;; Live document references plus the exact cross-document identity, when any.
(struct/contract Document-Reference-Result
  ([source Reference-Source?]
   [module-binding (or/c Module-Binding? #f)])
  #:transparent)

(define (interval-map-of value/c)
  (define value-flat/c (coerce-flat-contract 'interval-map-of value/c))
  (define value? (flat-contract-predicate value-flat/c))
  (flat-named-contract
    `(interval-map-of ,(contract-name value-flat/c))
    (lambda (m)
      (and (interval-map? m)
           (for/and ([(k value) (in-dict m)])
             (value? value))))))

