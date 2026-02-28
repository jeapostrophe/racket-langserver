#lang racket/base

;; internal structs ;;
;;
;; Placement rule:
;; - Keep internal runtime/domain structs here (analysis state, sentinels,
;;   and other non-protocol helpers).
;; - Do not place protocol JSON payload structs here; those belong in
;;   `interfaces.rkt` as `define-json-struct` types.

(require racket/contract
         racket/class
         racket/match
         racket/dict
         racket/logging
         drracket/check-syntax
         data/interval-map)

(provide
  (struct-out ExpandResult)
  (struct-out Decl)
  interval-map-of)

;; Struct to hold the result of an expansion.
;; pre-stx: the syntax before expansion, result of `read-syntax`
;; post-stx: the syntax after expansion, result of `expand`
;; logs: the log collected during expansion
(struct/contract ExpandResult
  ([pre-stx (or/c syntax? exn? eof-object?)]
   [post-stx (or/c syntax? exn? #f)]
   [logs (listof (vector/c log-level/c string? any/c (or/c symbol? #f)))])
  #:transparent)

(struct/contract Decl
  ([filepath (or/c path-string? #f)]
   [id (or/c symbol? #f)]
   [left exact-nonnegative-integer?]
   [right exact-nonnegative-integer?])
  #:transparent)

(define undef-object (gensym 'undef))

(define (undef? x)
  (eq? x undef-object))

(define (undef/c pred?)
  (λ (x)
    (or/c (undef? x) (pred? x))))

(define (interval-map-of value/c)
  (define value-flat/c (coerce-flat-contract 'interval-map-of value/c))
  (define value? (flat-contract-predicate value-flat/c))
  (flat-named-contract
    `(interval-map-of ,(contract-name value-flat/c))
    (lambda (m)
      (and (interval-map? m)
           (for/and ([(k value) (in-dict m)])
             (value? value))))))

(provide undef?
         undef/c
         undef-object)

