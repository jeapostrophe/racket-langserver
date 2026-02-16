#lang racket/base

;; internal structs ;;
;;
;; Placement rule:
;; - Keep internal runtime/domain structs here (analysis state, sentinels,
;;   and other non-protocol helpers).
;; - Do not place protocol JSON payload structs here; those belong in
;;   `interfaces.rkt` as `define-json-struct` types.

(require racket/contract
         racket/match
         racket/dict
         data/interval-map)

(provide
  (contract-out
    [struct Decl ([filename any/c]
                  [id any/c]
                  [left exact-nonnegative-integer?]
                  [right exact-nonnegative-integer?])])
  interval-map-of)

(struct Decl (filename id left right) #:transparent)

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

