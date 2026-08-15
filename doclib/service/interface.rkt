#lang racket/base

(require "../check-syntax-compat.rkt"
         racket/class
         drracket/check-syntax)

(provide service<%>
         base-service%)

(define service<%>
  (interface (syncheck-annotations<%>)
    get
    expand
    contract
    reset
    walk-stx
    walk-log
    add-log-tooltip))

(define base-service%
  (class* (check-syntax-annotations-mixin object%)
    (service<%> syncheck-annotations<%>)

    (super-new)

    ;; return data
    (define/public (get)
      #f)

    ;; insert text between from start to end (exclusive)
    (define/public (expand start end)
      (void))

    ;; delete text between from start to end (exclusive)
    (define/public (contract start end)
      (void))

    ;; clear all content
    (define/public (reset)
      (void))

    ;; walk syntax expansion result
    (define/public (walk-stx expand-result)
      (void))

    (define/public (walk-log log)
      (void))

    (define/public (add-log-tooltip source start end text)
      (void))
    ))
