#lang racket/base

(require "interface.rkt"
         racket/class
         "../internal-types.rkt"
         drracket/check-syntax)

(provide definition%)

(define definition%
  (class base-service%
    (init-field src)
    (super-new)

    (define definitions (make-hash))

    (define/override (get)
      definitions)

    (define/override (reset)
      (set! definitions (make-hash)))

    (define/override (syncheck:add-definition-target/phase-level+space
                       _src-obj start end id submods phase+space)
      (define decl (Decl src submods phase+space id start end))
      (hash-set! definitions (list submods phase+space id) decl))))
