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

    (define/override (syncheck:add-definition-target _src-obj start end id _mods)
      (hash-set! definitions id (Decl src id start end)))))

