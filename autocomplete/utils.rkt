;; copied from https://github.com/yjqww6/drcomplete/blob/master/drcomplete-required/private/utils.rkt

; drcomplete
; Copyright (c) 2019 yjqww6

; This package is distributed under the GNU Lesser General Public
; License (LGPL).  This means that you can link drcomplete into proprietary
; applications, provided you follow the rules stated in the LGPL.  You
; can also modify this package; if you distribute a modified version,
; you must distribute it under the terms of the LGPL, which in
; particular means that you must release the source code for the
; modified software.  See http://www.gnu.org/copyleft/lesser.html
; for more information.
#lang racket/base
(require syntax/parse/define (for-syntax racket/base))

(define-syntax-parser cond-bound
  [(_ [(X:id ...) Body:expr ...] Rest ...)
   (if (andmap identifier-binding (syntax->list #'(X ...)))
       #'(begin Body ...)
       (syntax/loc this-syntax (cond-bound Rest ...)))]
  [(_  [(~literal else) Body:expr ...])
   #'(begin Body ...)])

(define-syntax-parse-rule (cond-use-bound Body:expr ... (~optional (~seq #:else Alt:expr ...) #:defaults ([(Alt 1) '()])))
  (cond-bound
   [(syntax-bound-phases syntax-bound-symbols)
    Body ...]
   [else Alt ...]))

(define (visible? id)
  (for/and ([scope (in-list
                    (hash-ref (syntax-debug-info id)
                              'context (λ () '())))])
    (not (eq? 'macro (vector-ref scope 1)))))

(provide (all-defined-out))
