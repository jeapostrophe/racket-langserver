#lang racket/base

(provide walk
         walk-online)

;; powered by https://github.com/yjqww6/drcomplete

(require racket/set
         (prefix-in user-defined: "autocomplete/user-defined.rkt")
         (prefix-in required: "autocomplete/required.rkt")
         (prefix-in module-name: "autocomplete/module.rkt"))

;; Get completions that will be cached for every successful expand.
;; Syntax -> (Listof Symbol)
(define (walk stx)
  (append
    (set->list (user-defined:walk stx))
    (with-handlers ([(λ (_exn) #t) (λ (_) '())])
      (set->list (required:walk-module stx)))))

;; Get completions that will be computed for every text change.
;; String -> (Listof Symbol)
(define (walk-online str-before-cursor)
  (map string->symbol (set->list (module-name:get-completions str-before-cursor))))

