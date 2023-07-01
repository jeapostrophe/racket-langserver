#lang racket/base

;; (struct/c Doc (is-a?/c text%) (is-a?/c build-trace%))
(struct Doc
  (text trace) #:transparent #:mutable)

(provide Doc
         Doc-text
         Doc-trace
         set-Doc-trace!)
