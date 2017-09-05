#lang racket/base
(require json
         racket/contract/base)

(struct exn:fail:langserver exn:fail (id code) #:transparent)

(define (raise-langserver-error id code message)
  (raise (exn:fail:langserver message (current-continuation-marks) id code)))

(provide
 (struct-out exn:fail:langserver)
 (contract-out
  [raise-langserver-error
   ((or/c number? string? (json-null)) number? string? . -> . exn:fail:langserver?)]))