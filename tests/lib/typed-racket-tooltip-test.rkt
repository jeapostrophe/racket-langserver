#lang racket/base

;; Typed Racket policy tests over generic decoded tooltip records. Generic log
;; and property-tree behavior lives in tooltip-log-test.rkt.

(require rackunit
         "../../common/interfaces.rkt"
         "../../doclib/editor.rkt"
         "../../doclib/service/tooltip-log.rkt"
         "../../doclib/service/typed-racket/service.rkt"
         racket/class
         racket/set)

(define source
  (string->path "/tmp/typed-racket-tooltip-test.rkt"))

(define (make-service [text "0123456789"])
  (define doc-text
    (new lsp-editor%))
  (send doc-text insert text 0)
  (new typed-racket%
    [src source]
    [doc-text doc-text]))

(define (diagnostics-by-message service)
  (sort (set->list (send service get-diagnostics))
        string<?
        #:key Diagnostic-message))

(module+ test
  (test-case
    "Typed Racket message suffixes classify without a logger prefix"
    (check-equal?
      (tooltip-log-kind typed-racket-inferred-type-message)
      'typed-racket-inferred-type)
    (check-equal?
      (tooltip-log-kind typed-racket-type-error-message)
      'typed-racket-type-error)
    (check-equal?
      (tooltip-log-kind
        (string-append "online-check-syntax: "
                       typed-racket-inferred-type-message))
      'typed-racket-inferred-type))

  (test-case
    "Inferred tooltips populate the type interval map"
    (define service
      (make-service))
    (send service
          consume-inferred-tooltips
          (list (Tooltip source 0 1 "Integer")
                (Tooltip source 4 5 "String")))
    (define-values (first-start first-end first-text)
      (send service inferred-type-at 0))
    (check-equal? (list first-start first-end first-text)
                  (list 0 1 "Integer"))
    (define-values (second-start second-end second-text)
      (send service inferred-type-at 4))
    (check-equal? (list second-start second-end second-text)
                  (list 4 5 "String")))

  (test-case
    "Matching type-error endpoints merge into one diagnostic range"
    (define service
      (make-service))
    (send service
          consume-type-error-tooltips
          (list (Tooltip source 0 1 "type mismatch")
                (Tooltip source 4 5 "type mismatch")))
    (define diagnostics
      (diagnostics-by-message service))
    (check-equal? (length diagnostics) 1)
    (check-equal? (Diagnostic-range (car diagnostics))
                  (Range (Pos 0 0) (Pos 0 5)))
    (check-equal? (Diagnostic-message (car diagnostics))
                  "type mismatch"))

  (test-case
    "Distinct type-error messages remain separate diagnostics"
    (define service
      (make-service))
    (send service
          consume-type-error-tooltips
          (list (Tooltip source 0 1 "expected Number")
                (Tooltip source 4 5 "given String")))
    (define diagnostics
      (diagnostics-by-message service))
    (check-equal? (map Diagnostic-message diagnostics)
                  (list "expected Number" "given String")))

  (test-case
    "The service rejects tooltip ranges beyond the document snapshot"
    (define service
      (make-service "abc"))
    (send service
          consume-inferred-tooltips
          (list (Tooltip source 0 1 "Valid")
                (Tooltip source 2 4 "Outside")))
    (send service
          consume-type-error-tooltips
          (list (Tooltip source 2 4 "Outside error")))
    (define-values (start end text)
      (send service inferred-type-at 0))
    (check-equal? (list start end text)
                  (list 0 1 "Valid"))
    (define-values (_outside-start _outside-end outside-text)
      (send service inferred-type-at 2))
    (check-false outside-text)
    (check-equal? (set-count (send service get-diagnostics)) 0)))
