#lang racket/base

;; Reading the pre-expand syntax of a document.
;;
;; `read-syntax` positions are 1-based and count from the start of the port the
;; document was read from, so the absolute character offset of a form is one
;; less.

(require racket/contract
         syntax/parse
         "../common/interfaces.rkt")

(provide (contract-out
           [syntax-start (-> syntax? (or/c #f exact-nonnegative-integer?))]
           [syntax-end (-> syntax? (or/c #f exact-nonnegative-integer?))]
           [syntax-char-range (-> syntax? (or/c #f CharRange?))]
           [syntax-keyword (-> syntax? (or/c #f keyword?))]
           [code-form? (-> syntax? boolean?)]
           [module-body-forms (-> syntax? (listof syntax?))]
           [for-each-code-form (-> (listof syntax?) (-> syntax? any) void?)]))

(define (syntax-start stx)
  (define position (syntax-position stx))
  (and position (sub1 position)))

(define (syntax-end stx)
  (define start (syntax-start stx))
  (define span (syntax-span stx))
  (and start span (+ start span)))

(define (syntax-char-range stx)
  (define start (syntax-start stx))
  (define end (syntax-end stx))
  (and start end (CharRange start end)))

(define (syntax-keyword stx)
  (define datum (syntax-e stx))
  (and (keyword? datum) datum))

;; What the reader quotes is data rather than code, so nothing is read from it.
(define-syntax-class quotation
  #:datum-literals (quote quasiquote syntax quasisyntax)

  (pattern ((~or quote quasiquote syntax quasisyntax) _ ...)))

(define (code-form? stx)
  (syntax-parse stx
    [_:quotation #f]
    [(_ _ ...) #t]
    [_ #f]))

;; The forms a `#lang` document is made of. Reading one yields a `module` form,
;; whose body the reader may have wrapped in `#%module-begin`; a document that
;; is not a module is its own only form.
(define (module-body-forms stx)
  (syntax-parse stx
    #:datum-literals (module #%module-begin)
    [(module _name _language (#%module-begin body ...))
     (syntax->list #'(body ...))]
    [(module _name _language body ...)
     (syntax->list #'(body ...))]
    [_ (list stx)]))

;; Visit each parenthesized form and everything written inside it, outermost
;; first and in document order.
(define (for-each-code-form stxs proc)
  (for ([stx (in-list stxs)])
    (when (code-form? stx)
      (proc stx)
      (for-each-code-form (syntax->list stx) proc))))
