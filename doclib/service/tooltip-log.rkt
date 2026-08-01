#lang racket/base

;; Decode tooltip syntax properties carried by `online-check-syntax` logs.
;; Message classification is routing policy; property decoding is shared by all
;; providers. Invalid logs and leaves are ignored.

(require racket/contract
         racket/match
         racket/string
         srfi/2)

(provide (struct-out Tooltip)
         typed-racket-inferred-type-message
         typed-racket-type-error-message
         online-tooltip-log?
         online-tooltip-log-message
         online-tooltip-log-tooltips
         tooltip-log-kind)

(define typed-racket-inferred-type-message
  "TR's tooltip syntaxes; this message is ignored")

(define typed-racket-type-error-message
  "TR's type error tooltip syntaxes; this message is ignored")

(struct/contract Tooltip
  ([source path-string?]
   [start exact-nonnegative-integer?]
   [end exact-nonnegative-integer?]
   [text string?])
  #:transparent)

;; online-check-syntax log vector: (vector level message data topic)
(define (online-tooltip-log? value)
  (match value
    [(vector _level (? string?) (? list? syntaxes) 'online-check-syntax)
     (andmap syntax? syntaxes)]
    [_ #f]))

(define/contract (online-tooltip-log-message log)
  (-> online-tooltip-log? string?)
  (vector-ref log 1))

(define/contract (tooltip-log-kind message)
  (-> string?
      (or/c 'typed-racket-inferred-type
            'typed-racket-type-error
            'mouse-over))
  (cond
    [(string-suffix? message typed-racket-inferred-type-message)
     'typed-racket-inferred-type]
    [(string-suffix? message typed-racket-type-error-message)
     'typed-racket-type-error]
    [else
     'mouse-over]))

(define (tooltip-text? value)
  (or (string? value)
      (and (procedure? value)
           (procedure-arity-includes? value 0))))

;; A leaf is (vector located-syntax start end text-or-thunk). Property values
;; are arbitrary pair trees whose other leaves are ignored.
(define (tooltip-leaf? value)
  (match value
    [(vector (? syntax?)
             (? exact-nonnegative-integer?)
             (? exact-nonnegative-integer?)
             (? tooltip-text?))
     #t]
    [_ #f]))

;; Filter by source before forcing provider code. A foreign tooltip thunk must
;; not run while this document is being analyzed.
(define (tooltip-leaf->tooltip leaf source kind)
  (match-define (vector located-syntax start end text-or-thunk) leaf)
  (and-let* ([(equal? source (syntax-source located-syntax))]
             [(< start end)])
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (eprintf "Ignoring malformed ~a tooltip: ~a\n"
                                kind
                                (exn-message exn))
                       #f)])
      (define text
        (if (string? text-or-thunk)
            text-or-thunk
            (text-or-thunk)))
      (and (string? text)
           (not (string=? text ""))
           (Tooltip source start end text)))))

(define (tooltip-property->tooltips property source kind)
  (define tooltips '())

  (define (walk value)
    (cond
      [(pair? value)
       (walk (car value))
       (walk (cdr value))]
      [(tooltip-leaf? value)
       (define tooltip
         (tooltip-leaf->tooltip value source kind))
       (when tooltip
         (set! tooltips (cons tooltip tooltips)))]))

  (walk property)
  (reverse tooltips))

(define/contract (online-tooltip-log-tooltips log source)
  (-> online-tooltip-log? path-string? (listof Tooltip?))
  (define kind
    (tooltip-log-kind (online-tooltip-log-message log)))
  (for*/list ([stx (in-list (vector-ref log 2))]
              [tooltip
               (in-list
                 (tooltip-property->tooltips
                   (syntax-property stx 'mouse-over-tooltips)
                   source
                   kind))])
    tooltip))
