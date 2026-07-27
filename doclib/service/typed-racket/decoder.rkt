#lang racket/base

;; Convert Typed Racket online-check-syntax log records into normalized
;; annotations.
;;
;; Typed Racket does not put type tooltips on the final expanded syntax that
;; check-syntax walks. During expand it logs syntax objects that carry
;; 'mouse-over-tooltips; this module reads that log channel instead.
;;
;; Approach and leaf walk follow racket-mode's online-check-syntax handling
;; (racket-xp-mode):
;; https://github.com/greghendershott/racket-mode/blob/master/racket/online-check-syntax.rkt
;; We adapt it for LSP: classify TR message suffixes, and split inferred types
;; (hover) from type-error tooltips (diagnostics).
;;
;; Keep classification narrow. Other languages may use the same log topic and
;; 'mouse-over-tooltips property for different purposes.

(require racket/contract
         racket/list
         racket/match
         racket/string
         srfi/2)

(provide (struct-out Inferred-Type)
         (struct-out Type-Error)
         typed-racket-inferred-type-message
         typed-racket-type-error-message
         typed-racket-log-annotations)

;; Invariant: these suffixes are the only classification rule for TR tooltip
;; logs. Keep the strings exact. Match with string-suffix? so an optional
;; topic prefix (for example "online-check-syntax: ") still classifies.
(define typed-racket-inferred-type-message
  "TR's tooltip syntaxes; this message is ignored")

(define typed-racket-type-error-message
  "TR's type error tooltip syntaxes; this message is ignored")

;; Decoded tooltip leaf. Kind turns these into Inferred-Type or Type-Error.
(struct/contract Mouseover
  ([source path-string?]
   [start exact-nonnegative-integer?]
   [end exact-nonnegative-integer?]
   [text string?])
  #:transparent)

(struct/contract Inferred-Type
  ([source path-string?]
   [start exact-nonnegative-integer?]
   [end exact-nonnegative-integer?]
   [text string?])
  #:transparent)

(struct/contract Type-Error
  ([source path-string?]
   [start exact-nonnegative-integer?]
   [end exact-nonnegative-integer?]
   [message string?])
  #:transparent)

(define (tooltip-text? value)
  (or (string? value)
      (and (procedure? value)
           (procedure-arity-includes? value 0))))

;; 'mouse-over-tooltips leaf on supported Racket versions:
;; (vector located-syntax start end text-or-thunk).
;; The property value is an arbitrary pair tree of such leaves.

;; Syntax that carries the tooltip source; used to keep this document only.
(define (tooltip-leaf-located-syntax leaf)
  (vector-ref leaf 0))

;; Absolute character offset where the tooltip span starts.
(define (tooltip-leaf-start leaf)
  (vector-ref leaf 1))

;; Absolute character offset where the tooltip span ends.
(define (tooltip-leaf-end leaf)
  (vector-ref leaf 2))

;; String text, or a zero-arg thunk that returns it.
(define (tooltip-leaf-text-or-thunk leaf)
  (vector-ref leaf 3))

(define (tooltip-leaf? value)
  (and (vector? value)
       (= (vector-length value) 4)
       (syntax? (tooltip-leaf-located-syntax value))
       (exact-nonnegative-integer? (tooltip-leaf-start value))
       (exact-nonnegative-integer? (tooltip-leaf-end value))
       (tooltip-text? (tooltip-leaf-text-or-thunk value))))

;; Decode one leaf for this document, or #f when foreign, empty, or malformed.
;; Do not force a provider thunk before the source check: foreign-source
;; tooltip code must not run while analyzing this document.
(define (tooltip-leaf->mouseover leaf source)
  (define located-syntax (tooltip-leaf-located-syntax leaf))
  (define start (tooltip-leaf-start leaf))
  (define end (tooltip-leaf-end leaf))
  (define text-or-thunk (tooltip-leaf-text-or-thunk leaf))
  (and-let* ([(equal? source (syntax-source located-syntax))]
             [(< start end)]
             [text (if (string? text-or-thunk)
                       text-or-thunk
                       (text-or-thunk))]
             [(string? text)]
             [(not (string=? text ""))])
    (Mouseover source start end text)))

(define (tooltip-property->mouseovers property source)
  (define mouseovers '())

  (define (walk value)
    (cond
      [(pair? value)
       (walk (car value))
       (walk (cdr value))]
      [(tooltip-leaf? value)
       (define mouseover (tooltip-leaf->mouseover value source))
       (when mouseover
         (set! mouseovers (cons mouseover mouseovers)))]))

  (walk property)
  (reverse mouseovers))

;; online-check-syntax log vector: (vector level message data topic)
(define (log-message log)
  ;; Log message string; TR classification matches its suffix.
  (vector-ref log 1))
(define (log-data log)
  ;; Payload: list of syntax objects that carry 'mouse-over-tooltips.
  (vector-ref log 2))
(define (log-topic log)
  ;; Must be 'online-check-syntax for records we handle.
  (vector-ref log 3))

;; Classify by message suffix only (see invariant above). #f for any other
;; online-check-syntax record.
(define (typed-racket-log-kind log)
  (and-let* ([(vector? log)]
             [(= (vector-length log) 4)]
             [(eq? (log-topic log) 'online-check-syntax)]
             [message (log-message log)]
             [(string? message)])
    (cond
      [(string-suffix? message typed-racket-inferred-type-message)
       'inferred-type]
      [(string-suffix? message typed-racket-type-error-message)
       'type-error]
      [else #f])))

(define (log-payload-syntaxes log)
  (define data (log-data log))
  (and (list? data)
       (andmap syntax? data)
       data))

;; A type error may use two endpoint mouseovers for a compound form. Typed
;; Racket emits one log record per error, so equal-message endpoints describe
;; one range. Distinct messages stay separate diagnostics.
(define (mouseovers->type-errors mouseovers)
  (match mouseovers
    ['() '()]
    [(cons first-mouseover rest-mouseovers)
     (define shared-message?
       (for/and ([mouseover (in-list rest-mouseovers)])
         (and (equal? (Mouseover-source mouseover)
                      (Mouseover-source first-mouseover))
              (string=? (Mouseover-text mouseover)
                        (Mouseover-text first-mouseover)))))
     (cond
       [shared-message?
        ;; Union the endpoint ranges: min start .. max end.
        (list (Type-Error (Mouseover-source first-mouseover)
                          (apply min (map Mouseover-start mouseovers))
                          (apply max (map Mouseover-end mouseovers))
                          (Mouseover-text first-mouseover)))]
       [else
        (for/list ([mouseover (in-list mouseovers)])
          (Type-Error (Mouseover-source mouseover)
                      (Mouseover-start mouseover)
                      (Mouseover-end mouseover)
                      (Mouseover-text mouseover)))])]))

(define (log-record->annotations log kind source)
  (define syntaxes (log-payload-syntaxes log))
  (cond
    [(not syntaxes) '()]
    [else
     (define mouseovers
       (append*
         (for/list ([stx (in-list syntaxes)])
           ;; Do not traverse the logged syntax datum. It is only a carrier for
           ;; 'mouse-over-tooltips. Walking it pulls unrelated annotations and
           ;; needs the original expansion namespace.
           (tooltip-property->mouseovers
             (syntax-property stx 'mouse-over-tooltips)
             source))))
     (case kind
       [(inferred-type)
        (for/list ([mouseover (in-list mouseovers)])
          (Inferred-Type (Mouseover-source mouseover)
                         (Mouseover-start mouseover)
                         (Mouseover-end mouseover)
                         (Mouseover-text mouseover)))]
       [(type-error)
        (mouseovers->type-errors mouseovers)])]))

(define/contract (typed-racket-log-annotations logs source)
  (-> (listof vector?)
      path-string?
      (listof (or/c Inferred-Type? Type-Error?)))
  (append*
    (for/list ([log (in-list logs)])
      (define kind (typed-racket-log-kind log))
      (cond
        [(not kind) '()]
        [else
         ;; One malformed provider record must not abort the rest of the batch.
         (with-handlers ([exn:fail?
                          (lambda (exn)
                            (eprintf "Ignoring malformed Typed Racket ~a record: ~a\n"
                                     kind
                                     (exn-message exn))
                            '())])
           (log-record->annotations log kind source))]))))
