#lang racket/base

;; Hover card data and Markdown rendering.
;;
;; Maps type, source, annotation, and documentation facts onto fixed slots and
;; renders each present slot with a label. `doc-hover` collects the facts.
;;
;; Invariant: every character inside a slot is verbatim from its source. Do not
;; trim, inline, paraphrase, or length-gate presentation here.
;;
;; Fixed render order: type, definition, annotation, documentation.

(require "service/hover/types.rkt"
         racket/contract
         racket/match
         racket/string)

(provide (struct-out Hover-Annotation)
         (struct-out Hover-Card)
         (struct-out Hover-Code-Summary)
         (struct-out Hover-Definition)
         (struct-out Hover-Documentation)
         build-hover-card
         render-hover-card
         hover-card-has-content?)

(struct/contract Hover-Code-Summary
  ([text string?]
   [fence-language string?])
  #:transparent)

(struct/contract Hover-Definition
  ([kind (or/c 'source 'signature)]
   [summary Hover-Code-Summary?])
  #:transparent)

(struct/contract Hover-Documentation
  ([body string?]
   [link (or/c string? #f)])
  #:transparent)

(struct/contract Hover-Card
  ([type (or/c Hover-Code-Summary? #f)]
   [type-stale? boolean?]
   [definition (or/c Hover-Definition? #f)]
   [annotation (or/c Hover-Annotation? #f)]
   [documentation (or/c Hover-Documentation? #f)])
  #:transparent)

;; Map hover inputs onto the fixed renderer slots. Same-file source wins over a
;; documentation signature. `link` is the final online URL.
(define (build-hover-card #:type-text type-text
                          #:type-stale? type-stale?
                          #:annotation annotation
                          #:link link
                          #:signature signature
                          #:source-summary source-summary
                          #:documentation-text documentation-text)
  (define type
    (and (non-empty-text? type-text)
         (Hover-Code-Summary type-text "racket")))
  (define definition
    (cond
      [source-summary
       (Hover-Definition 'source source-summary)]
      [signature
       (Hover-Definition 'signature
                         (Hover-Code-Summary signature "racket"))]
      [else #f]))
  (define kept-annotation
    (and annotation
         (non-empty-text? (Hover-Annotation-text annotation))
         annotation))
  (define documentation
    (and link
         (Hover-Documentation documentation-text link)))
  (Hover-Card type
              (and type type-stale?)
              definition
              kept-annotation
              documentation))

(define (non-empty-text? text)
  (and (string? text)
       (not (string=? text ""))))

(define (render-code-summary summary)
  (define text
    (Hover-Code-Summary-text summary))
  (define fence-language
    (Hover-Code-Summary-fence-language summary))
  (and (non-empty-text? text)
       (format "```~a\n~a\n```" fence-language text)))

(define (render-labeled-fence label summary)
  (define fenced
    (render-code-summary summary))
  (and fenced
       (format "**~a**\n~a" label fenced)))

(define (render-type type type-stale?)
  (and type
       (render-labeled-fence (if type-stale?
                                 "Type (stale)"
                                 "Type")
                             type)))

(define (definition-label kind)
  (match kind
    ['source "Source"]
    ['signature "Signature"]))

(define (render-definition definition)
  (and definition
       (render-labeled-fence
         (definition-label (Hover-Definition-kind definition))
         (Hover-Definition-summary definition))))

(define (annotation-label kind)
  (match kind
    ['mouse-over-status "Mouse-over status"]
    ['log-tooltip "Log tooltip"]))

(define (render-annotation annotation)
  (and annotation
       (non-empty-text? (Hover-Annotation-text annotation))
       (format "**~a**\n\n~a"
               (annotation-label (Hover-Annotation-kind annotation))
               (Hover-Annotation-text annotation))))

(define (render-documentation documentation)
  (cond
    [(not documentation) #f]
    [else
     (define body
       (Hover-Documentation-body documentation))
     (define link
       (Hover-Documentation-link documentation))
     (define online-link
       (and (non-empty-text? link)
            (format "[Online docs](~a)" link)))
     (define body-present?
       (non-empty-text? body))
     (define heading
       (if online-link
           (format "**Documentation** | ~a" online-link)
           "**Documentation**"))
     (and (or online-link body-present?)
          (if body-present?
              (format "~a\n\n~a" heading body)
              heading))]))

(define (hover-card-has-content? card)
  (or (Hover-Card-type card)
      (Hover-Card-definition card)
      (Hover-Card-annotation card)
      (match (Hover-Card-documentation card)
        [#f #f]
        [(Hover-Documentation body link)
         (or (non-empty-text? body)
             (non-empty-text? link))])))

(define/contract (render-hover-card card)
  (-> Hover-Card? string?)
  (define type
    (render-type (Hover-Card-type card)
                 (Hover-Card-type-stale? card)))
  (define definition
    (render-definition (Hover-Card-definition card)))
  (define annotation
    (render-annotation (Hover-Card-annotation card)))
  (define documentation
    (render-documentation (Hover-Card-documentation card)))
  (string-join
    (filter values
            (list type definition annotation documentation))
    "\n\n"))
