#lang racket/base

;; Hover card data and Markdown rendering.
;;
;; `doc-hover` collects type, check-syntax, source, and docs facts, then
;; `build-hover-card` maps them onto fixed slots and `render-hover-card`
;; turns the card into Markdown. All cards use the same layout.
;;
;; Invariant: the renderer decides only whether a slot appears and where it
;; appears. Every character inside a slot is verbatim from its source. Do not
;; trim, inline, paraphrase, or length-gate presentation here.
;;
;; Fixed render order: type, definition, documentation (note then link then
;; body), standalone note. Always label a type fence (`Type` / `Type
;; (stale)`). Label `Source` or `Signature` only when a type fence precedes
;; the definition. Check-syntax mouse-over text is unlabeled prose before the
;; online-docs link, or alone when it is the only slot.

(require racket/contract
         racket/match
         racket/string
         srfi/2)

(provide (struct-out Hover-Card)
         (struct-out Hover-Code-Summary)
         (struct-out Hover-Definition)
         (struct-out Hover-Documentation)
         build-hover-card
         render-hover-card
         hover-card-has-content?)

;; One fenced code block (type, source, or signature text).
;; Empty text omits that fence when rendering.
(struct/contract Hover-Code-Summary
  ([text string?]
   [fence-language string?])
  #:transparent)

;; Definition slot: same-file source form or a docs signature.
(struct/contract Hover-Definition
  ([kind (or/c 'source 'signature)]
   [summary Hover-Code-Summary?])
  #:transparent)

;; Documentation slot. At most one `---` precedes it, and only when earlier
;; slots are also present. Body may be empty when only the online link should
;; show; link comes before a long body.
(struct/contract Hover-Documentation
  ([body string?]
   [link (or/c string? #f)])
  #:transparent)

;; Fixed slots. Use #f to omit a slot. Documentation may be link-only with an
;; empty body.
;; Struct field order is not render order: `note` sits before `documentation`
;; here, but render places documentation before note (see module header).
;; `type-stale?` matters only when `type` is present.
;; `note` holds check-syntax mouse-over text. It is kept whenever non-empty,
;; shown before the online-docs link when docs are present, and appended after
;; earlier slots otherwise.
(struct/contract Hover-Card
  ([type (or/c Hover-Code-Summary? #f)]
   [type-stale? boolean?]
   [definition (or/c Hover-Definition? #f)]
   [note (or/c string? #f)]
   [documentation (or/c Hover-Documentation? #f)])
  #:transparent)

;; Map hover inputs onto the fixed renderer slots.
;; Same-file source wins over a docs signature.
;; Check-syntax text is kept whenever it is a non-empty string. With a docs
;; link, it renders before `[Online docs]`; otherwise it follows earlier
;; slots. Do not classify its strings here.
;; `#:link` is the final online URL (caller rewrites local docs paths).
(define (build-hover-card #:type-text type-text
                          #:type-stale? type-stale?
                          #:hover-text hover-text
                          #:link link
                          #:signature signature
                          #:source-summary source-summary
                          #:documentation-text documentation-text)
  (define type
    (and type-text
         (Hover-Code-Summary type-text "racket")))
  (define definition
    (cond
      [source-summary
       (Hover-Definition 'source source-summary)]
      [signature
       (Hover-Definition 'signature
                         (Hover-Code-Summary signature "racket"))]
      [else #f]))
  (define documentation
    (and link
         (Hover-Documentation documentation-text link)))
  (define note
    (and (non-empty-text? hover-text) hover-text))
  (Hover-Card type
              ;; Caller sets type-stale? for the whole document; attach it only
              ;; when a type fence exists so note-only cards stay unmarked.
              (and type type-stale?)
              definition
              note
              documentation))

(define (non-empty-text? text)
  (and (string? text)
       (not (string=? text ""))))

(define (render-code-summary summary)
  (define text (Hover-Code-Summary-text summary))
  (define fence-language (Hover-Code-Summary-fence-language summary))
  (and (non-empty-text? text)
       (format "```~a\n~a\n```" fence-language text)))

;; Tight label-to-fence pairing. Blank lines belong between slots, not here.
(define (render-labeled-fence label summary)
  (define fenced (render-code-summary summary))
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

;; Label only when a type fence precedes this slot. A lone definition fence
;; stays unlabeled so it matches the old single-fence hover layout; two
;; adjacent fences need labels.
(define (render-definition definition #:label? label?)
  (and-let* (definition
              [summary (Hover-Definition-summary definition)])
    (if label?
        (render-labeled-fence (definition-label (Hover-Definition-kind definition))
                              summary)
        (render-code-summary summary))))

(define (render-note note)
  (and (non-empty-text? note)
       note))

(define (render-documentation documentation #:note [note #f])
  (cond
    [(not documentation) #f]
    [else
     (define body (Hover-Documentation-body documentation))
     (define link (Hover-Documentation-link documentation))
     ;; Mouse-over provenance, then the link, then a long excerpt.
     (define header
       (and (non-empty-text? link)
            (format "[Online docs](~a)" link)))
     (define parts
       (append (if (render-note note)
                   (list note)
                   '())
               (if header
                   (list header)
                   '())
               (if (non-empty-text? body)
                   (list body)
                   '())))
     (and (pair? parts)
          (string-join parts "\n\n"))]))

;; Chosen over checking raw fields: documentation may be link-only with an
;; empty body, and empty fences must not count as content.
(define (hover-card-has-content? card)
  (or (Hover-Card-type card)
      (Hover-Card-definition card)
      (non-empty-text? (Hover-Card-note card))
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
    (render-definition (Hover-Card-definition card)
                       #:label? (and type #t)))
  (define note
    (render-note (Hover-Card-note card)))
  (define documentation
    (render-documentation (Hover-Card-documentation card)
                          #:note (and (Hover-Card-documentation card) note)))
  (define before-docs
    (string-join
      (filter non-empty-text?
              (list type definition))
      "\n\n"))
  ;; At most one `---`, only before docs, and only when both sides are non-empty.
  (define with-docs
    (cond
      [(not documentation) before-docs]
      [(string=? before-docs "") documentation]
      [else
       (string-append before-docs
                      "\n\n---\n\n"
                      documentation)]))
  (cond
    [(not note) with-docs]
    [(Hover-Card-documentation card) with-docs]
    [(string=? with-docs "") note]
    [else
     (string-append with-docs "\n\n" note)]))
