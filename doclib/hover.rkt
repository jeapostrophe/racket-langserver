#lang racket/base

;; Data for a hover card. Other code fills the fields. This module turns them
;; into Markdown.
;;
;; Keep this module as data only. `doc-hover` collects check-syntax and docs,
;; then calls `render-hover-card`. All cards use the same layout.
;;
;; Section order is fixed: summary, metadata, facts, documentation.
;; Do not build hover strings outside this model
;; (see tests/lib/doc-test.rkt hover cases).

(require racket/contract
         racket/string)

(provide (struct-out Hover-Card)
         (struct-out Hover-Code-Summary)
         (struct-out Hover-Fact)
         (struct-out Hover-Documentation)
         render-hover-card)

;; Signature or source snippet inside a Markdown code fence.
;; If text is empty, skip the whole summary section when rendering.
(struct/contract Hover-Code-Summary
  ([text string?]
   [fence-language string?])
  #:transparent)

;; One labeled fact. Rendered as `Label: value`.
;; Skip the fact if label or value is empty.
(struct/contract Hover-Fact
  ([label string?]
   [value string?])
  #:transparent)

;; Documentation section. It can follow a `---` separator.
;; Body may be empty if only the online link should show.
;; Put the link before a long body.
(struct/contract Hover-Documentation
  ([body string?]
   [link (or/c string? #f)])
  #:transparent)

;; Summary is a fenced code block, or it is missing. Put other hover text in
;; `facts` or `metadata`. Do not use a second summary style.
;;
;; `summary`: fenced signature or source snippet. `doc-hover` puts check-syntax
;;   text in `facts`, not here.
;; `metadata`: short source labels joined with ` | `. Empty strings are
;;   dropped when rendering.
;; `facts`: labeled lines (type, contract, ...) in the order they were added.
;; `documentation`: last section. Skip it when body and link are both empty.
(struct/contract Hover-Card
  ([summary (or/c Hover-Code-Summary? #f)]
   [metadata (listof string?)]
   [facts (listof Hover-Fact?)]
   [documentation (or/c Hover-Documentation? #f)])
  #:transparent)

(define (non-empty-text? text)
  (and (string? text)
       (not (string=? text ""))))

(define (render-summary summary)
  (cond
    [(not summary) #f]
    [(Hover-Code-Summary? summary)
     (define text (Hover-Code-Summary-text summary))
     (define fence-language (Hover-Code-Summary-fence-language summary))
     (and (non-empty-text? text)
          (format "```~a\n~a\n```" fence-language text))]
    [else
     (raise-argument-error 'render-hover-card
                           "(or/c Hover-Code-Summary? #f)"
                           summary)]))

;; Join metadata with ` | ` next to the summary.
;; Drop empty entries so missing data does not leave extra separators.
(define (render-metadata metadata)
  (define entries
    (for/list ([entry (in-list metadata)]
               #:when (non-empty-text? entry))
      entry))
  (and (pair? entries)
       (string-join entries " | ")))

(define (render-facts facts)
  (define lines
    (for/list ([fact (in-list facts)]
               #:when (and (non-empty-text? (Hover-Fact-label fact))
                           (non-empty-text? (Hover-Fact-value fact))))
      (format "~a: ~a"
              (Hover-Fact-label fact)
              (Hover-Fact-value fact))))
  (and (pair? lines)
       (string-join lines "\n")))

(define (render-documentation documentation)
  (cond
    [(not documentation) #f]
    [else
     (define body (Hover-Documentation-body documentation))
     (define link (Hover-Documentation-link documentation))
     ;; Put the link before the body. A long excerpt can hide a link at the end.
     (define header
       (cond
         [(non-empty-text? link)
          (format "[Online docs](~a)" link)]
         [(non-empty-text? body) "Documentation"]
         [else #f]))
     (define parts
       (append (if header
                   (list header)
                   '())
               (if (non-empty-text? body)
                   (list body)
                   '())))
     (and (pair? parts)
          (string-join parts "\n\n"))]))

(define/contract (render-hover-card card)
  (-> Hover-Card? string?)
  (define summary (render-summary (Hover-Card-summary card)))
  (define metadata (render-metadata (Hover-Card-metadata card)))
  (define facts (render-facts (Hover-Card-facts card)))
  (define documentation
    (render-documentation (Hover-Card-documentation card)))
  (define non-doc-sections
    (filter non-empty-text?
            (list summary metadata facts)))
  (define non-doc-contents
    (string-join non-doc-sections "\n\n"))
  ;; Use at most one `---` separator, and only before the docs section.
  ;; Do not put separators between summary, metadata, and facts. Skip the
  ;; separator when docs are empty (see "Hover card omits empty sections...").
  (cond
    [(not documentation) non-doc-contents]
    [(string=? non-doc-contents "") documentation]
    [else
     (string-append non-doc-contents
                    "\n\n---\n\n"
                    documentation)]))
