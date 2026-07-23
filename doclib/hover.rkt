#lang racket/base

;; A hover card is the renderer-facing description of an LSP hover. Producers
;; supply semantic values; this module owns their Markdown layout.
;;
;; Keep this renderer data-only: `doc-hover` gathers check-syntax and
;; documentation data before calling it, so new hover facts share one layout
;; without adding lookup work to rendering.
;;
;; Layout order is fixed: summary, metadata, facts, then documentation.
;; Later producers fill metadata/facts; do not invent ad hoc string
;; concatenation around this model (see tests/lib/doc-test.rkt hover cases).
;;
;; Example:
;;   (Hover-Card
;;    (Hover-Code-Summary "(parse-config raw)")
;;    (list "Workspace binding" "config.rkt:12" "phase 0")
;;    (list (Hover-Fact "Type" "(-> string? config?)")
;;          (Hover-Fact "Contract" "(-> string? config?)"))
;;    (Hover-Documentation "Parse a configuration value."
;;                         "https://docs.example.test/parse-config"))
;; renders as:
;;   ```racket
;;   (parse-config raw)
;;   ```
;;
;;   Workspace binding | config.rkt:12 | phase 0
;;
;;   Type: (-> string? config?)
;;   Contract: (-> string? config?)
;;
;;   ---
;;
;;   Documentation - [Online docs](https://docs.example.test/parse-config)
;;
;;   Parse a configuration value.

(require racket/contract
         racket/string)

(provide (struct-out Hover-Card)
         (struct-out Hover-Code-Summary)
         (struct-out Hover-Prose-Summary)
         (struct-out Hover-Fact)
         (struct-out Hover-Documentation)
         render-hover-card)

;; Signature or workspace definition snippet rendered as a ```racket fence.
;; Empty text drops the whole summary section at render time.
(struct/contract Hover-Code-Summary
  ([text string?])
  #:transparent)

;; Plain syncheck mouse-over status when no code summary is available.
;; Empty text drops the whole summary section at render time.
(struct/contract Hover-Prose-Summary
  ([text string?])
  #:transparent)

;; One labeled semantic fact, rendered as `Label: value`.
;; Either field empty drops the fact (no orphan labels).
(struct/contract Hover-Fact
  ([label string?]
   [value string?])
  #:transparent)

;; Docs section after the optional `---` separator.
;; Empty body is fine when only the online link should appear.
;; Prefer putting the link in the header, not after a long body.
(struct/contract Hover-Documentation
  ([body string?]
   [link (or/c string? #f)])
  #:transparent)

;; Summary is exactly one of code, prose, or absent. Never put both a code
;; fence and prose summary in one card.
;;
;; `summary` — primary reading cue. Prefer code (signature or definition
;;   snippet); prose is raw syncheck status when no richer block exists.
;; `metadata` — short provenance/phase tokens joined into one pipe-separated
;;   line. Empty strings are omitted at render time so partial producers are safe.
;; `facts` — labeled semantic lines (type, contract, …) in producer order.
;; `documentation` — last section; omit entirely when neither body nor online
;;   link is useful.
(struct/contract Hover-Card
  ([summary (or/c Hover-Code-Summary? Hover-Prose-Summary? #f)]
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
     (and (non-empty-text? text)
          (format "```racket\n~a\n```" text))]
    [(Hover-Prose-Summary? summary)
     (define text (Hover-Prose-Summary-text summary))
     (and (non-empty-text? text)
          text)]
    [else
     (raise-argument-error 'render-hover-card
                           "(or/c Hover-Code-Summary? Hover-Prose-Summary? #f)"
                           summary)]))

;; Metadata is one pipe-joined line so provenance/phase stay scannable beside
;; the summary rather than competing with docs. Drop empty entries so a partial
;; producer does not leave dangling separators.
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
     ;; Keep the link in the section header. Documentation excerpts can be long,
     ;; so placing it after the body makes the navigation affordance easy to miss.
     (define header
       (cond
         [(non-empty-text? link)
          (format "Documentation - [Online docs](~a)" link)]
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
  ;; Invariant: at most one `---` separator, and only before a docs section.
  ;; Do not insert bars between summary/metadata/facts; empty docs must omit
  ;; the separator entirely (pinned by "Hover card omits empty sections...").
  (cond
    [(not documentation) non-doc-contents]
    [(string=? non-doc-contents "") documentation]
    [else
     (string-append non-doc-contents
                    "\n\n---\n\n"
                    documentation)]))
