#lang racket/base

;; Check-syntax hover text and same-file source detail for hover cards.
;;
;; Both live in one collection for one trace snapshot.
;;
;; Do not build hover cards here. `doc-text` is frozen at expansion time.
;; Live buffer reads, freshness checks, and use-to-declaration lookup belong
;; in `doc-hover`. This service does not call other services.
;;
;; Hot path: `annotation-at` and `source-detail-at` only. Syntax walks, lex,
;; and comment scanning run when the trace is built, not on each hover request.

(require "../interface.rkt"
         "../../internal-types.rkt"
         "types.rkt"
         "detail.rkt"
         "../position-journal.rkt"
         racket/class
         racket/set
         data/interval-map
         drracket/check-syntax
         (only-in "../../lexer.rkt"
                  LexerState-language-policy
                  Language-Policy-policy-language
                  Language-Policy-body-mode)
         srfi/2)

(provide hover%
         (struct-out Hover-Annotation)
         (struct-out Hover-Comment-Line)
         (struct-out Hover-Detail))

;; The interval map splits overlaps into atomic segments. Keep the producer's
;; original span width in each value so a narrower annotation wins every
;; overlap; equal spans keep the later annotation.
(struct Stored-Hover-Annotation
  (annotation source-span)
  #:transparent)

(define hover%
  (class base-service%
    (init-field src doc-text lexer-state)
    (super-new)

    ;; Character range -> one winning hover annotation.
    (define mouse-over-by-range (make-interval-map))
    ;; Identifier range -> same-file `Hover-Detail` at declaration ranges.
    ;; `doc-hover` finds uses. We do not copy detail onto every use.
    (define detail-by-range (make-interval-map))
    ;; Edit log for ranges stored inside each `Hover-Detail` (not map keys).
    ;; Record on expand/contract. Replay only in `source-detail-at`.
    (define detail-journal (make-position-journal))
    ;; Definition targets with no uses; used in `walk-stx`.
    (define definition-target-ranges (mutable-set))
    ;; Local declaration ranges from non-require arrows (no duplicates).
    (define local-declaration-ranges (mutable-set))
    ;; Cache comment lookup by display-start so shared forms scan once.
    ;; Filled only during `walk-stx`. Edits replay stored comment ranges.
    (define leading-comments-by-display-start (make-hash))
    ;; From lexer policy at build time. Chooses candidates and fence language.
    (define source-kind
      (let ([policy (LexerState-language-policy lexer-state)])
        (cond
          [(eq? (Language-Policy-policy-language policy) 'rhombus) 'rhombus]
          [(eq? (Language-Policy-body-mode policy) 'sexp) 'sexp]
          [else 'none])))
    (define fence-language
      (if (eq? source-kind 'rhombus) "rhombus" "racket"))

    ;; Do not expose interval-maps. Bounds are the atomic range over which this
    ;; exact winner remains active.
    (define/public (annotation-at pos)
      (define-values (start end stored-annotation)
        (interval-map-ref/bounds mouse-over-by-range pos #f))
      (values start
              end
              (and stored-annotation
                   (Stored-Hover-Annotation-annotation stored-annotation))))

    ;; Do not expose interval-maps. Returns start, end, Hover-Detail, or #f #f #f.
    ;; Replay may return #f when a deletion broke the detail. Treat that as a miss.
    (define/public (source-detail-at pos)
      (define-values (start end stored-detail)
        (interval-map-ref/bounds detail-by-range pos #f))
      (define detail
        (and stored-detail
             (position-journal-replay detail-journal
                                      stored-detail
                                      expand-detail
                                      contract-detail)))
      (if detail
          (values start end detail)
          (values #f #f #f)))

    (define/override (reset)
      (set! mouse-over-by-range (make-interval-map))
      (set! detail-by-range (make-interval-map))
      ;; Clear the journal with the maps. Values belong to one snapshot.
      (position-journal-reset! detail-journal)
      (set! definition-target-ranges (mutable-set))
      (set! local-declaration-ranges (mutable-set))
      (set! leading-comments-by-display-start (make-hash)))

    ;; Detail values hold absolute ranges. Move map keys now. Journal the
    ;; ranges inside each detail so `source-detail-at` can replay them later.
    (define/override (expand start end)
      (interval-map-expand! mouse-over-by-range start end)
      (interval-map-expand! detail-by-range start end)
      (position-journal-record-expand! detail-journal start end))

    (define/override (contract start end)
      (interval-map-contract! mouse-over-by-range start end)
      (interval-map-contract! detail-by-range start end)
      (position-journal-record-contract! detail-journal start end))

    (define/override (syncheck:add-mouse-over-status _src start end text)
      ;; When start = end, check-syntax had no source span. Skip it so we do
      ;; not create a zero-width hover range.
      (when (< start end)
        (store-annotation! 'mouse-over-status start end text)))

    (define/override (add-log-tooltip _src start end text)
      (when (< start end)
        (store-annotation! 'log-tooltip start end text)))

    (define/private (store-annotation! kind start end text)
      (define source-span
        (- end start))
      (define candidate
        (Stored-Hover-Annotation
          (Hover-Annotation kind text)
          source-span))
      (interval-map-update*!
        mouse-over-by-range
        start
        end
        (lambda (current)
          (if (<= source-span
                  (Stored-Hover-Annotation-source-span current))
              candidate
              current))
        candidate))

    (define/override (syncheck:add-definition-target/phase-level+space
                       src-obj start end _id _submods _phase+space)
      (when (and (equal? src src-obj)
                 (< start end))
        (set-add! definition-target-ranges (cons start end))))

    (define/override (syncheck:add-arrow/name-dup _start-src start-left start-right
                                                  _end-src _el _er
                                                  _actual? _phase require-arrow? _dup?)
      ;; Non-require arrows mark same-file local declarations that need detail.
      ;; Skip zero-width starts. Do not widen them.
      (when (and (not require-arrow?)
                 (< start-left start-right))
        (set-add! local-declaration-ranges (cons start-left start-right))))

    (define/override (walk-stx expand-result)
      (define pre-stx (ExpandResult-pre-syntax expand-result))
      (when (and pre-stx (not (eq? source-kind 'none)))
        (define candidates
          (source-candidates pre-stx
                             src
                             (send doc-text end-pos)
                             source-kind))
        ;; Definition targets: module-level decls with no uses.
        ;; Arrow starts: local decls in let and clauses.
        (for ([range (in-set definition-target-ranges)])
          (store-detail-for-identifier! candidates range))
        (for ([range (in-set local-declaration-ranges)])
          (store-detail-for-identifier! candidates range))))

    (define/private (store-detail-for-identifier! candidates identifier-range)
      (define detail
        (detail-for-identifier candidates identifier-range))
      (when detail
        (interval-map-set!
          detail-by-range
          (car identifier-range)
          (cdr identifier-range)
          detail)))

    (define/private (detail-for-identifier candidates identifier-range)
      (define-values (source-range nearest-range)
        (ranges-for-identifier candidates doc-text identifier-range))
      (and-let* (source-range nearest-range)
        (let ()
          (define-values (display-start display-end)
            (display-range-for-identifier doc-text
                                          identifier-range
                                          source-range
                                          nearest-range))
          (define-values (comment-lines comments-truncated?)
            (comments-for-display display-start))
          (Hover-Detail comment-lines
                        comments-truncated?
                        (car source-range)
                        display-start
                        display-end
                        (cdr source-range)
                        fence-language))))

    (define/private (comments-for-display display-start)
      (define cached
        (hash-ref leading-comments-by-display-start display-start #f))
      (cond
        [cached
         (values (car cached) (cdr cached))]
        [else
         (define-values (comment-lines comments-truncated?)
           (leading-comment-lines lexer-state display-start))
         (hash-set! leading-comments-by-display-start
                    display-start
                    (cons comment-lines comments-truncated?))
         (values comment-lines comments-truncated?)]))

    ))
