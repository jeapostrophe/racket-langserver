#lang racket/base

;; Inlay hints for one document.
;;
;; The hints are read once, from the pre-expand syntax of the last analysis,
;; and then kept: a request is answered from what was read, without walking
;; the document again.
;;
;; An edit shifts the hints it left alone and drops the ones whose own text it
;; changed, so what stays on screen still describes the text under it. The
;; dropped ones come back with the next analysis.

(require racket/class
         racket/list
         "interface.rkt"
         "char-range-edit.rkt"
         "../inlay-hint-source.rkt"
         "../internal-types.rkt"
         (only-in "../lexer.rkt"
                  LexerState-language-policy
                  Language-Policy-inlay-hints)
         "../../common/interfaces.rkt")

(provide inlay-hint%)

;; Whether replacing the text in [start, end) changed the text inside `range`.
;; A replace reaches inside a form as soon as the two overlap. An insert is the
;; empty interval, and it adds text next to a form rather than inside it, so
;; only a strictly interior one counts.
(define (replace-disturbs? range start end)
  (if (= start end)
      (and (< (CharRange-start range) start)
           (< start (CharRange-end range)))
      (and (< start (CharRange-end range))
           (< (CharRange-start range) end))))

(define (drop-disturbed-groups groups start end)
  (for/list ([group (in-list groups)]
             #:unless (for/or ([range (in-list (Inlay-Hint-Group-sources group))])
                        (replace-disturbs? range start end)))
    group))

;; The group with every position it holds moved, or #f when a source collapsed
;; to nothing and it has no text left to describe.
(define (move-group group move-position move-range start end)
  (define sources
    (for/list ([range (in-list (Inlay-Hint-Group-sources group))])
      (move-range range start end)))
  (and (andmap values sources)
       (Inlay-Hint-Group
         sources
         (for/list ([anchor (in-list (Inlay-Hint-Group-anchors group))])
           (struct-copy Inlay-Hint-Anchor anchor
             [pos (move-position (Inlay-Hint-Anchor-pos anchor) start end)])))))

(define (move-groups groups move-position move-range start end)
  (for*/list ([group (in-list groups)]
              [moved (in-value (move-group group move-position move-range start end))]
              #:when moved)
    moved))

(define (expand-groups groups start end)
  (move-groups groups expand-position expand-char-range start end))

(define (contract-groups groups start end)
  (move-groups groups contract-position contract-char-range start end))

(define inlay-hint%
  (class base-service%
    (init-field lexer-state declaration typed-racket)
    (super-new)

    (define pre-syntax #f)
    (define groups '())

    ;; The hints drawn in [req-start, req-end), in the order they were read.
    (define/public (hints-in-range req-start req-end)
      (for*/list ([group (in-list groups)]
                  [anchor (in-list (Inlay-Hint-Group-anchors group))]
                  #:when (and (<= req-start (Inlay-Hint-Anchor-pos anchor))
                              (< (Inlay-Hint-Anchor-pos anchor) req-end)))
        anchor))

    (define/override (reset)
      (set! pre-syntax #f)
      (set! groups '()))

    (define/override (walk-stx expand-result)
      (set! pre-syntax (ExpandResult-pre-syntax expand-result)))

    ;; A hint source works in the coordinates of the syntax it is given and
    ;; asks other services for facts in those same coordinates, so this must
    ;; run once every service it reads is filled and before any edit moves a
    ;; position.
    (define/public (build!)
      (define sources
        (Language-Policy-inlay-hints (LexerState-language-policy lexer-state)))
      (set! groups
            (cond
              [(or (not pre-syntax) (empty? sources)) '()]
              [else
               (define context
                 (Inlay-Hint-Context
                   (lambda (pos) (send declaration definition-at pos))
                   (lambda (pos) (send typed-racket inferred-type-at pos))))
               (append*
                 (for/list ([source (in-list sources)])
                   (source context pre-syntax)))]))
      ;; Read once; holding a syntax tree per open document is not worth it.
      (set! pre-syntax #f))

    ;; `expand` and `contract` cannot say whether a hint's own text changed: a
    ;; replace of the same length changes no length at all, and a longer one
    ;; gains its characters at the end of the text it replaced, where typing
    ;; after that text would gain them too.
    (define/override (text-replaced start end)
      (set! groups (drop-disturbed-groups groups start end)))

    (define/override (expand start end)
      (set! groups (expand-groups groups start end)))

    (define/override (contract start end)
      (set! groups (contract-groups groups start end)))))
