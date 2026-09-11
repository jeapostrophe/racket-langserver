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

;; Whether an edit changed the text inside `range`. An insert at either end of
;; a form adds text next to it, not inside it, so only a strictly interior one
;; counts; a delete counts as soon as it takes any of the form away.
(define (insert-disturbs? range start _end)
  (and (< (CharRange-start range) start)
       (< start (CharRange-end range))))

(define (delete-disturbs? range start end)
  (and (< start (CharRange-end range))
       (< (CharRange-start range) end)))

(define (edit-group group disturbs? move-position move-range start end)
  (define sources (Inlay-Hint-Group-sources group))
  (and (not (for/or ([range (in-list sources)])
              (disturbs? range start end)))
       (Inlay-Hint-Group
         (for/list ([range (in-list sources)])
           (move-range range start end))
         (for/list ([anchor (in-list (Inlay-Hint-Group-anchors group))])
           (struct-copy Inlay-Hint-Anchor anchor
             [pos (move-position (Inlay-Hint-Anchor-pos anchor) start end)])))))

(define (edit-groups groups disturbs? move-position move-range start end)
  (for*/list ([group (in-list groups)]
              [edited (in-value (edit-group group disturbs? move-position move-range start end))]
              #:when edited)
    edited))

(define (expand-groups groups start end)
  (edit-groups groups insert-disturbs? expand-position expand-char-range start end))

(define (contract-groups groups start end)
  (edit-groups groups delete-disturbs? contract-position contract-char-range start end))

(define inlay-hint%
  (class base-service%
    (init-field lexer-state declaration typed-racket)
    (super-new)

    (define pre-syntax #f)
    (define groups '())

    ;; The hints drawn in [req-start, req-end], in the order they were read.
    (define/public (hints-in-range req-start req-end)
      (for*/list ([group (in-list groups)]
                  [anchor (in-list (Inlay-Hint-Group-anchors group))]
                  #:when (and (<= req-start (Inlay-Hint-Anchor-pos anchor) req-end)
                              (< req-end (Inlay-Hint-Anchor-pos anchor))))
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

    (define/override (expand start end)
      (set! groups (expand-groups groups start end)))

    (define/override (contract start end)
      (set! groups (contract-groups groups start end)))))
