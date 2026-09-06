#lang racket/base

;; What a language publishes to have inlay hints, and what it gets back.
;;
;; A hint source reads the pre-expand syntax of a document once per analysis
;; and returns the hints it found. Positions are absolute character offsets in
;; the text that syntax was read from.

(provide (struct-out Inlay-Hint-Context)
         (struct-out Inlay-Hint-Anchor)
         (struct-out Inlay-Hint-Group)
         inlay-hint-source/c)

(require racket/contract
         "../common/interfaces.rkt")

;; What a hint source may ask about the document it is reading. Both lookups take an
;; absolute character offset. `inferred-type-at` answers (values start end
;; text), with text #f where the checker published none.
(struct/contract Inlay-Hint-Context
  ([definition-at (-> exact-nonnegative-integer? (or/c #f CharRange?))]
   [inferred-type-at (-> exact-nonnegative-integer? any)])
  #:transparent)

(struct/contract Inlay-Hint-Anchor
  ([pos exact-nonnegative-integer?]
   [kind InlayHintKind?]
   [label string?]
   [tooltip string?])
  #:transparent)

;; Hints and the spans they were read from. An edit that changes what one of
;; those spans holds drops the whole group: the text no longer says what its
;; hints say. An edit anywhere else only moves them.
(struct/contract Inlay-Hint-Group
  ([sources (listof CharRange?)]
   [anchors (listof Inlay-Hint-Anchor?)])
  #:transparent)

(define inlay-hint-source/c
  (-> Inlay-Hint-Context? syntax? (listof Inlay-Hint-Group?)))
