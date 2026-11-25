#lang racket/base

;; Named hover-detail values from the hover service.
;; Do not expose raw interval-maps from hover%.
;;
;; Store absolute ranges only. Do not copy snippet text. `doc-hover` reads the
;; live buffer from those ranges, even when a trace is old. The snippet can be
;; wrong or incomplete. The binding link may also be wrong. Edits move ranges
;; through `position-journal` and expand/contract-detail. We do not rebuild each
;; detail on every edit.
;;
;; Offsets are 0-based, absolute, and end-exclusive. Details are stored at
;; declaration ranges. `doc-hover` finds uses through `occurrence-at`.

(require racket/contract)

(provide (struct-out Hover-Annotation)
         (struct-out Hover-Comment-Line)
         (struct-out Hover-Detail))

;; One winning hover annotation. `kind` is the ingestion mechanism
;; (`mouse-over-status` or `log-tooltip`) so the renderer can label it.
(struct/contract Hover-Annotation
  ([kind (or/c 'mouse-over-status 'log-tooltip)]
   [text string?])
  #:transparent)

;; Own-line comment above the display window.
;; Ranges are recorded when the trace is built. `doc-hover` reads [start, end)
;; from the live buffer. `truncated?` is true when the line hit the per-line
;; character limit.
(struct/contract Hover-Comment-Line
  ([start exact-nonnegative-integer?]
   [end exact-nonnegative-integer?]
   [truncated? boolean?])
  #:transparent)

;; Same-file detail for one local declaration. Stored at the declaration range.
;; `doc-hover` finds uses. We do not copy this detail onto every use.
;;
;; The nearest enclosing form is the smallest snippet. We add more context only
;; when it stays on one line (for example a same-line header).
;;
;; `source-*`: outer bound (largest same-line form, else nearest form).
;; `display-*`: what we show (often just the declaration line).
;; Rule: source-start <= display-start < display-end <= source-end.
;;
;; `comment-lines`: own-line comments directly above display-start.
;; `comments-truncated?`: more comment lines than the limit allows.
;; `fence-language`: "racket" or "rhombus".
(struct/contract Hover-Detail
  ([comment-lines (listof Hover-Comment-Line?)]
   [comments-truncated? boolean?]
   [source-start exact-nonnegative-integer?]
   [display-start exact-nonnegative-integer?]
   [display-end exact-nonnegative-integer?]
   [source-end exact-nonnegative-integer?]
   [fence-language string?])
  #:transparent)
