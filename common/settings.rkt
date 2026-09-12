#lang racket/base

(require racket/contract)

(provide get-resyntax-enabled
         set-resyntax-enabled!
         (struct-out Formatting-Settings)
         default-formatting-settings
         current-formatting-settings
         set-formatting-settings!)

(define resyntax-enabled? #t)

(define (get-resyntax-enabled)
  resyntax-enabled?)

(define (set-resyntax-enabled! val)
  (set! resyntax-enabled? val))

;; Document formatting may reflow a complete buffer, while indentation
;; formatting must remain safe for ranges and on-type requests. Keep the two
;; selections distinct instead of silently substituting one for the other.
(struct/contract Formatting-Settings
  ([document-formatter (or/c 'fixw 'drracket 'fmt)]
   [indentation-formatter (or/c 'fixw 'drracket)])
  #:transparent)

(define default-formatting-settings
  (Formatting-Settings 'fixw 'fixw))

;; Process-wide, like `resyntax-enabled?`.
(define current-formatting-settings default-formatting-settings)

(define/contract (set-formatting-settings! settings)
  (-> Formatting-Settings? void?)
  (set! current-formatting-settings settings))
