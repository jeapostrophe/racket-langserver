#lang racket/base

;; With line counting on, Racket reads "\r\n" as one position, while the editor
;; counts it as two characters, so each position after a CRLF would be one
;; character early for every CRLF before it. A port from `open-input-text`
;; keeps Racket's lines and columns and reports each position as the character
;; offset plus one, so syntax, srcloc and lexer positions are editor positions.

(require racket/port
         racket/string)

(provide open-input-text)

(define (open-input-text text)
  (define in (open-input-string text))
  (port-count-lines! in)
  ;; Without a CRLF, Racket's positions already count characters.
  (if (string-contains? text "\r\n")
      (count-positions-in-characters in text)
      in))

(define (count-positions-in-characters in text)
  ;; `file-position` counts the UTF-8 bytes read so far, and `chars-read` is
  ;; the number of characters they encode. Reading only moves forward.
  (define chars-read 0)
  (define bytes-read 0)
  (define (get-location)
    (define-values (line column _position) (port-next-location in))
    (define end (file-position in))
    (let loop ()
      (when (< bytes-read end)
        (set! bytes-read (+ bytes-read (char-utf-8-length (string-ref text chars-read))))
        (set! chars-read (add1 chars-read))
        (loop)))
    (values line column (add1 chars-read)))
  (define port (transplant-input-port in get-location 1))
  (port-count-lines! port)
  port)
