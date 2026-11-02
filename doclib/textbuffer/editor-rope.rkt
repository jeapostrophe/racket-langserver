#lang racket/base

(provide lsp-editor%)

(require racket/class
         racket/file
         (only-in "../../common/interfaces.rkt"
                  char-utf16-length
                  string-utf16-length)
         "textbuffer-rope.rkt")

(define lsp-editor%
  (class object%
    (init [initial-buffer (rope-from-string "")])

    (define rope initial-buffer)

    (define/private (replace-range str start-pos end-pos)
      (set! rope (rope-replace rope start-pos end-pos str)))

    (define/private (line-count)
      (add1 (rope-offset->line rope (rope-chars rope))))

    (define/private (line-start-pos/raw line)
      (cond
        [(zero? line) 0]
        [(< line (line-count))
         (add1 (rope-line-end-offset rope (sub1 line)))]
        [else
         (error 'line/char->pos "line out of range: ~a" line)]))

    ;; insert str at start
    (define/public (insert str start)
      (replace-range str start start))

    ;; replace text at (range start end) with str
    (define/public (replace str start end)
      (replace-range str start end))

    (define/public (replace-in-line str line ch-st ch-ed)
      (send this replace str
            (send this line/char->pos line ch-st)
            (send this line/char->pos line ch-ed)))

    (define/public (delete start end)
      (replace-range "" start end))

    ;; remove all content
    (define/public (erase)
      (set! rope (rope-from-string "")))

    (define/public (load-file path)
      (set! rope (rope-from-string (file->string path))))

    ;; return current content in editor as string
    (define/public (get-text [start 0] [end 'eof])
      (rope-get-text rope start (if (eq? end 'eof) (send this end-pos) end)))

    ;; return the number of characters in editor
    ;; also the end of file position
    (define/public (end-pos)
      (rope-chars rope))

    ;; LSP positions count UTF-16 code units within a line, while the rope and
    ;; Racket syntax positions count characters. Each conversion scans its line,
    ;; so reading a position leaves the editor unchanged. A column inside a
    ;; surrogate pair means the start of its character. Past the end of a line,
    ;; one position is one code unit in both directions.
    (define/public (line/char->pos line char)
      (define start (line-start-pos/raw line))
      ;; Each character is at least one code unit, so `char` characters suffice.
      (define text
        (send this get-text start (min (+ start char) (send this line-end-pos line))))
      (let loop ([offset 0]
                 [units 0])
        (cond
          [(= offset (string-length text))
           (+ start offset (- char units))]
          [else
           (define next (+ units (char-utf16-length (string-ref text offset))))
           (if (> next char)
               (+ start offset)
               (loop (add1 offset) next))])))

    (define/public (pos->line/char pos)
      (define line (rope-offset->line rope pos))
      (define end (min pos (send this line-end-pos line)))
      (list line
            (+ (string-utf16-length (send this get-text (send this line-start-pos line) end))
               (- pos end))))

    (define/public (line-start-pos line)
      (rope-line-start-offset rope line))

    (define/public (line-end-pos line)
      (rope-line-end-offset rope line))

    ;; pos at which line?
    (define/public (at-line pos)
      (rope-offset->line rope pos))

    (define/public (copy)
      (new lsp-editor% [initial-buffer rope]))

    (define/public (get-char pos)
      (rope-ref rope pos))

    (define/public (get-line line)
      (send this get-text (send this line-start-pos line) (send this line-end-pos line)))

    (super-new)))

