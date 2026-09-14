#lang racket/base

;; GUI-free port of Scribble's `determine-spaces` from
;; `scribble/private/indentation` in gui-lib.  That official hook
;; requires `racket/gui/base` and `framework`; loading it would force a
;; display (or Xvfb) on the language server. The algorithm only needs
;; `color-textoid<%>` and the local `racket-up-sexp` equivalent below.

(require racket/class
         (only-in syntax-color/scribble-lexer
                  scribble-inside-lexer
                  scribble-lexer))

(provide scribble-surface-language?
         scribble-determine-spaces)

;;; Language identification

(define (scribble-surface-language? info)
  (and (procedure? info)
       (with-handlers ([exn:fail? (lambda (_exn) #f)])
         (or (equal? (info 'drracket:default-extension #f) "scrbl")
             (scribble-color-lexer? (info 'color-lexer #f))))))

;; Language get-info returns `contract-out` impersonators of these lexers.
;; `equal?` matches them; `eq?` does not, so `#lang at-exp` would miss.
(define (scribble-color-lexer? lexer)
  (or (equal? lexer scribble-inside-lexer)
      (equal? lexer scribble-lexer)))

;;; determine-spaces

(define (scribble-determine-spaces textoid position)
  (define paragraph (send textoid position-paragraph position))
  (define content-start (paragraph-content-start textoid paragraph))
  (and content-start
       (let ([opener-position (racket-up-sexp textoid content-start)])
         (cond
           [opener-position (indentation-inside textoid paragraph opener-position)]
           [(eq? 'text (send textoid classify-position content-start)) 0]
           [else #f]))))

(define (indentation-inside textoid paragraph opener-position)
  (case (send textoid get-character opener-position)
    [(#\[)
     (if (= paragraph (send textoid position-paragraph opener-position))
         0
         (bracket-indentation textoid opener-position))]
    ;; Decline embedded Racket so DrRacket's `or` rule uses the standard indenter.
    [(#\() #f]
    [else
     (define closing-braces (closing-brace-count textoid paragraph))
     (define indentation (enclosing-indentation textoid opener-position))
     (if closing-braces
         (max 0 (- indentation closing-braces))
         indentation)]))

(define (bracket-indentation textoid position)
  (define paragraph (send textoid position-paragraph position))
  (if (rest-empty? textoid paragraph position)
      1
      (add1 (- position (send textoid paragraph-start-position paragraph)))))

;;; Paragraph content and enclosing expressions

(define (racket-up-sexp textoid start-position)
  (define expression-position
    (send textoid backward-containing-sexp start-position 0))
  (cond
    [expression-position
     (define inside-start
       (send textoid skip-whitespace expression-position 'backward #t))
     (define-values (start _end)
       (send textoid get-token-range (sub1 inside-start)))
     start]
    [else #f]))

(define (rest-empty? textoid line start)
  (for/and ([position (in-range (add1 start) (send textoid paragraph-end-position line))])
    (not (memq (send textoid classify-position position)
               '(parenthesis string symbol text)))))

(define (paragraph-content-start textoid paragraph)
  (define start (send textoid paragraph-start-position paragraph))
  (define end (send textoid paragraph-end-position paragraph))
  (for/first ([position (in-range start end)]
              #:unless (memq (send textoid get-character position)
                             '(#\space #\tab)))
    position))

;; Return #f if the paragraph contains anything except whitespace and `}`.
(define (closing-brace-count textoid paragraph)
  (for/fold ([count 0])
            ([position (in-range (send textoid paragraph-start-position paragraph)
                                 (send textoid paragraph-end-position paragraph))]
             #:break (not count))
    (define character (send textoid get-character position))
    (cond
      [(char-whitespace? character) count]
      [(char=? character #\}) (add1 count)]
      [else #f])))

(define (enclosing-indentation textoid position)
  (let loop ([position position] [indentation 0])
    (cond
      [(not position) indentation]
      [else
       (define contribution
         (case (send textoid get-character position)
           [(#\{) 1]
           [(#\[) (bracket-indentation textoid position)]
           [else 0]))
       (loop (racket-up-sexp textoid position) (+ indentation contribution))])))
