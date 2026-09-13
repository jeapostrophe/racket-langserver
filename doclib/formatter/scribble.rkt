#lang racket/base

;; GUI-free port of Scribble's `determine-spaces` from
;; `scribble/private/indentation` in gui-lib.  That official hook
;; requires `racket/gui/base` and `framework`; loading it would force a
;; display (or xvfb) on the language server. The algorithm only needs
;; `color-textoid<%>` and the local `racket-up-sexp` equivalent below.

(require racket/class
         (only-in syntax-color/scribble-lexer
                  scribble-inside-lexer
                  scribble-lexer))

(provide scribble-surface-language?
         scribble-determine-spaces)

(define (scribble-surface-language? info)
  (and (procedure? info)
       (with-handlers ([exn:fail? (lambda (_exn) #f)])
         (or (equal? (info 'drracket:default-extension #f) "scrbl")
             (scribble-color-lexer? (info 'color-lexer #f))))))

(define (scribble-color-lexer? lexer)
  (and (procedure? lexer)
       (or (eq? lexer scribble-inside-lexer)
           (eq? lexer scribble-lexer))))

(define (racket-up-sexp textoid start-position)
  (define expression-position
    (send textoid backward-containing-sexp start-position 0))
  (and expression-position
       (let ([inside-start
              (send textoid skip-whitespace expression-position 'backward #t)])
         (define-values (start _end)
           (send textoid get-token-range (sub1 inside-start)))
         start)))

(define (scribble-determine-spaces txt posi)
  (define current-para (send txt position-paragraph posi))
  (define para-start-skip-space (start-skip-spaces txt current-para 'forward))
  (cond
    [para-start-skip-space
     (define char-classify (send txt classify-position para-start-skip-space))
     (define prev-posi (racket-up-sexp txt para-start-skip-space))
     (cond
       [prev-posi
        (define this-para (send txt position-paragraph prev-posi))
        (cond
          [(equal? #\[ (send txt get-character prev-posi))
           (define this-para-start (send txt paragraph-start-position this-para))
           (if (= current-para this-para)
               0
               (if (rest-empty? txt this-para prev-posi)
                   1
                   (add1 (- prev-posi this-para-start))))]
          ;; Inside a Racket function and not on its first line: official
          ;; `determine-spaces` returns #f so DrRacket's `or` rule can use
          ;; `racket-amount-to-indent`. This backend does the same.
          [(equal? #\( (send txt get-character prev-posi)) #f]
          [else
           (define curleys
             (number-of-curley-braces-if-there-are-only-curley-braces txt current-para))
           (if curleys
               (max 0 (- (count-parens txt prev-posi) curleys))
               (count-parens txt prev-posi))])]
       [(equal? 'text char-classify) 0]
       [else #f])]
    [else #f]))

(define (txt-position-classify txt start end)
  (for/list ([x (in-range start end 1)])
    (send txt classify-position x)))

(define (para-not-empty? classify-lst)
  (and (or (member 'parenthesis classify-lst)
           (member 'string classify-lst)
           (member 'symbol classify-lst)
           (member 'text classify-lst))
       #t))

(define (rest-empty? txt line start)
  (define line-start (add1 start))
  (define line-end (send txt paragraph-end-position line))
  (define line-classify (txt-position-classify txt line-start line-end))
  (not (para-not-empty? line-classify)))

(define (start-skip-spaces txt para direction)
  (define para-start (send txt paragraph-start-position para))
  (define para-end (send txt paragraph-end-position para))
  (if (equal? direction 'forward)
      (for/first ([start-skip-space (in-range para-start para-end 1)]
                  #:unless (memq (send txt get-character start-skip-space) '(#\space #\tab)))
        start-skip-space)
      (for/first ([start-skip-space (in-range (sub1 para-end) para-start -1)]
                  #:unless (memq (send txt get-character start-skip-space) '(#\space #\tab)))
        start-skip-space)))

(define (number-of-curley-braces-if-there-are-only-curley-braces txt para)
  (define-values (only-curleys? count)
    (for/fold ([only-curleys? #t] [count 0])
              ([p (in-range (send txt paragraph-start-position para)
                            (send txt paragraph-end-position para))])
      #:break (not only-curleys?)
      (define c (send txt get-character p))
      (cond
        [(char-whitespace? c) (values #t count)]
        [(equal? c #\}) (values #t (add1 count))]
        [else (values #f count)])))
  (and only-curleys? count))

(define (count-parens txt posi)
  (let loop ([p posi] [count 0])
    (cond
      [(not p) count]
      [(equal? #\{ (send txt get-character p))
       (loop (racket-up-sexp txt p) (add1 count))]
      [(equal? #\[ (send txt get-character p))
       (define this-para (send txt position-paragraph p))
       (define this-para-start (send txt paragraph-start-position this-para))
       (loop (racket-up-sexp txt p)
             (if (rest-empty? txt this-para p)
                 (add1 count)
                 (+ (add1 (- p this-para-start)) count)))]
      [else (loop (racket-up-sexp txt p) count)])))
