#lang racket/base

;; A small, non-GUI implementation of syntax-color's `color-textoid<%>`.
;; DrRacket language indentation procedures deliberately depend on this
;; interface rather than on framework's text% class.  Keeping this adapter
;; here lets the language server run those procedures headlessly.
;;
;; This intentionally does not wrap the editor's rope: the formatter receives
;; a string, while syntax-color token classification and navigation dominate
;; this adapter. A rope wrapper would still need the same full-document lexer,
;; and `rope-ref` is O(log n) on the indent hot path.
;;
;; Content is a contiguous string so `get-character` stays O(1).
;; Sequential leading-whitespace edits recopy that string and shift later
;; token and paragraph offsets, rather than splitting into line-relative
;; columns. Line storage was tried and made indent slower.

(require racket/class
         racket/match
         (only-in srfi/2 and-let*)
         syntax-color/module-lexer
         "../lexer/snapshot.rkt")

(provide make-textoid
         make-textoid-from-lexer-snapshot
         textoid-replace-leading-whitespace
         textoid-replace-line-prefix!
         textoid-content)

;;; Tokens and the color-textoid interface

(struct token (attributes paren [start #:mutable] [end #:mutable])
  #:transparent)

(define (token-type the-token)
  (hash-ref (token-attributes the-token) 'type 'unknown))

(define fallback-color-textoid<%>
  (interface ()
    get-text
    get-character
    last-position
    position-paragraph
    paragraph-start-position
    paragraph-end-position
    skip-whitespace
    backward-match
    backward-containing-sexp
    forward-match
    classify-position
    classify-position*
    get-token-range
    get-backward-navigation-limit
    get-regions))

(define module-lexer*/maybe
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (dynamic-require 'syntax-color/module-lexer 'module-lexer*)))

;; This headless interface was extracted from framework into syntax-color-lib
;; 1.4. Keep older supported Racket releases loadable, while using the canonical
;; interface whenever it is installed so contracted language hooks recognize the
;; textoid object.
(define color-textoid<%>
  (with-handlers ([exn:fail? (lambda (_exn) fallback-color-textoid<%>)])
    (dynamic-require 'syntax-color/color-textoid 'color-textoid<%>)))

;;; Lexing and construction

(define default-paren-matches
  '((|(| |)|) (|[| |]|) (|{| |}|)))

(define (source-directory->path source-directory)
  (and (or (path? source-directory) (string? source-directory))
       (path->complete-path source-directory)))

(define (lex-content content source-directory)
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (define input (open-input-string content))
    (port-count-lines! input)
    (define lexer (or module-lexer*/maybe module-lexer))
    (parameterize ([current-directory
                    (or (source-directory->path source-directory)
                        (current-directory))])
      (let loop ([mode #f] [tokens '()])
        (define-values (_lexeme attributes paren start end _backup next-mode)
          (lexer input 0 mode))
        (define-values (type stored)
          (if (symbol? attributes)
              (values attributes (hasheq 'type attributes))
              (values (hash-ref attributes 'type 'unknown) attributes)))
        (cond
          [(eq? type 'eof)
           (list->vector (reverse tokens))]
          [(and (exact-integer? start)
                (exact-integer? end)
                (<= start end))
           ;; Lexer positions are one-based; textoid positions are zero-based.
           (loop next-mode
                 (cons (token stored paren (sub1 start) (sub1 end)) tokens))]
          [else
           ;; A malformed/custom lexer token is not useful to indentation.
           #f])))))

(define (line-starts content)
  (list->vector
    (cons 0
          (for/list ([character (in-string content)]
                     [index (in-naturals)]
                     #:when (char=? character #\newline))
            (add1 index)))))

(define (textoid-from-tokens content tokens paren-matches source-directory)
  (new textoid%
    [initial-content content]
    [initial-tokens tokens]
    [source-directory source-directory]
    [paren-matches (if (list? paren-matches) paren-matches default-paren-matches)]))

(define (make-textoid content #:source-directory [source-directory #f]
                      #:paren-matches [paren-matches default-paren-matches])
  (define tokens (lex-content content source-directory))
  (and tokens
       (textoid-from-tokens content tokens paren-matches source-directory)))

(define (snapshot-token-type type)
  (case type
    [(open-paren close-paren) 'parenthesis]
    [(quote quasiquote syntax-quote syntax-quasiquote) 'constant]
    [(unquote unquote-splicing syntax-unquote syntax-unquote-splicing
              lang-directive reader-directive)
     'other]
    [else type]))

(define (snapshot-token-paren content type start end)
  (define (last-symbol)
    (and (< start end)
         (string->symbol (string (string-ref content (sub1 end))))))
  (case type
    [(open-paren close-paren) (last-symbol)]
    [(parenthesis)
     (define candidate (last-symbol))
     (and (memq candidate '(|(| |)| |[| |]| |{| |}|)) candidate)]
    [else #f]))

(define (valid-snapshot-spans? content spans)
  (define content-length (string-length content))
  (let loop ([index 0] [previous-end 0])
    (cond
      [(= index (vector-length spans))
       (= previous-end content-length)]
      [else
       (match-define (struct* LexerTokenSpan ([start start] [end end]))
         (vector-ref spans index))
       (and (= previous-end start)
            (< start end)
            (<= end content-length)
            (loop (add1 index) end))])))

;; Lexer snapshots normalize a few token names but retain the same complete
;; source spans. This constructor is used only for the standard s-expression
;; fallback, where the normalized names above can be mapped back exactly to the
;; classifications consumed by syntax-color's Racket indentation procedures.
(define (make-textoid-from-lexer-snapshot snapshot
                                          #:source-directory [source-directory #f]
                                          #:paren-matches [paren-matches default-paren-matches])
  (define content (LexerSnapshot-text snapshot))
  (define spans (LexerSnapshot-tokens snapshot))
  (cond
    [(valid-snapshot-spans? content spans)
     (define tokens
       (for/vector #:length (vector-length spans)
         ([span (in-vector spans)])
         (match-define (struct* LexerTokenSpan ([type type] [start start] [end end]))
           span)
         (token (hasheq 'type (snapshot-token-type type))
                (snapshot-token-paren content type start end)
                start end)))
     (textoid-from-tokens content tokens paren-matches source-directory)]
    [else
     (make-textoid content
                   #:source-directory source-directory
                   #:paren-matches paren-matches)]))

;;; Navigation indexes
;;
;; Official color:text% / expeditor skip only white-space and comment.
;; Forward skip also includes sexp-comment; do not unify these lists.

(define skip-types/backward '(white-space comment))
(define skip-types/forward '(white-space comment sexp-comment))

(define (paren-tables paren-matches)
  (for/fold ([opening->closing (hash)] [closing-symbols (hash)])
            ([pair (in-list paren-matches)])
    (match pair
      [(list* open close _)
       (values (hash-set opening->closing open close)
               (hash-set closing-symbols close #t))]
      [_ (values opening->closing closing-symbols)])))

(define (token-kind-vector tokens opening->closing closing-symbols)
  (for/vector #:length (vector-length tokens)
    ([the-token (in-vector tokens)])
    (cond
      [(hash-has-key? opening->closing (token-paren the-token)) 'open]
      [(hash-has-key? closing-symbols (token-paren the-token)) 'close]
      [else #f])))

;; `open-stack-after` is a persistent stack of unmatched opener indexes at
;; each token boundary. A mismatched closer clears the active stack,
;; matching DrRacket's error-tree navigation instead of creating crossed
;; delimiter pairs.
(define (delimiter-indexes tokens token-kinds opening->closing)
  (define token-count (vector-length tokens))
  (define matching-open (make-vector token-count #f))
  (define matching-close (make-vector token-count #f))
  (define open-stack-after (make-vector token-count '()))
  (define (stack-after-close index stack)
    (and-let* ([(pair? stack)]
               [open-index (car stack)]
               [(eq? (hash-ref opening->closing
                               (token-paren (vector-ref tokens open-index))
                               #f)
                     (token-paren (vector-ref tokens index)))])
      (vector-set! matching-open index open-index)
      (vector-set! matching-close open-index index)
      (cdr stack)))
  (for/fold ([stack '()])
            ([index (in-range token-count)])
    (define next-stack
      (case (vector-ref token-kinds index)
        [(open) (cons index stack)]
        [(close) (or (stack-after-close index stack) '())]
        [else stack]))
    (vector-set! open-stack-after index next-stack)
    next-stack)
  (values matching-open matching-close open-stack-after))

(define (previous-non-skip-tokens tokens)
  (define result (make-vector (vector-length tokens) #f))
  (for/fold ([previous #f])
            ([the-token (in-vector tokens)]
             [index (in-naturals)])
    (vector-set! result index previous)
    (if (memq (token-type the-token) skip-types/backward) previous index))
  result)

(define (next-non-skip-tokens tokens)
  (define token-count (vector-length tokens))
  (define result (make-vector token-count #f))
  (for/fold ([next #f])
            ([index (in-range (sub1 token-count) -1 -1)])
    (vector-set! result index next)
    (define type (token-type (vector-ref tokens index)))
    (if (memq type skip-types/forward) next index))
  result)

;;; color-textoid<%>

(define textoid%
  (class* object% (color-textoid<%>)
    (init initial-content initial-tokens paren-matches [source-directory #f])

    (super-new)

    ;; Text, token offsets, and line starts change together. Keep these private
    ;; so callers cannot invalidate the navigation indexes through field writes.
    (define content initial-content)
    (define tokens initial-tokens)
    (define starts (line-starts content))
    (define source-dir source-directory)
    (define parens paren-matches)
    (define token-count (vector-length tokens))

    (define-values (token-kinds matching-open matching-close open-stack-after
                                previous-backward-token next-forward-token)
      (let ()
        (define-values (opening->closing closing-symbols)
          (paren-tables paren-matches))
        (define kinds (token-kind-vector tokens opening->closing closing-symbols))
        (define-values (matching-open matching-close open-stack-after)
          (delimiter-indexes tokens kinds opening->closing))
        (values kinds
                matching-open
                matching-close
                open-stack-after
                (previous-non-skip-tokens tokens)
                (next-non-skip-tokens tokens))))

    (define (token-index-at position [previous? #f])
      (define lookup-position
        (if (and previous?
                 (= position (string-length content))
                 (positive? position))
            (sub1 position)
            position))
      (and-let* ([(exact-nonnegative-integer? lookup-position)]
                 [(< lookup-position (string-length content))]
                 [index (first-token-ending-after lookup-position)]
                 [the-token (vector-ref tokens index)]
                 [(<= (token-start the-token) lookup-position)]
                 [(< lookup-position (token-end the-token))])
        index))

    (define (token-at position [previous? #f])
      (define index (token-index-at position previous?))
      (and index (vector-ref tokens index)))

    (define (first-token-ending-after position)
      (let loop ([low 0] [high token-count])
        (if (= low high)
            (and (< low token-count) low)
            (let* ([middle (quotient (+ low high) 2)]
                   [the-token (vector-ref tokens middle)])
              (if (<= (token-end the-token) position)
                  (loop (add1 middle) high)
                  (loop low middle))))))

    (define (last-token-ending-at-or-before position)
      (define next (first-token-ending-after position))
      (cond
        [(not next) (and (positive? token-count) (sub1 token-count))]
        [(zero? next) #f]
        [else (sub1 next)]))

    (define (line-for-position position)
      (define line-count (vector-length starts))
      (let loop ([low 0] [high line-count])
        (if (= low high)
            (sub1 low)
            (let ([middle (quotient (+ low high) 2)])
              (if (<= (vector-ref starts middle) position)
                  (loop (add1 middle) high)
                  (loop low middle))))))

    (define/public (get-text [start 0] [end 'eof])
      (substring content start (if (eq? end 'eof) (string-length content) end)))

    (define/public (get-character position)
      (if (and (exact-nonnegative-integer? position)
               (< position (string-length content)))
          (string-ref content position)
          #\nul))

    (define/public (last-position)
      (string-length content))

    (define/public (paragraph-count)
      (vector-length starts))

    (define/public (position-paragraph position [eol? #f])
      (define clamped (min (max position 0) (string-length content)))
      (line-for-position clamped))

    (define/public (paragraph-start-position paragraph [visible? #t])
      (if (and (exact-nonnegative-integer? paragraph)
               (< paragraph (vector-length starts)))
          (vector-ref starts paragraph)
          (string-length content)))

    (define/public (paragraph-end-position paragraph [visible? #t])
      (if (and (exact-nonnegative-integer? paragraph)
               (< (add1 paragraph) (vector-length starts)))
          (sub1 (vector-ref starts (add1 paragraph)))
          (string-length content)))

    (define (indent-whitespace? text)
      (for/and ([character (in-string text)])
        (and (char-whitespace? character)
             (not (char=? character #\newline)))))

    (define (whitespace-edit-safe? start end insert-text)
      (and (indent-whitespace? (substring content start end))
           (indent-whitespace? insert-text)
           (for/and ([the-token (in-vector tokens)]
                     #:when (and (< (token-start the-token) end)
                                 (> (token-end the-token) start)))
             (eq? (token-type the-token) 'white-space))))

    (define (whitespace-token-before position)
      (and-let* ([index (last-token-ending-at-or-before position)]
                 [the-token (vector-ref tokens index)]
                 [(= (token-end the-token) position)]
                 [(eq? (token-type the-token) 'white-space)])
        the-token))

    (define (apply-whitespace-edit! start end insert-text previous-token)
      (define delta (- (string-length insert-text) (- end start)))
      (set! content
            (string-append (substring content 0 start) insert-text (substring content end)))
      (for ([line-start (in-vector starts)]
            [index (in-naturals)])
        (when (> line-start start)
          (vector-set! starts index (+ line-start delta))))
      (for ([the-token (in-vector tokens)])
        (define token-start-position (token-start the-token))
        (define token-end-position (token-end the-token))
        (cond
          [(eq? the-token previous-token)
           (set-token-end! the-token (+ token-end-position delta))]
          [(<= token-end-position start) (void)]
          [(>= token-start-position end)
           (set-token-start! the-token (+ token-start-position delta))
           (set-token-end! the-token (+ token-end-position delta))]
          [else (set-token-end! the-token (+ token-end-position delta))])))

    ;; Mutate and return this textoid without invoking the language lexer when
    ;; the edit fits existing whitespace tokens and preserves line count.
    ;; Otherwise return #f without mutation, so the caller can re-lex.
    (define/public (replace-leading-whitespace paragraph delete-amount insert-text)
      (let/ec decline
        (unless (and (exact-nonnegative-integer? paragraph)
                     (< paragraph (vector-length starts)))
          (decline #f))
        (define start (vector-ref starts paragraph))
        (define end (+ start delete-amount))
        (unless (and (<= end (paragraph-end-position paragraph))
                     (whitespace-edit-safe? start end insert-text))
          (decline #f))
        (define insertion? (and (zero? delete-amount) (positive? (string-length insert-text))))
        (define previous-token (and insertion? (whitespace-token-before start)))
        ;; A pure insertion needs a preceding whitespace token to extend;
        ;; this path cannot create tokens or rebuild navigation indexes.
        (unless (or (positive? delete-amount)
                    (zero? (string-length insert-text))
                    previous-token)
          (decline #f))
        (apply-whitespace-edit! start end insert-text previous-token)
        this))

    ;; Return this object on the whitespace fast path, or a rebuilt textoid.
    ;; Re-lexing may fail (#f); the original object then remains unchanged.
    (define/public (replace-line-prefix! paragraph delete-amount insert-text)
      (or (replace-leading-whitespace paragraph delete-amount insert-text)
          (let ([start (paragraph-start-position paragraph)])
            (make-textoid (string-append (substring content 0 start)
                                         insert-text
                                         (substring content (+ start delete-amount)))
                          #:source-directory source-dir
                          #:paren-matches parens))))

    (define/public (get-token-range position)
      (define the-token (token-at position))
      (if the-token
          (values (token-start the-token) (token-end the-token))
          (values #f #f)))

    (define/public (classify-position* position)
      (define the-token (token-at position #t))
      (and the-token (token-attributes the-token)))

    (define/public (classify-position position)
      (define classification (classify-position* position))
      (and classification
           (hash-ref classification 'type 'unknown)))

    (define/public (skip-whitespace position direction comments?)
      (define (skip? the-token)
        (and the-token
             (memq (token-type the-token)
                   (if comments?
                       skip-types/forward
                       '(white-space)))))
      (case direction
        [(forward)
         (let loop ([position position])
           (define the-token (token-at position))
           (if (skip? the-token)
               (loop (token-end the-token))
               position))]
        [(backward)
         (let loop ([position (max 0 (sub1 position))]
                    [end-position position])
           (define the-token (token-at position))
           (if (skip? the-token)
               (if (zero? (token-start the-token))
                   0
                   (loop (sub1 (token-start the-token))
                         (token-start the-token)))
               end-position))]
        [else
         (error 'skip-whitespace "bad direction: ~e" direction)]))

    (define/public (backward-match position cutoff)
      (define initial-index (token-index-at (sub1 position)))
      (and-let* ([index
                  (cond
                    [(not initial-index) #f]
                    [(memq (token-type (vector-ref tokens initial-index))
                           skip-types/backward)
                     (vector-ref previous-backward-token initial-index)]
                    [else initial-index])])
        (let* ([the-token (vector-ref tokens index)]
               [kind (vector-ref token-kinds index)]
               [start (token-start the-token)])
          (cond
            [(< (min (sub1 position) (sub1 (token-end the-token))) cutoff) #f]
            [(eq? kind 'open) #f]
            [(eq? kind 'close)
             (if (> (token-end the-token) position)
                 start
                 (and-let* ([open-index (vector-ref matching-open index)]
                            [open-token (vector-ref tokens open-index)]
                            [(>= (sub1 (token-end open-token)) cutoff)])
                   (token-start open-token)))]
            [else start]))))

    ;; This follows the boundary behavior of syntax-color's text objects: an
    ;; opener immediately before the cursor is not itself a backward match,
    ;; while a cursor inside an atom matches that atom's start.
    (define/public (backward-containing-sexp position cutoff)
      (define query-position
        (if (<= position cutoff) cutoff (sub1 position)))
      (define query-index (token-index-at query-position))
      (define direct-open-index
        (and query-index
             (eq? (vector-ref token-kinds query-index) 'open)
             query-index))
      (define prefix-index
        (last-token-ending-at-or-before position))
      (define stack
        (and prefix-index (vector-ref open-stack-after prefix-index)))
      (and-let* ([open-index (or direct-open-index (and (pair? stack) (car stack)))]
                 [open-token (vector-ref tokens open-index)]
                 [(>= (sub1 (token-end open-token)) cutoff)])
        (min (skip-whitespace (token-end open-token) 'forward #f)
             position)))

    (define/public (forward-match position cutoff)
      (define initial-index (first-token-ending-after position))
      (and-let* ([index
                  (cond
                    [(not initial-index) #f]
                    [(memq (token-type (vector-ref tokens initial-index))
                           skip-types/forward)
                     (vector-ref next-forward-token initial-index)]
                    [else initial-index])]
                 [end
                  (let ([the-token (vector-ref tokens index)]
                        [kind (vector-ref token-kinds index)])
                    (cond
                      [(eq? kind 'open)
                       (define close-index (vector-ref matching-close index))
                       (and close-index
                            (token-end (vector-ref tokens close-index)))]
                      [(eq? kind 'close) #f]
                      [else (token-end the-token)]))]
                 [(<= end cutoff)])
        end))

    (define/public (get-backward-navigation-limit position)
      0)

    (define/public (get-regions)
      '((0 end)))

    (define/public (text)
      content)))

(define (textoid-content textoid)
  (send textoid text))

(define (textoid-replace-leading-whitespace textoid paragraph delete-amount insert-text)
  (send textoid replace-leading-whitespace paragraph delete-amount insert-text))

(define (textoid-replace-line-prefix! textoid paragraph delete-amount insert-text)
  (send textoid replace-line-prefix! paragraph delete-amount insert-text))
