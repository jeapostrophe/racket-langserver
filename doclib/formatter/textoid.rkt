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
;; Content is a contiguous string so `get-character` and token ranges stay O(1).
;; Sequential leading-whitespace edits recopy that string and shift later
;; token and paragraph offsets, rather than splitting into line-relative
;; columns. Line storage was tried and made indent slower.

(require racket/class
         "../lexer/snapshot.rkt"
         syntax-color/module-lexer)

(provide make-textoid
         make-textoid-from-lexer-snapshot
         textoid-replace-leading-whitespace
         textoid-content)

(struct token (attributes paren start end) #:mutable #:transparent)

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

;; These headless interfaces were extracted from framework into syntax-color-lib
;; 1.4. Keep older supported Racket releases loadable, while using the canonical
;; interface whenever it is installed so contracted language hooks recognize the
;; textoid object.
(define color-textoid<%>
  (with-handlers ([exn:fail? (lambda (_exn) fallback-color-textoid<%>)])
    (dynamic-require 'syntax-color/color-textoid 'color-textoid<%>)))

(define default-paren-matches
  (list (list (string->symbol "(") (string->symbol ")"))
        (list (string->symbol "[") (string->symbol "]"))
        (list (string->symbol "{") (string->symbol "}"))))

(define (source-directory->path source-directory)
  (cond
    [(not source-directory) #f]
    [(path? source-directory) (path->complete-path source-directory)]
    [(string? source-directory) (path->complete-path source-directory)]
    [else #f]))

(define (lex-content content source-directory)
  (define input (open-input-string content))
  (port-count-lines! input)
  (define (lex)
    (parameterize ([current-directory
                    (or (source-directory->path source-directory)
                        (current-directory))])
      (let loop ([mode #f] [tokens '()])
        (define-values (lexeme attributes paren start end backup next-mode)
          ((or module-lexer*/maybe module-lexer) input 0 mode))
        (define type
          (if (symbol? attributes)
              attributes
              (hash-ref attributes 'type 'unknown)))
        (cond
          [(eq? type 'eof)
           (list->vector (reverse tokens))]
          [(and (exact-integer? start)
                (exact-integer? end)
                (<= start end))
           ;; Lexer positions are one-based; textoid positions are zero-based.
           (loop next-mode
                 (cons (token (if (symbol? attributes)
                                  (hasheq 'type attributes)
                                  attributes)
                              paren
                              (sub1 start)
                              (sub1 end))
                       tokens))]
          [else
           ;; A malformed/custom lexer token is not useful to indentation.
           #f]))))
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (lex)))

(define (line-starts content)
  (list->vector
    (for/list ([position (in-range (add1 (string-length content)))]
               #:when (or (zero? position)
                          (and (> position 0)
                               (char=? (string-ref content (sub1 position)) #\newline))))
      position)))

(define (make-textoid content #:source-directory [source-directory #f]
                      #:paren-matches [paren-matches default-paren-matches])
  (define tokens (lex-content content source-directory))
  (and tokens
       (new textoid%
         [content content]
         [tokens tokens]
         [starts (line-starts content)]
         [paren-matches (if (list? paren-matches)
                            paren-matches
                            default-paren-matches)])))

(define (snapshot-token-type type)
  (case type
    [(open-paren close-paren) 'parenthesis]
    [(quote quasiquote syntax-quote syntax-quasiquote) 'constant]
    [(unquote unquote-splicing syntax-unquote syntax-unquote-splicing
              lang-directive reader-directive)
     'other]
    [else type]))

(define (snapshot-token-paren content span)
  (define type (LexerTokenSpan-type span))
  (define start (LexerTokenSpan-start span))
  (define end (LexerTokenSpan-end span))
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
      [(= index (vector-length spans)) #t]
      [else
       (define span (vector-ref spans index))
       (define start (LexerTokenSpan-start span))
       (define end (LexerTokenSpan-end span))
       (and (<= previous-end start)
            (< start end)
            (<= end content-length)
            (loop (add1 index) end))])))

;; Lexer snapshots normalize a few token names but retain the same complete
;; source spans. This constructor is used only for the standard s-expression
;; fallback, where the normalized names above can be mapped back exactly to the
;; classifications consumed by syntax-color's Racket indentation procedures.
(define (make-textoid-from-lexer-snapshot snapshot)
  (define content (LexerSnapshot-text snapshot))
  (define spans (LexerSnapshot-tokens snapshot))
  (cond
    [(valid-snapshot-spans? content spans)
     (define tokens
       (for/vector #:length (vector-length spans)
         ([span (in-vector spans)])
         (token (hasheq 'type
                        (snapshot-token-type (LexerTokenSpan-type span)))
                (snapshot-token-paren content span)
                (LexerTokenSpan-start span)
                (LexerTokenSpan-end span))))
     (new textoid%
       [content content]
       [tokens tokens]
       [starts (line-starts content)]
       [paren-matches default-paren-matches])]
    [else (make-textoid content)]))

(define textoid%
  (class* object% (color-textoid<%>)
    (init-field content tokens starts paren-matches)

    (super-new)

    (define token-count (vector-length tokens))

    (define opening-symbols
      (for/hash ([pair (in-list paren-matches)]
                 #:when (and (pair? pair) (pair? (cdr pair))))
        (values (car pair) #t)))

    (define opening->closing
      (for/hash ([pair (in-list paren-matches)]
                 #:when (and (pair? pair) (pair? (cdr pair))))
        (values (car pair) (cadr pair))))

    (define closing-symbols
      (for/hash ([pair (in-list paren-matches)]
                 #:when (and (pair? pair) (pair? (cdr pair))))
        (values (cadr pair) #t)))

    (define token-kinds
      (for/vector #:length token-count
        ([the-token (in-vector tokens)])
        (cond
          [(hash-ref opening-symbols (token-paren the-token) #f) 'open]
          [(hash-ref closing-symbols (token-paren the-token) #f) 'close]
          [else #f])))

    ;; `open-stack-after` is a persistent stack of unmatched opener indexes at
    ;; each token boundary, which makes containing-form lookup constant-time
    ;; after locating the token at the cursor. A mismatched closer invalidates
    ;; the active stack, matching DrRacket's error-tree navigation instead of
    ;; creating crossed delimiter pairs.
    (define-values (matching-open matching-close open-stack-after)
      (let ([matching-open (make-vector token-count #f)]
            [matching-close (make-vector token-count #f)]
            [open-stack-after (make-vector token-count '())])
        (let loop ([index 0] [stack '()])
          (cond
            [(= index token-count)
             (values matching-open matching-close open-stack-after)]
            [else
             (define kind (vector-ref token-kinds index))
             (define next-stack
               (case kind
                 [(open) (cons index stack)]
                 [(close)
                  (cond
                    [(and (pair? stack)
                          (eq? (hash-ref opening->closing
                                         (token-paren (vector-ref tokens (car stack)))
                                         #f)
                               (token-paren (vector-ref tokens index))))
                     (define open-index (car stack))
                     (vector-set! matching-open index open-index)
                     (vector-set! matching-close open-index index)
                     (cdr stack)]
                    [else '()])]
                 [else stack]))
             (vector-set! open-stack-after index next-stack)
             (loop (add1 index) next-stack)]))))

    (define previous-backward-token
      (let ([result (make-vector token-count #f)])
        (let loop ([index 0] [previous #f])
          (cond
            [(= index token-count) result]
            [else
             (vector-set! result index previous)
             (define type (token-type (vector-ref tokens index)))
             (loop (add1 index)
                   (if (memq type '(white-space comment)) previous index))]))))

    (define next-forward-token
      (let ([result (make-vector token-count #f)])
        (let loop ([index (sub1 token-count)] [next #f])
          (cond
            [(negative? index) result]
            [else
             (vector-set! result index next)
             (define type (token-type (vector-ref tokens index)))
             (loop (sub1 index)
                   (if (memq type '(white-space comment sexp-comment)) next index))]))))

    (define (token-ref index)
      (and index (vector-ref tokens index)))

    (define (token-index-at position [previous? #f])
      (define lookup-position
        (if (and previous?
                 (= position (string-length content))
                 (positive? position))
            (sub1 position)
            position))
      (and (exact-nonnegative-integer? lookup-position)
           (< lookup-position (string-length content))
           (let ([index (first-token-ending-after lookup-position)])
             (and index
                  (let ([the-token (vector-ref tokens index)])
                    (and (<= (token-start the-token) lookup-position)
                         (< lookup-position (token-end the-token))
                         index))))))

    (define (token-at position [previous? #f])
      (token-ref (token-index-at position previous?)))

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
      (substring content
                 start
                 (if (eq? end 'eof)
                     (string-length content)
                     end)))

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
      (vector-ref starts (min paragraph (sub1 (vector-length starts)))))

    (define/public (paragraph-end-position paragraph [visible? #t])
      (define next-line-start
        (and (exact-nonnegative-integer? paragraph)
             (< (add1 paragraph) (vector-length starts))
             (vector-ref starts (add1 paragraph))))
      (if next-line-start
          (sub1 next-line-start)
          (string-length content)))

    ;; Return an updated textoid without invoking the language lexer. Replacing
    ;; only a paragraph's leading whitespace cannot change token categories,
    ;; delimiter nesting, or line count. Callers must fall back to `make-textoid`
    ;; when this deliberately narrow precondition is not met.
    (define/public (replace-leading-whitespace paragraph delete-amount insert-text)
      (define paragraph-valid?
        (and (exact-nonnegative-integer? paragraph)
             (< paragraph (vector-length starts))))
      (define start
        (and paragraph-valid? (vector-ref starts paragraph)))
      (define end
        (and start (+ start delete-amount)))
      (define paragraph-end
        (and paragraph-valid?
             (send this paragraph-end-position paragraph)))
      (define deleted-text
        (and end paragraph-end (<= end paragraph-end)
             (substring content start end)))
      (define safe-edit?
        (and deleted-text
             (for/and ([character (in-string deleted-text)])
               (and (char-whitespace? character)
                    (not (char=? character #\newline))))
             (for/and ([character (in-string insert-text)])
               (and (char-whitespace? character)
                    (not (char=? character #\newline))))
             (for/and ([the-token (in-vector tokens)]
                       #:when (and (< (token-start the-token) end)
                                   (> (token-end the-token) start)))
               (eq? (token-type the-token) 'white-space))))
      (and safe-edit?
           (let* ([insert-length (string-length insert-text)]
                  [delta (- insert-length delete-amount)]
                  [previous-index
                   (and (zero? delete-amount)
                        (positive? insert-length)
                        (last-token-ending-at-or-before start))]
                  [previous-whitespace?
                   (and previous-index
                        (let ([previous-token (vector-ref tokens previous-index)])
                          (and (= (token-end previous-token) start)
                               (eq? (token-type previous-token) 'white-space))))]
                  [overlapping-whitespace?
                   (or (positive? delete-amount)
                       (zero? insert-length)
                       previous-whitespace?)])
             (and overlapping-whitespace?
                  (begin
                    (set! content
                          (string-append (substring content 0 start)
                                         insert-text
                                         (substring content end)))
                    (for ([index (in-range (vector-length starts))])
                      (when (> (vector-ref starts index) start)
                        (vector-set! starts index
                                     (+ (vector-ref starts index) delta))))
                    (for ([the-token (in-vector tokens)]
                          [index (in-naturals)])
                      (define token-start-position (token-start the-token))
                      (define token-end-position (token-end the-token))
                      (cond
                        [(and previous-whitespace?
                              (= index previous-index))
                         (set-token-end! the-token (+ token-end-position delta))]
                        [(<= token-end-position start) (void)]
                        [(>= token-start-position end)
                         (set-token-start! the-token (+ token-start-position delta))
                         (set-token-end! the-token (+ token-end-position delta))]
                        [else
                         (set-token-end! the-token (+ token-end-position delta))]))
                    this)))))

    (define/public (get-token-range position)
      (define the-token (token-at position))
      (if the-token
          (values (token-start the-token) (token-end the-token))
          (values #f #f)))

    (define/public (classify-position* position)
      (define the-token (token-at position #t))
      (and the-token (token-attributes the-token)))

    (define/public (classify-position position)
      (define classification (send this classify-position* position))
      (and classification
           (hash-ref classification 'type 'unknown)))

    (define/public (skip-whitespace position direction comments?)
      (define (skip? the-token)
        (and the-token
             (or (eq? (token-type the-token) 'white-space)
                 (and comments?
                      (memq (token-type the-token)
                            '(comment sexp-comment))))))
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
      (backward-matching-search position cutoff 'one))

    (define/public (backward-containing-sexp position cutoff)
      (backward-matching-search position cutoff 'all))

    ;; This follows the boundary behavior of syntax-color's text objects: an
    ;; opener immediately before the cursor is not itself a backward match,
    ;; while a cursor inside an atom matches that atom's start.
    (define/private (backward-matching-search initial-position cutoff mode)
      (case mode
        [(one)
         (define initial-index (token-index-at (sub1 initial-position)))
         (define index
           (let loop ([index initial-index])
             (cond
               [(not index) #f]
               [(memq (token-type (vector-ref tokens index))
                      '(white-space comment))
                (loop (vector-ref previous-backward-token index))]
               [else index])))
         (and index
              (let* ([the-token (vector-ref tokens index)]
                     [kind (vector-ref token-kinds index)]
                     [start (token-start the-token)])
                (cond
                  [(< (min (sub1 initial-position)
                           (sub1 (token-end the-token)))
                      cutoff)
                   #f]
                  [(eq? kind 'open) #f]
                  [(eq? kind 'close)
                   (if (> (token-end the-token) initial-position)
                       start
                       (let ([open-index (vector-ref matching-open index)])
                         (and open-index
                              (let ([open-token (vector-ref tokens open-index)])
                                (and (>= (sub1 (token-end open-token)) cutoff)
                                     (token-start open-token))))))]
                  [else start])))]
        [(all)
         (define query-position
           (if (<= initial-position cutoff) cutoff (sub1 initial-position)))
         (define query-index (token-index-at query-position))
         (define direct-open-index
           (and query-index
                (eq? (vector-ref token-kinds query-index) 'open)
                query-index))
         (define prefix-index
           (last-token-ending-at-or-before initial-position))
         (define stack
           (and prefix-index (vector-ref open-stack-after prefix-index)))
         (define open-index
           (or direct-open-index (and (pair? stack) (car stack))))
         (and open-index
              (let ([open-token (vector-ref tokens open-index)])
                (and (>= (sub1 (token-end open-token)) cutoff)
                     (min (send this skip-whitespace
                                (token-end open-token) 'forward #f)
                          initial-position))))]))

    (define/public (forward-match position cutoff)
      (define initial-index (first-token-ending-after position))
      (define index
        (let loop ([index initial-index])
          (cond
            [(not index) #f]
            [(memq (token-type (vector-ref tokens index))
                   '(white-space comment sexp-comment))
             (loop (vector-ref next-forward-token index))]
            [else index])))
      (and index
           (let* ([the-token (vector-ref tokens index)]
                  [kind (vector-ref token-kinds index)]
                  [end
                   (cond
                     [(eq? kind 'open)
                      (define close-index (vector-ref matching-close index))
                      (and close-index
                           (token-end (vector-ref tokens close-index)))]
                     [(eq? kind 'close) #f]
                     [else (token-end the-token)])])
             (and end (<= end cutoff) end))))

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
