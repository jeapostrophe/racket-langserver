#lang racket/base

;; Helpers for same-file hover source detail. Built when the trace expands.
;; We store ranges only. `doc-hover` reads text from the live buffer.
;; While a trace is old, we use shifted ranges. The result can be wrong or
;; incomplete. Do not wait for `doc-trace-latest?`.
;;
;; Do not match binder names like `define`, `let`, or `fun`. Use structure only
;; so macro binders (for example `for/list`) and Rhombus share one path.
;; See tests/lib/doc-test.rkt hover cases.
;;
;; We do not use a fixed +/-1 line window. Lines above or below often have
;; unrelated indentation and make the card harder to read.

(require "types.rkt"
         racket/class
         racket/list
         srfi/2
         (only-in "../../lexer/state.rkt"
                  LexerState-snapshot)
         (only-in "../../lexer/snapshot.rkt"
                  LexerSnapshot-text
                  LexerSnapshot-tokens
                  LexerTokenSpan-start
                  LexerTokenSpan-end
                  LexerTokenSpan-type
                  find-token-index-at-or-before))

(provide leading-comment-lines
         source-candidates
         ranges-for-identifier
         display-range-for-identifier
         expand-detail
         contract-detail)

;; Comment scanning has limits and runs only when the trace is built.
;; Keep these limits in sync with features.md and the hover manual.
(define max-hover-comment-lines 10)
(define max-hover-comment-line-characters 200)

;; Own-line comments directly above `form-start`.
;; Runs only when the trace is built. Hover requests never scan backwards.
;; Own-line means trailing comments on the line above are not attached to the
;; form below.
(define (leading-comment-lines lexer-state form-start)
  (define snapshot (LexerState-snapshot lexer-state))
  (define text (LexerSnapshot-text snapshot))
  (define tokens (LexerSnapshot-tokens snapshot))
  (define comment-range
    (and-let* ([(positive? form-start)]
               [token-index
                (find-token-index-at-or-before tokens (sub1 form-start))])
      (leading-comment-range text tokens token-index form-start)))
  (if comment-range
      (comment-range->lines text (car comment-range) (cdr comment-range))
      (values '() #f)))

;; Walk tokens backwards from `form-start`. Stop at a non-comment, or at a gap
;; of more than one line.
(define (leading-comment-range text tokens token-index form-start)
  (let loop ([idx token-index]
             [next-start form-start]
             [comment-ranges '()])
    (cond
      [(negative? idx)
       (comment-ranges->range comment-ranges)]
      [else
       (define span (vector-ref tokens idx))
       (define type (LexerTokenSpan-type span))
       (define start (LexerTokenSpan-start span))
       (define end (LexerTokenSpan-end span))
       (cond
         [(eq? type 'white-space)
          (loop (sub1 idx) next-start comment-ranges)]
         [(and (eq? type 'comment)
               (comment-on-own-line? text start)
               (single-line-gap? text end next-start))
          (loop (sub1 idx)
                start
                (cons (cons start end) comment-ranges))]
         [else
          (comment-ranges->range comment-ranges)])])))

(define (comment-ranges->range comment-ranges)
  (and (pair? comment-ranges)
       (cons (caar comment-ranges)
             (cdr (last comment-ranges)))))

;; Only whitespace may appear between the line start and the comment.
(define (comment-on-own-line? text comment-start)
  (define line-start
    (or (for/first ([pos (in-range (sub1 comment-start) -1 -1)]
                    #:when (char=? (string-ref text pos) #\newline))
          (add1 pos))
        0))
  (for/and ([pos (in-range line-start comment-start)])
    (char-whitespace? (string-ref text pos))))

;; True when [start, end) is whitespace with exactly one newline.
(define (single-line-gap? text start end)
  (let loop ([pos start]
             [newlines 0])
    (cond
      [(>= pos end)
       (= newlines 1)]
      [else
       (define ch (string-ref text pos))
       (cond
         [(char=? ch #\newline)
          (and (zero? newlines)
               (loop (add1 pos) 1))]
         [(char-whitespace? ch)
          (loop (add1 pos) newlines)]
         [else #f])])))

;; Split [start, end) into comment lines. Stop after the line limit.
(define (comment-range->lines text start end)
  (define-values (kept-rev truncated?)
    (let loop ([pos start]
               [line-start start]
               [kept-rev '()]
               [kept-count 0])
      (cond
        [(>= pos end)
         (if (= kept-count max-hover-comment-lines)
             (values kept-rev (< line-start end))
             (values (cons (cons line-start end) kept-rev) #f))]
        [(char=? (string-ref text pos) #\newline)
         (cond
           [(= kept-count max-hover-comment-lines)
            (values kept-rev #t)]
           [else
            (define new-kept (cons (cons line-start pos) kept-rev))
            (define new-count (add1 kept-count))
            (if (= new-count max-hover-comment-lines)
                (values new-kept (< (add1 pos) end))
                (loop (add1 pos) (add1 pos) new-kept new-count))])]
        [else
         (loop (add1 pos) line-start kept-rev kept-count)])))
  (define comment-lines
    (for/list ([line-range (in-list (reverse kept-rev))])
      (define line-start (car line-range))
      (define line-end (cdr line-range))
      (define displayed-end
        (min line-end
             (+ line-start max-hover-comment-line-characters)))
      (Hover-Comment-Line line-start
                          displayed-end
                          (< displayed-end line-end))))
  (values comment-lines truncated?))

;; A source candidate is a 0-based, end-exclusive range pair. Racket compound
;; syntax nodes have original source ranges. Rhombus nodes like `group` do not,
;; so we build their range from original descendants.
(define (source-candidates stx src doc-length source-kind)
  (remove-duplicates
    (case source-kind
      [(sexp)
       (collect-sexp-candidates stx src doc-length)]
      [(rhombus)
       (define-values (_all-ranges candidates)
         (collect-rhombus-candidates stx src doc-length 0))
       candidates]
      [else '()])
    equal?))

;; Sexp candidates use each compound node's own original range.
;; No aggregate bubbling: parents do not need descendant ranges.
(define (collect-sexp-candidates stx src doc-length)
  (define children (syntax-children stx))
  (define child-candidates
    (append* (for/list ([child (in-list children)])
               (collect-sexp-candidates child src doc-length))))
  (define own-range (original-source-range stx src doc-length))
  (define candidate (and (pair? children) own-range))
  (if candidate
      (cons candidate child-candidates)
      child-candidates))

;; Returns (values all-ranges candidates). Parents need `all-ranges` to build
;; aggregate ranges for Rhombus nodes that lack original ranges. Candidates use
;; aggregate-range after skipping reader wrappers (depth < 2).
(define (collect-rhombus-candidates stx src doc-length depth)
  (define children (syntax-children stx))
  (define-values (range-chunks candidate-chunks)
    (for/lists (range-chunks candidate-chunks)
               ([child (in-list children)])
      (collect-rhombus-candidates child src doc-length (add1 depth))))
  (define child-ranges (append* range-chunks))
  (define child-candidates (append* candidate-chunks))
  (define own-range (original-source-range stx src doc-length))
  (define all-ranges
    (if own-range
        (cons own-range child-ranges)
        child-ranges))
  (define aggregate-range (bounding-range all-ranges))
  (define candidate
    ;; Skip reader wrappers. Use concrete structural descendants instead.
    (and (>= depth 2) (pair? children) aggregate-range))
  (values all-ranges
          (if candidate
              (cons candidate child-candidates)
              child-candidates)))

(define (syntax-children stx)
  (define maybe-list (syntax->list stx))
  (cond
    [maybe-list maybe-list]
    [else
     (define datum (syntax-e stx))
     (cond
       [(pair? datum)
        (filter syntax? (list (car datum) (cdr datum)))]
       [(vector? datum)
        (filter syntax? (vector->list datum))]
       [else '()])]))

(define (original-source-range stx src doc-length)
  (and-let* ([(syntax-original? stx)]
             [(equal? src (syntax-source stx))]
             [position (syntax-position stx)]
             [span-length (syntax-span stx)]
             [(exact-positive-integer? position)]
             [(exact-positive-integer? span-length)]
             [start (sub1 position)]
             [end (+ start span-length)]
             [(<= end doc-length)])
    (cons start end)))

(define (bounding-range ranges)
  (and-let* ([(pair? ranges)])
    (for/fold ([lo (caar ranges)]
               [hi (cdar ranges)]
               #:result (cons lo hi))
              ([r (in-list (cdr ranges))])
      (values (min lo (car r))
              (max hi (cdr r))))))

(define (ranges-for-identifier candidates doc-text identifier-range)
  (define containing-ranges
    (filter (lambda (candidate)
              (strictly-contains-range? candidate identifier-range))
            candidates))
  (define nearest-range
    (smallest-range containing-ranges))
  ;; The nearest enclosing compound form is the smallest source range.
  ;; Prefer the largest same-line containing form when one exists (compact
  ;; clause, collapsed header, or one-line form). Otherwise use the full nearest
  ;; form. `display-range-for-identifier` may still cut to one line.
  ;; Do not add lines from above or below.
  (values (or (largest-range (same-line-ranges doc-text
                                               identifier-range
                                               containing-ranges))
              nearest-range)
          nearest-range))

;; Stale or out-of-range positions can fail in `pos->line/char`. Treat as no
;; same-line candidates rather than aborting detail selection.
(define (same-line-ranges doc-text identifier-range candidates)
  (with-handlers ([exn:fail? (lambda (_exn) '())])
    (define identifier-line
      (car (send doc-text pos->line/char (car identifier-range))))
    (filter (lambda (candidate)
              (= identifier-line
                 (car (send doc-text pos->line/char (car candidate)))))
            candidates)))

;; Same stale-position fallback: keep the nearest form as the display window.
(define (display-range-for-identifier doc-text identifier-range source-range nearest-range)
  (define source-start (car source-range))
  (define source-end (cdr source-range))
  (define nearest-start (car nearest-range))
  (define nearest-end (cdr nearest-range))
  (with-handlers ([exn:fail? (lambda (_exn)
                               (values nearest-start nearest-end))])
    (define identifier-line
      (car (send doc-text pos->line/char (car identifier-range))))
    (define source-start-line
      (car (send doc-text pos->line/char source-start)))
    (cond
      [(= source-start-line identifier-line)
       (define header-end (send doc-text line-end-pos identifier-line))
       (values source-start
               (min source-end (max header-end nearest-end)))]
      ;; No same-line header. Show the full nearest form.
      [else (values nearest-start nearest-end)])))

(define (smallest-range ranges)
  (for/fold ([smallest #f])
            ([candidate (in-list ranges)])
    (cond
      [(not smallest) candidate]
      [(< (- (cdr candidate) (car candidate))
          (- (cdr smallest) (car smallest)))
       candidate]
      [else smallest])))

(define (largest-range ranges)
  (for/fold ([largest #f])
            ([candidate (in-list ranges)])
    (cond
      [(not largest) candidate]
      ;; When a root aggregate shares a start offset, prefer the smaller child.
      ;; Then the first top-level Rhombus form does not cover everything after it.
      [(= (car candidate) (car largest))
       (if (< (cdr candidate) (cdr largest))
           candidate
           largest)]
      [(> (- (cdr candidate) (car candidate))
          (- (cdr largest) (car largest)))
       candidate]
      [else largest])))

(define (strictly-contains-range? outer inner)
  (and (<= (car outer) (car inner))
       (>= (cdr outer) (cdr inner))
       (or (< (car outer) (car inner))
           (> (cdr outer) (cdr inner)))))

;; Insertion at [start, end). Starts use >= so a boundary insert moves them.
;; Ends use > so an insert at an end-exclusive boundary does not grow the range.
(define (expand-start-position position start increase)
  (if (>= position start)
      (+ position increase)
      position))

(define (expand-end-position position start increase)
  (if (> position start)
      (+ position increase)
      position))

;; `position-journal-replay` callback for insertions at [start, end).
(define (expand-detail detail start end)
  (define increase (- end start))
  (define comment-lines
    (for/list ([line (in-list (Hover-Detail-comment-lines detail))])
      (expand-comment-line line start end)))
  (Hover-Detail comment-lines
                (Hover-Detail-comments-truncated? detail)
                (expand-start-position (Hover-Detail-source-start detail)
                                       start
                                       increase)
                (expand-start-position (Hover-Detail-display-start detail)
                                       start
                                       increase)
                (expand-end-position (Hover-Detail-display-end detail)
                                     start
                                     increase)
                (expand-end-position (Hover-Detail-source-end detail)
                                     start
                                     increase)
                (Hover-Detail-fence-language detail)))

;; Deletion at [start, end): positions inside the span collapse to `start`.
(define (contract-position position start end)
  (cond
    [(<= position start) position]
    [(>= position end) (- position (- end start))]
    [else start]))

;; `position-journal-replay` callback for deletions at [start, end).
;; Return #f when a deletion would break source/display ordering.
;; After that, the journal drops the value forever. Later inserts cannot fix it.
(define (contract-detail detail start end)
  (define comment-lines
    (filter values
            (for/list ([line (in-list (Hover-Detail-comment-lines detail))])
              (contract-comment-line line start end))))
  (define source-start
    (contract-position (Hover-Detail-source-start detail) start end))
  (define display-start
    (contract-position (Hover-Detail-display-start detail) start end))
  (define display-end
    (contract-position (Hover-Detail-display-end detail) start end))
  (define source-end
    (contract-position (Hover-Detail-source-end detail) start end))
  (and (<= source-start display-start)
       (< display-start display-end)
       (<= display-end source-end)
       (Hover-Detail comment-lines
                     (Hover-Detail-comments-truncated? detail)
                     source-start
                     display-start
                     display-end
                     source-end
                     (Hover-Detail-fence-language detail))))

(define (expand-comment-line line start end)
  (define increase (- end start))
  (Hover-Comment-Line (expand-start-position (Hover-Comment-Line-start line)
                                             start
                                             increase)
                      (expand-end-position (Hover-Comment-Line-end line)
                                           start
                                           increase)
                      (Hover-Comment-Line-truncated? line)))

;; Return #f when the comment line is fully inside the deleted span.
(define (contract-comment-line line start end)
  (define contracted-start
    (contract-position (Hover-Comment-Line-start line) start end))
  (define contracted-end
    (contract-position (Hover-Comment-Line-end line) start end))
  (and (< contracted-start contracted-end)
       (Hover-Comment-Line contracted-start
                           contracted-end
                           (Hover-Comment-Line-truncated? line))))
