#lang racket/base

;; Headless DrRacket-compatible indentation. This module uses syntax-color's
;; public `color-textoid<%>` when available and an identical local interface
;; on older supported releases. It does not load framework, racket:text%, or
;; any GUI library. Scribble's published indentation hook lives in gui-lib, so
;; this backend substitutes a GUI-free port of `determine-spaces` for
;; Scribble-surface languages.

(require racket/class
         racket/contract
         racket/list
         "../../common/interfaces.rkt"
         "../lexer/snapshot.rkt"
         "scribble.rkt"
         "textoid.rkt")

(provide drracket-format-edits)

(define hook-failed (gensym 'hook-failed))

(define racket-amount-to-indent
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (dynamic-require 'syntax-color/racket-indentation
                     'racket-amount-to-indent)))

(define (source-directory->path source-directory)
  (and source-directory
       (path->complete-path source-directory)))

(define (safe-reader-info content source-directory)
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (parameterize ([current-directory
                    (or (source-directory->path source-directory)
                        (current-directory))])
      (define info
        (read-language (open-input-string content) (lambda () #f)))
      (and (procedure? info) info))))

(define (safe-info-ref info key [default #f])
  (if info
      (with-handlers ([exn:fail? (lambda (_exn) default)])
        (info key default))
      default))

(define (language-indentation-hook info)
  (cond
    [(scribble-surface-language? info) scribble-determine-spaces]
    [else (safe-info-ref info 'drracket:indentation)]))

(define (call-hook hook . arguments)
  (with-handlers ([exn:fail? (lambda (_exn) hook-failed)])
    (apply hook arguments)))

(define (line-count textoid)
  (send textoid paragraph-count))

(define (line-leading-whitespace-length textoid line)
  (define start (send textoid paragraph-start-position line))
  (define end (send textoid paragraph-end-position line))
  (let loop ([position start])
    (if (and (< position end)
             (char-whitespace? (send textoid get-character position)))
        (loop (add1 position))
        (- position start))))

(define (valid-substitution? value)
  (and (list? value)
       (>= (length value) 2)
       (exact-nonnegative-integer? (first value))
       (string? (second value))))

(define (range-substitutions value)
  (and (list? value)
       (andmap valid-substitution? value)
       value))

(define (make-edit textoid line delete-amount insert-text)
  (define start (send textoid paragraph-start-position line))
  (define end (send textoid paragraph-end-position line))
  (define bounded-delete (min delete-amount (- end start)))
  (define old-text (send textoid get-text start (+ start bounded-delete)))
  (and (not (and (string=? old-text insert-text)
                 (= bounded-delete (string-length old-text))))
       (TextEdit #:range (Range (Pos line 0)
                                (Pos line bounded-delete))
                 #:newText insert-text)))

(define (edits-for-substitutions textoid first-line last-line substitutions)
  ;; `in-value` sits after `#:break` so it nests per line. Beside `in-list`
  ;; it would be a length-1 parallel sequence and stop after the first line.
  (for/list ([substitution (in-list substitutions)]
             [line (in-naturals first-line)]
             #:break (or (> line last-line)
                         (>= line (line-count textoid)))
             [edit (in-value
                     (make-edit textoid
                                line
                                (first substitution)
                                (second substitution)))]
             #:when edit)
    edit))

(define (apply-line-edit text textoid edit)
  (define line
    (Pos-line (Range-start (TextEdit-range edit))))
  (define start (send textoid paragraph-start-position line))
  (define deleted
    (Pos-char (Range-end (TextEdit-range edit))))
  (string-append (substring text 0 start)
                 (TextEdit-newText edit)
                 (substring text (+ start deleted))))

(define (line-indentation textoid line indentation racket-fallback?)
  (define position (send textoid paragraph-start-position line))
  (define hook-result
    (if (procedure? indentation)
        (call-hook indentation textoid position)
        #f))
  (define target
    (cond
      [(exact-nonnegative-integer? hook-result) hook-result]
      [(eq? hook-result hook-failed) hook-failed]
      ;; DrRacket uses `(or hook racket-amount-to-indent)`. A `#f` result,
      ;; including Scribble's embedded-Racket `(` case, therefore still
      ;; indents when the GUI-free standard indenter is installed.
      [racket-amount-to-indent
       (with-handlers ([exn:fail? (lambda (_exn) hook-failed)])
         (racket-amount-to-indent textoid position))]
      [racket-fallback?
       (raise-arguments-error
         'drracket-format-edits
         (string-append
           "standard Racket indentation requires syntax-color-lib 1.4 or newer; "
           "update the syntax-color-lib package"))]
      [else #f]))
  (cond
    [(eq? target hook-failed) hook-failed]
    [(exact-nonnegative-integer? target)
     (make-edit textoid
                line
                (line-leading-whitespace-length textoid line)
                (make-string target #\space))]
    [else #f]))

(define (fallback-line-edits text textoid first-line last-line indentation
                             source-directory parens racket-fallback?)
  ;; DrRacket tabifies one paragraph at a time.  Refreshing the textoid after
  ;; each replacement is important: the visual offset used for a later line
  ;; can depend on indentation corrected on an earlier line.  The edit ranges
  ;; remain line-relative, so they are valid simultaneously against `text`.
  (let loop ([current-text text]
             [current-textoid textoid]
             [line first-line]
             [edits '()])
    (cond
      [(> line last-line) (reverse edits)]
      [else
       (define edit
         (line-indentation current-textoid line indentation racket-fallback?))
       (cond
         [(eq? edit hook-failed)
          ;; Abort the whole request rather than returning a prefix of
          ;; line edits. DrRacket would already have applied earlier
          ;; lines in the buffer; LSP applies the returned list
          ;; simultaneously, so a partial list would leave later lines
          ;; indented against a layout that never landed.
          '()]
         [else
          (define updated-textoid
            (and edit
                 (textoid-replace-leading-whitespace
                   current-textoid
                   line
                   (Pos-char (Range-end (TextEdit-range edit)))
                   (TextEdit-newText edit))))
          ;; On the whitespace fast path the textoid is authoritative.
          ;; Rebuilding the full document string here would make sequential
          ;; indentation quadratic in document size.
          (define next-text
            (cond
              [updated-textoid #f]
              [edit
               (apply-line-edit (or current-text
                                    (textoid-content current-textoid))
                                current-textoid
                                edit)]
              [else current-text]))
          (define next-textoid
            (cond
              [updated-textoid updated-textoid]
              [edit
               (make-textoid next-text
                             #:source-directory source-directory
                             #:paren-matches parens)]
              [else current-textoid]))
          (if next-textoid
              (loop next-text
                    next-textoid
                    (add1 line)
                    (if edit (cons edit edits) edits))
              '())])])))

;; Format the inclusive line interval [start-line, end-line].  A language's
;; `drracket:range-indentation` hook is attempted first.  Its substitutions
;; are interpreted exactly as DrRacket does: delete N characters at each line
;; start, then insert a string.  If it returns #f (or is unavailable), each
;; line uses `drracket:indentation`. Missing hooks or a `#f` result then use
;; syntax-color's standard Racket indentation when it is installed, matching
;; DrRacket's `or` rule. Recognized s-expression languages still require that
;; library when no hook supplies an amount. The backend accepts and
;; intentionally ignores unsupported LSP formatting options.
(define/contract (drracket-format-edits text start-line end-line
                                        #:formatting-options _options
                                        #:racket-fallback? [racket-fallback? #f]
                                        #:lexer-snapshot [lexer-snapshot #f]
                                        #:src-dir [src-dir #f]
                                        #:interactive? [interactive? #f])
  (->* (string?
         exact-nonnegative-integer?
         exact-nonnegative-integer?
         #:formatting-options FormattingOptions?)
       (#:racket-fallback? boolean?
        #:lexer-snapshot (or/c LexerSnapshot? #f)
        #:src-dir (or/c path? #f)
        #:interactive? boolean?)
       (listof TextEdit?))
  (cond
    [(> start-line end-line)
     '()]
    [else
     (define info (safe-reader-info text src-dir))
     (define indentation (language-indentation-hook info))
     (define range-indentation
       (safe-info-ref info 'drracket:range-indentation))
     (define hook-available?
       (or (procedure? indentation)
           (procedure? range-indentation)))
     (cond
       [(not (or hook-available? racket-fallback?)) '()]
       [else
        (define parens
          (safe-info-ref info 'drracket:paren-matches))
        (define reusable-snapshot?
          (and lexer-snapshot
               racket-fallback?
               (not hook-available?)
               (not parens)
               (string=? text (LexerSnapshot-text lexer-snapshot))))
        (define textoid
          (cond
            [reusable-snapshot?
             (make-textoid-from-lexer-snapshot lexer-snapshot)]
            [parens
             (make-textoid text
                           #:source-directory src-dir
                           #:paren-matches parens)]
            [else (make-textoid text #:source-directory src-dir)]))
        (define total-lines (and textoid (line-count textoid)))
        (if (or (not total-lines)
                (>= start-line total-lines))
            '()
            (let* ([last-line (min end-line (sub1 total-lines))]
                   [start-position
                    (send textoid paragraph-start-position start-line)]
                   [end-position
                    (send textoid paragraph-end-position last-line)]
                   [range-result
                    (and (not interactive?)
                         (procedure? range-indentation)
                         (call-hook range-indentation
                                    textoid
                                    start-position
                                    end-position))])
              (cond
                [(eq? range-result hook-failed) '()]
                [(not range-result)
                 (fallback-line-edits text
                                      textoid
                                      start-line
                                      last-line
                                      indentation
                                      src-dir
                                      parens
                                      racket-fallback?)]
                [else
                 (define substitutions
                   (range-substitutions range-result))
                 (if substitutions
                     (edits-for-substitutions textoid
                                              start-line
                                              last-line
                                              substitutions)
                     '())])))])]))
