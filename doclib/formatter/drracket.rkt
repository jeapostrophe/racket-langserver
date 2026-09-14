#lang racket/base

;; Headless DrRacket-compatible indentation. This module uses syntax-color's
;; public `color-textoid<%>` when available and an identical local interface
;; on older supported releases. It does not load framework, racket:text%, or
;; any GUI library. Scribble's published indentation hook lives in gui-lib, so
;; this backend substitutes a GUI-free port of `determine-spaces` for
;; Scribble-surface languages.

(require racket/class
         racket/contract
         racket/match
         "../../common/interfaces.rkt"
         "../lexer/snapshot.rkt"
         "scribble.rkt"
         "textoid.rkt")

(provide drracket-format-edits)

(struct Line-edit (line delete-amount insert-text) #:transparent)

(define (line-edit->text-edit edit)
  (match-define (struct* Line-edit ([line line]
                                    [delete-amount delete-amount]
                                    [insert-text insert-text]))
    edit)
  (TextEdit (Range (Pos line 0) (Pos line delete-amount)) insert-text))

(define hook-failed (gensym 'hook-failed))

(define racket-amount-to-indent
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (dynamic-require 'syntax-color/racket-indentation
                     'racket-amount-to-indent)))

(define (safe-reader-info content source-directory)
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (parameterize ([current-directory
                    (if source-directory
                        (path->complete-path source-directory)
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

(define (line-leading-whitespace-length textoid line)
  (define start (send textoid paragraph-start-position line))
  (define end (send textoid paragraph-end-position line))
  (for/fold ([count 0])
            ([position (in-range start end)]
             #:break (not (char-whitespace?
                            (send textoid get-character position))))
    (add1 count)))

(define (valid-substitution? value)
  (match value
    [(list (? exact-nonnegative-integer?) (? string?) _ ...) #t]
    [_ #f]))

(define (make-line-edit textoid line delete-amount insert-text)
  (define start (send textoid paragraph-start-position line))
  (define end (send textoid paragraph-end-position line))
  ;; Clamp to the line so LSP ranges stay in-line. DrRacket can delete past
  ;; the newline; simultaneous LSP apply cannot.
  (define bounded-delete (min delete-amount (- end start)))
  (define old-text (send textoid get-text start (+ start bounded-delete)))
  (and (not (string=? old-text insert-text))
       (Line-edit line bounded-delete insert-text)))

(define (edits-for-substitutions textoid first-line last-line substitutions)
  (reverse
    (for/fold ([edits '()])
              ([substitution (in-list substitutions)]
               [line (in-range first-line (add1 last-line))])
      (match-define (list delete-amount insert-text _ ...) substitution)
      (define edit
        (make-line-edit textoid line delete-amount insert-text))
      (if edit (cons edit edits) edits))))

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
       (call-hook racket-amount-to-indent textoid position)]
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
     (make-line-edit textoid
                     line
                     (line-leading-whitespace-length textoid line)
                     (make-string target #\space))]
    [else #f]))

(define (fallback-line-edits textoid first-line last-line indentation racket-fallback?)
  ;; DrRacket tabifies one paragraph at a time. Refreshing the textoid after
  ;; each replacement is important: the visual offset used for a later line
  ;; can depend on indentation corrected on an earlier line. The edit ranges
  ;; remain line-relative, so they are valid simultaneously against the
  ;; original document.
  (let loop ([current-textoid textoid]
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
         [(not edit)
          (loop current-textoid (add1 line) edits)]
         [else
          (match-define (struct* Line-edit ([delete-amount deleted] [insert-text inserted])) edit)
          (define next-textoid
            (textoid-replace-line-prefix! current-textoid line deleted inserted))
          (if next-textoid
              (loop next-textoid (add1 line) (cons edit edits))
              '())])])))

;; `#f` means the range hook is absent or declined, so line indentation
;; should run. A list, including '(), is the complete result: a failed or
;; invalid range hook must not fall through to per-line edits.
(define (range-hook-edits textoid range-indentation first-line last-line interactive?)
  (cond
    [(or interactive? (not (procedure? range-indentation))) #f]
    [else
     (define result
       (call-hook range-indentation
                  textoid
                  (send textoid paragraph-start-position first-line)
                  (send textoid paragraph-end-position last-line)))
     (cond
       [(eq? result hook-failed) '()]
       [(not result) #f]
       [(and (list? result) (andmap valid-substitution? result))
        (edits-for-substitutions textoid first-line last-line result)]
       [else '()])]))

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
  (let/ec finish
    (when (> start-line end-line)
      (finish '()))
    (define info (safe-reader-info text src-dir))
    (define indentation (language-indentation-hook info))
    (define range-indentation (safe-info-ref info 'drracket:range-indentation))
    (define hook-available?
      (or (procedure? indentation) (procedure? range-indentation)))
    (unless (or hook-available? racket-fallback?)
      (finish '()))
    (define parens (safe-info-ref info 'drracket:paren-matches))
    (define reusable-snapshot?
      (and lexer-snapshot
           racket-fallback?
           (not hook-available?)
           (string=? text (LexerSnapshot-text lexer-snapshot))))
    (define textoid
      (if reusable-snapshot?
          (make-textoid-from-lexer-snapshot lexer-snapshot
                                            #:source-directory src-dir
                                            #:paren-matches parens)
          (make-textoid text #:source-directory src-dir #:paren-matches parens)))
    (unless (and textoid (< start-line (send textoid paragraph-count)))
      (finish '()))
    (define last-line (min end-line (sub1 (send textoid paragraph-count))))
    (map line-edit->text-edit
         (or (range-hook-edits textoid range-indentation start-line last-line interactive?)
             (fallback-line-edits textoid start-line last-line indentation racket-fallback?)))))
