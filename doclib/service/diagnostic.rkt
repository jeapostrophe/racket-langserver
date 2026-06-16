#lang racket/base

(require "interface.rkt"
         racket/class
         racket/string
         racket/set
         racket/match
         racket/list
         setup/path-to-relative
         data/interval-map
         "../../common/interfaces.rkt"
         "../internal-types.rkt"
         "../doc-lang.rkt"
         "../../common/path-util.rkt"
         drracket/check-syntax)

(provide diag%)

(define diag%
  (class base-service%
    (init-field src doc-text)
    (super-new)

    (define diags (mutable-seteq))

    (define (add-diag! d)
      (set-add! diags d))

    (define (add-diags! ds)
      (for ([d (in-list ds)])
        (add-diag! d)))

    (define quickfixs (make-interval-map))

    (define/override (get)
      (list diags quickfixs))

    (define/override (reset)
      (set-clear! diags)
      (set! quickfixs (make-interval-map)))

    (define/override (walk-stx expand-result)
      (define pre-exn (ExpandResult-pre-exn expand-result))
      (define post-exn (ExpandResult-post-exn expand-result))
      (define maybe-language-diag
        (and (requires-language-declaration? src)
             (language-diagnostic doc-text)))
      (when maybe-language-diag
        (add-diag! maybe-language-diag))
      (when pre-exn
        (add-diags! (error-diagnostics doc-text pre-exn)))
      (when post-exn
        (add-diags! (error-diagnostics doc-text post-exn))))

    (define/override (walk-log logs)
      (for ([log (in-list logs)])
        (define result (check-typed-racket-log doc-text log))
        (when (list? result)
          (add-diags! result))))

    (define/override (syncheck:add-mouse-over-status src-obj start finish text)
      (when (and (< start finish)
                 (string=? "no bound occurrences" text))
        (hint-unused-variable src-obj start finish)))

    ;; Mouse-over status
    (define (hint-unused-variable src-obj start finish)
      (unless (string=? "_" (send doc-text get-text start (add1 start)))
        (define diag
          (Diagnostic #:range (Range #:start (abs-pos->Pos doc-text start)
                                     #:end (abs-pos->Pos doc-text finish))
                      #:severity DiagnosticSeverity-Information
                      #:source (path->uri src-obj)
                      #:message "unused variable"))

        (define code-action
          (CodeAction
            #:title "Add prefix `_` to ignore"
            #:kind "quickfix"
            #:diagnostics (list diag)
            #:isPreferred #f
            #:edit (WorkspaceEdit
                     #:changes
                     (hasheq (string->symbol (path->uri src-obj))
                             (list (TextEdit #:range (Range #:start (abs-pos->Pos doc-text start)
                                                            #:end (abs-pos->Pos doc-text start))
                                             #:newText "_"))))))

        (interval-map-set! quickfixs start (add1 finish) code-action)
        (add-diag! diag)))

    (define/override (syncheck:add-unused-require _src left right)
      (define diag (Diagnostic #:range (Range #:start (abs-pos->Pos doc-text left)
                                              #:end (abs-pos->Pos doc-text right))
                               #:severity DiagnosticSeverity-Information
                               #:source "Racket"
                               #:message "unused require"))
      (add-diag! diag))))

(define language-diagnostic-source "Language Declaration Check")

(define (language-diagnostic doc-text)
  (define text (send doc-text get-text))
  (define maybe-language-prefix (parse-language-prefix text))
  (cond
    [(not maybe-language-prefix)
     (language-error-diag
       (first-line-range doc-text)
       "Missing language header. Start the file with `#lang <language>`, `#reader <reader>`, or `(module <name> <language> ...)`.")]
    [(language-declaration-malformed?
       (Language-Prefix-declaration maybe-language-prefix))
     (language-error-diag
       (language-prefix-range doc-text maybe-language-prefix)
       "Incomplete language header. Provide the missing language or reader name.")]
    [else #f]))

(define (language-error-diag range message)
  (Diagnostic #:range range
              #:severity DiagnosticSeverity-Error
              #:source language-diagnostic-source
              #:message message))

(define (empty-range? range)
  (define start (Range-start range))
  (define end (Range-end range))
  (and (= (Pos-line start) (Pos-line end))
       (= (Pos-char start) (Pos-char end))))

(define (nonempty-diagnostic-range doc-text range)
  (if (empty-range? range)
      (first-line-range doc-text)
      range))

(define (first-line-range doc-text)
  (define start (send doc-text line-start-pos 0))
  (define end (send doc-text line-end-pos 0))
  (Range #:start (abs-pos->Pos doc-text start)
         #:end (abs-pos->Pos doc-text end)))

(define (language-prefix-range doc-text language-prefix)
  (nonempty-diagnostic-range
    doc-text
    (Range #:start (abs-pos->Pos doc-text (Language-Prefix-start-pos language-prefix))
           #:end (abs-pos->Pos doc-text (Language-Prefix-end-pos language-prefix)))))

(define (error-diagnostics doc-text exn)
  (define msg (exn-message exn))
  (cond
    ;; Typed Racket reports each type error separately, then emits a final
    ;; "Type Checker: Summary" message that repeats the error count. The
    ;; per-error diagnostics are more useful, so do not publish the summary.
    [(string-prefix? msg "Type Checker: Summary") (list)]
    ;; A .zo file was compiled by a different Racket version. The original
    ;; reader error is hard to act on, so rewrite it into a recompilation
    ;; suggestion for the library or compiled file named in the message.
    [(and (exn:fail:read? exn)
          (regexp-match? #rx"version mismatch.*\\.zo.*raco setup" msg))
     (define maybe-error-source-mtchs/f
       (regexp-match #rx"in: (.*\\.zo)" msg))
     (define maybe-lib-error-source/f
       (and maybe-error-source-mtchs/f
            (path->relative-string/library (list-ref maybe-error-source-mtchs/f 1))))
     (define expanded-msg
       (cond
         [maybe-lib-error-source/f
          (format (string-append
                    "Failed to load file because it is compiled by a different version of Racket.\n"
                    "  file: ~a\n"
                    "  suggestion: ~a\n"
                    "  complete message:\n\n  ~a")
                  maybe-lib-error-source/f
                  (cond
                    [(regexp-match? #rx"<[a-z-]+>/" maybe-lib-error-source/f)
                     "run `raco setup` to recompile the libraries."]
                    [(regexp-match #rx"^(.*)compiled/([^/]+)_([^_]+)\\.zo$" maybe-lib-error-source/f)
                     => (match-lambda
                          [(list whole-str base-path filename file-extension)
                           (format "run `raco make '~a~a.~a'` to recompile it."
                                   base-path
                                   filename
                                   file-extension)])]
                    [else
                     (format "try running `raco setup` or `raco make <file>`.")])
                  msg)]
         [else msg]))
     ;; stub range -- see the comments in exn:missing-module?
     (list (Diagnostic #:range (first-line-range doc-text)
                       #:severity DiagnosticSeverity-Error
                       #:source "Racket"
                       #:message expanded-msg))]
    ;; Most reader, expander, and typechecker errors carry one or more srclocs.
    ;; Turn each reported srcloc into a diagnostic, falling back to the first
    ;; line only when Racket reports the error without a usable position.
    [(exn:srclocs? exn)
     (define srclocs ((exn:srclocs-accessor exn) exn))
     (for/list ([sl (in-list srclocs)])
       (match-define (srcloc src line col pos span) sl)
       (if (and (number? line) (number? col) (number? span))
           (Diagnostic #:range (nonempty-diagnostic-range
                                 doc-text
                                 (Range #:start (Pos #:line (sub1 line) #:char col)
                                        #:end (Pos #:line (sub1 line) #:char (+ col span))))
                       #:severity DiagnosticSeverity-Error
                       #:source "Racket"
                       #:message msg)
           ;; Some reader exceptions don't report a position
           (Diagnostic #:range (first-line-range doc-text)
                       #:severity DiagnosticSeverity-Error
                       #:source "Racket"
                       #:message msg)))]
    ;; Missing module exceptions tell us which module could not be loaded, but
    ;; not which `require` form in the user's file triggered the lookup. Publish
    ;; the message at a document-level range instead of guessing the require.
    [(exn:missing-module? exn)
     ;; Hack:
     ;; We do not have any source location for the offending `require`, but the language
     ;; server protocol requires a valid range object. So highlight the first line.
     ;; This is very close to DrRacket's behavior:  it also has no source location to work with,
     ;; however it simply doesn't highlight any code.
     (list (Diagnostic #:range (first-line-range doc-text)
                       #:severity DiagnosticSeverity-Error
                       #:source "Racket"
                       #:message msg))]
    ;; If a new kind of expansion failure reaches this point, fail loudly so we
    ;; can decide how to map it into an LSP diagnostic.
    [else (error 'error-diagnostics "unexpected failure: ~a" exn)]))

(define (check-typed-racket-log doc-text log)
  (match-define (vector _ msg data _) log)
  (when (and (list? data) (not (empty? data)) (syntax? (car data)))
    (define prop (syntax-property (car data) 'mouse-over-tooltips))
    (when (and prop (list? prop) (not (empty? prop)))
      (define-values (start end msg)
        (match prop
          [(list (vector _ start _ _) (vector _ _ end msg))
           (values start end msg)]
          [(list (vector _ start end msg))
           (values start end msg)]
          [else (values #f #f #f)]))
      (when (string? msg)
        (list (Diagnostic #:range (Range #:start (abs-pos->Pos doc-text start) #:end (abs-pos->Pos doc-text end))
                          #:severity DiagnosticSeverity-Error
                          #:source "Typed Racket"
                          #:message msg))))))
