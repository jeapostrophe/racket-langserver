#lang racket/base

(require "interface.rkt"
         racket/class
         racket/string
         racket/set
         racket/sandbox
         racket/match
         racket/list
         setup/path-to-relative
         data/interval-map
         "../interfaces.rkt"
         "../responses.rkt"
         "../path-util.rkt"
         "../settings.rkt"
         drracket/check-syntax
         "resyntax.rkt")

(provide diag%)

(define diag%
  (class base-service%
    (init-field src doc-text indenter)
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

    (define/override (walk-stx stx expanded-stx)
      (when (eq? indenter 'missing)
        (add-diag!
          (Diagnostic #:range (Range #:start (Pos #:line 0 #:char 0)
                                     #:end (Pos #:line 0 #:char 0))
                      #:severity Diag-Error
                      #:source "Racket"
                      #:message "Missing or invalid #lang line")))
      (when (exn? stx)
        (add-diags! (error-diagnostics doc-text stx)))
      (when (exn? expanded-stx)
        (add-diags! (error-diagnostics doc-text expanded-stx))))

    (define/override (walk-log logs)
      (for ([log (in-list logs)])
        (define result (check-typed-racket-log doc-text log))
        (when (list? result)
          (add-diags! result))))

    (define/override (walk-text text)
      (when (get-resyntax-enabled)
        (resyntax text doc-text src diags quickfixs)))

    (define/override (syncheck:add-mouse-over-status src-obj start finish text)
      (when (string=? "no bound occurrences" text)
        (hint-unused-variable src-obj start finish)))

    ;; Mouse-over status
    (define (hint-unused-variable src-obj start finish)
      (unless (string=? "_" (send doc-text get-text start (add1 start)))
        (define diag
          (Diagnostic #:range (Range #:start (abs-pos->Pos doc-text start)
                                     #:end (abs-pos->Pos doc-text finish))
                      #:severity Diag-Information
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
                               #:severity Diag-Information
                               #:source "Racket"
                               #:message "unused require"))
      (add-diag! diag))))

(define (error-diagnostics doc-text exn)
  (define msg (exn-message exn))
  (cond
    ;; typed racket support: don't report error summaries
    [(string-prefix? msg "Type Checker: Summary") (list)]
    [(exn:fail:resource? exn)
     (list (Diagnostic #:range (Range #:start (Pos #:line 0 #:char 0)
                                      #:end (Pos #:line 0 #:char 0))
                       #:severity Diag-Hint
                       #:source "Expander"
                       #:message "the expand time has exceeded the 90s limit.\
                        Check if your macro is infinitely expanding"))]
    [(and (exn:fail:read? exn)
          ;; Looking for the pattern '... version mismatch ... .zo ... raco setup ...'
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
     (list (Diagnostic #:range (Range #:start (Pos #:line 0 #:char 0)
                                      #:end (Pos #:line 0 #:char 0))
                       #:severity Diag-Error
                       #:source "Racket"
                       #:message expanded-msg))]
    [(exn:srclocs? exn)
     (define srclocs ((exn:srclocs-accessor exn) exn))
     (for/list ([sl (in-list srclocs)])
       (match-define (srcloc src line col pos span) sl)
       (if (and (number? line) (number? col) (number? span))
           (Diagnostic #:range (Range #:start (Pos #:line (sub1 line) #:char col)
                                      #:end (Pos #:line (sub1 line) #:char (+ col span)))
                       #:severity Diag-Error
                       #:source "Racket"
                       #:message msg)
           ;; Some reader exceptions don't report a position
           ;; Use end of file as a reasonable guess
           (let ([end-of-file (abs-pos->Pos doc-text (send doc-text end-pos))])
             (Diagnostic #:range (Range #:start end-of-file
                                        #:end end-of-file)
                         #:severity Diag-Error
                         #:source "Racket"
                         #:message msg))))]
    [(exn:missing-module? exn)
     ;; Hack:
     ;; We do not have any source location for the offending `require`, but the language
     ;; server protocol requires a valid range object.  So we punt and just highlight the
     ;; first character.
     ;; This is very close to DrRacket's behavior:  it also has no source location to work with,
     ;; however it simply doesn't highlight any code.
     (define silly-range
       (Range #:start (Pos #:line 0 #:char 0) #:end (Pos #:line 0 #:char 0)))
     (list (Diagnostic #:range silly-range
                       #:severity Diag-Error
                       #:source "Racket"
                       #:message msg))]
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
                          #:severity Diag-Error
                          #:source "Typed Racket"
                          #:message msg))))))

