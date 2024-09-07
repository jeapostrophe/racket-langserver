#lang racket/base
(require drracket/check-syntax
         racket/class
         racket/contract/base
         racket/match
         racket/set
         racket/logging
         racket/list
         racket/string
         syntax/modread
         racket/sandbox
         setup/path-to-relative
         "editor.rkt"
         "responses.rkt"
         "interfaces.rkt"
         "autocomplete.rkt"
         "doc-trace.rkt")

(define ((error-diagnostics doc-text) exn)
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
                                      #:end   (Pos #:line (sub1 line) #:char (+ col span)))
                       #:severity Diag-Error
                       #:source "Racket"
                       #:message msg)
           ;; Some reader exceptions don't report a position
           ;; Use end of file as a reasonable guess
           (let ([end-of-file (abs-pos->Pos doc-text (send doc-text end-pos))])
             (Diagnostic #:range (Range #:start end-of-file
                                        #:end   end-of-file)
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

(define (get-indenter doc-text)
  (define text (send doc-text get-text))
  (define lang-info
    (with-handlers ([exn:fail:read? (lambda (e) 'missing)]
                    [exn:missing-module? (lambda (e) #f)])
      (read-language (open-input-string text) (lambda () 'missing))))
  (cond
    [(procedure? lang-info)
     (lang-info 'drracket:indentation #f)]
    [(eq? lang-info 'missing)
     ; check for a #reader directive at start of file, ignoring comments
     ; the ^ anchor here matches start-of-string, not start-of-line
     (if (regexp-match #rx"^(;[^\n]*\n)*#reader" text)
       #f ; most likely a drracket file, use default indentation
          ; (https://github.com/jeapostrophe/racket-langserver/issues/86)
       'missing)]
    [else #f]))

(define-syntax-rule (timeout time-sec body)
  (with-limits time-sec #f body))

(define (check-syntax src doc-text trace)
  (define indenter (get-indenter doc-text))
  (define ns (make-base-namespace))
  (define new-trace (new build-trace% [src src] [doc-text doc-text] [indenter indenter]))
  (match-define-values (src-dir _ #f)
    (split-path src))
  (define-values (add-syntax done)
    (make-traversal ns src-dir))

  ;; Rewind input port and read syntax
  (define text (send doc-text get-text))
  (define in (open-input-string text))
  (port-count-lines! in)
  (when trace
    (set-clear! (send trace get-warn-diags)))
  (define valid #f)
  (define lang-diag
    (if (eq? indenter 'missing)
        (list (Diagnostic #:range (Range #:start (Pos #:line 0 #:char 0) #:end (Pos #:line 0 #:char 0))
                          #:severity Diag-Error
                          #:source "Racket"
                          #:message "Missing or invalid #lang line"))
        (list)))
  (define diags (list))
  (define err-diags
    (parameterize ([current-annotations new-trace]
                   [current-namespace ns]
                   [current-load-relative-directory src-dir])
      (with-intercepted-logging
          (lambda (l)
            (define result (check-typed-racket-log doc-text l))
            (when (list? result) (set! diags (append result diags))))
        (lambda ()
          (with-handlers ([(or/c exn:fail:read?
                                 exn:fail:syntax?
                                 exn:fail:filesystem?
                                 exn:fail:resource?)
                           (error-diagnostics doc-text)])
            (define original-stx (with-module-reading-parameterization
                                   (λ () (read-syntax src in))))
            ;; 90 seconds limit for possible infinity recursive macro expand
            (define stx (timeout 90 (expand original-stx)))
            (define completions (append (set->list (walk stx)) (set->list (walk-module stx))))
            (send new-trace set-completions completions)
            (when trace
              (send trace set-completions completions))
            (add-syntax stx)
            (set! valid #t)
            (done)
            (list)))
        'info)))

  (define warn-diags (send new-trace get-warn-diags))

  ;; reuse old trace if check-syntax failed
  (list (if valid new-trace (or trace new-trace))
        (append err-diags (set->list warn-diags) lang-diag diags)))

(provide
 (contract-out
  [check-syntax (-> any/c (is-a?/c lsp-editor%) (or/c #f (is-a?/c build-trace%))
                    (list/c (is-a?/c build-trace%) any/c))]))
