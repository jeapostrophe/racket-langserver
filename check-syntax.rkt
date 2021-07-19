#lang racket/base
(require drracket/check-syntax
         racket/class
         racket/contract/base
         racket/gui/base
         racket/match
         racket/set
         racket/logging
         racket/list
         racket/string
         net/url
         syntax/modread
         (only-in net/url path->url url->string)
         "msg-io.rkt"
         "responses.rkt"
         "interfaces.rkt"
         "autocomplete.rkt"
         "doc-trace.rkt")

(define path->uri (compose url->string path->url))

(define ((error-diagnostics src) exn)
  (define msg (exn-message exn))
  (cond
    ;; typed racket support: don't report error summaries
    [(string-prefix? msg "Type Checker: Summary") (list)]
    [(exn:srclocs? exn)
     (define srclocs ((exn:srclocs-accessor exn) exn))
     (for/list ([sl (in-list srclocs)])
       (match-define (srcloc src line col pos span) sl)
       (Diagnostic #:range (Range #:start (Pos #:line (sub1 line) #:char col)
                                  #:end   (Pos #:line (sub1 line) #:char (+ col span)))
                   #:severity Diag-Error
                   #:source "Racket"
                   #:message msg))]
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
  (define lang-info 
    (with-handlers ([exn:fail:read? (lambda (e) 'missing)]
                    [exn:missing-module? (lambda (e) #f)]) 
      (read-language (open-input-string (send doc-text get-text)) (lambda () 'missing))))
  (cond 
    [(procedure? lang-info)
     (lang-info 'drracket:indentation #f)]
    [(eq? lang-info 'missing) lang-info]
    [else #f]))

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
  (define warn-diags (send new-trace get-warn-diags))
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
            (when (list? result) (set! diags (append diags result))))
        (lambda () 
          (with-handlers ([(or/c exn:fail:read? exn:fail:syntax? exn:fail:filesystem?)
                           (error-diagnostics src)])
            (define stx (expand (with-module-reading-parameterization
                                  (λ () (read-syntax src in)))))
            ;; reading and expanding succeeded, clear out any syntax errors before the
            ;; heavy stuff in order to be responsive to the user
            (display-message/flush (diagnostics-message (path->uri src) (list)))
            (define completions (append (set->list (walk stx)) (set->list (walk-module stx))))
            (send new-trace set-completions completions)
            (when trace
              (send trace set-completions completions))
            (add-syntax stx)
            (set! valid #t)
            (done)
            (list)))
        'info)))
  (define all-diags (append err-diags (set->list warn-diags) lang-diag diags))
  (display-message/flush (diagnostics-message (path->uri src) all-diags))
  (if valid new-trace (or trace new-trace)))

(provide
 (contract-out
  [check-syntax (-> any/c (is-a?/c text%) (or/c #f (is-a?/c build-trace%)) (is-a?/c build-trace%))]))
