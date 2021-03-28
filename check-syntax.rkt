#lang racket/base
(require data/interval-map
         drracket/check-syntax
         racket/class
         racket/contract/base
         racket/gui/base
         racket/match
         racket/set
         racket/format
         racket/list
         syntax-color/module-lexer
         syntax-color/racket-lexer
         net/url
         syntax/modread
         (only-in net/url path->url url->string)
         "interfaces.rkt"
         "msg-io.rkt"
         "responses.rkt")

(define path->uri (compose url->string path->url))

(struct Decl (require? left right) #:transparent)

(define Diag-Error 1)
(define Diag-Warning 2)
(define Diag-Information 3)
(define Diag-Hint 4)

(define build-trace%
  (class (annotations-mixin object%)
    (init-field src doc-text indenter)
    (define warn-diags (mutable-seteq))
    (define hovers (make-interval-map))
    (define docs (make-interval-map))
    (define symbols (make-interval-map))
    ;; decl -> (set pos ...)
    (define sym-decls (make-interval-map))
    ;; pos -> decl
    (define sym-bindings (make-interval-map))
    ;; Getters
    (define/public (get-indenter) indenter)
    (define/public (get-warn-diags) warn-diags)
    (define/public (get-hovers) hovers)
    (define/public (get-docs) docs)
    (define/public (get-symbols) symbols)
    (define/public (get-sym-decls) sym-decls)
    (define/public (get-sym-bindings) sym-bindings)
    ;; Overrides
    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))
    ;; Mouse-over status
    (define/override (syncheck:add-mouse-over-status src-obj start finish text)
      ;; Infer a length of 1 for zero-length ranges in the document.
      ;; XXX This might not exactly match the behavior in DrRacket.
      (when (= start finish)
        (set! finish (add1 finish)))
      (interval-map-set! hovers start finish text))
    ;; Docs
    (define/override (syncheck:add-docs-menu text start finish id label path def-tag url-tag)
      (when url
        (when (= start finish)
          (set! finish (add1 finish)))
        (define url (path->url path))
        (define url2 (if url-tag
                         (make-url (url-scheme url)
                                   (url-user url)
                                   (url-host url)
                                   (url-port url)
                                   (url-path-absolute? url)
                                   (url-path url)
                                   (url-query url)
                                   url-tag)
                         url))
        (interval-map-set! docs start finish (url->string url2))))
    ;; References
    (define/override (syncheck:add-arrow/name-dup start-src-obj start-left start-right
                                                  end-src-obj end-left end-right
                                                  actual? phase-level
                                                  require-arrow? name-dup?)
      (when (= start-left start-right)
        (set! start-right (add1 start-right)))
      (when (= end-left end-right)
        (set! end-right (add1 end-right)))
      ;; Mapping from doc declaration to set of bindings.
      (define prev-bindings (interval-map-ref sym-decls start-left set))
      (define new-bindings (set-add prev-bindings (cons end-left end-right)))
      (interval-map-set! sym-decls start-left start-right new-bindings)
      ;; Mapping from binding to declaration.
      (define new-decl (Decl require-arrow? start-left start-right))
      (interval-map-set! sym-bindings end-left end-right new-decl))
    ;; Unused requires
    (define/override (syncheck:add-unused-require src left right)
      (define diag (Diagnostic #:range (Range #:start (abs-pos->Pos doc-text left)
                                              #:end   (abs-pos->Pos doc-text right))
                               #:severity Diag-Information
                               #:source "Racket"
                               #:message "unused require"))
      (set-add! warn-diags diag))
    (super-new)))

(define ((error-diagnostics src) exn)
  (define msg (exn-message exn))
  (cond
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

;; XXX Want to use read-language for this, but can't just assume this
;; XXX is the first line because there might be comments.
;; XXX Look into ways to efficiently read from doc-text like a port.
;; XXX (This would make get-lexer faster too.)
;; XXX For now, use default indentation for everything (until we support
;; XXX custom #langs).
(define (get-indenter doc-text)
  (define lang-info (read-language (open-input-string (send doc-text get-text))))
  (lang-info 'drracket:indentation #f))

(define (check-syntax src doc-text)
  (define indenter (get-indenter doc-text))
  (define ns (make-base-namespace))
  (define trace
    (new build-trace% [src src] [doc-text doc-text] [indenter indenter]))
  (match-define-values (src-dir _ #f)
    (split-path src))
  (define-values (add-syntax done)
    (make-traversal ns src))
  (define in (open-input-string (send doc-text get-text)))
  (port-count-lines! in)
  (define lexer (get-lexer in))
  (define symbols (send trace get-symbols))
  (for ([lst (in-port (lexer-wrap lexer) in)] #:when (set-member? '(constant string symbol) (first (rest lst))))
    (match-define (list text type paren? start end) lst)
    (interval-map-set! symbols start end (list text type)))
  
  (define err-diags
    (parameterize ([current-annotations trace]
                   [current-namespace ns]
                   [current-load-relative-directory src-dir])
      (with-handlers ([(or/c exn:fail:read? exn:fail:syntax? exn:fail:filesystem?)
                       (error-diagnostics src)])
        (define stx (with-module-reading-parameterization
                      (λ () (read-syntax src in))))
        (add-syntax (expand stx))
        (done)
        (list))))
  (define all-diags (append err-diags (set->list (send trace get-warn-diags))))
  (display-message/flush (diagnostics-message (path->uri src) all-diags))
  trace)

;; Wrapper for in-port, returns a list or EOF.
(define ((lexer-wrap lexer) in)
  (define-values (txt type paren? start end)
    (lexer in))
  (if (eof-object? txt)
      eof
      (list txt type paren? start end)))

;; Call module-lexer on an input port, then discard all
;; values except the lexer.
(define (get-lexer in)
  (match-define-values
    (_ _ _ _ _ _ lexer)
    (module-lexer in 0 #f))
  (if (procedure? lexer) ;; TODO: Is this an issue with module-lexer docs?
      lexer
      (if (eq? lexer 'no-lang-line)
          racket-lexer
          (error 'get-lexer "~v" lexer))))

(provide
 (contract-out
  [struct Decl ([require? any/c]
                [left exact-nonnegative-integer?]
                [right exact-nonnegative-integer?])]
  [check-syntax (-> any/c (is-a?/c text%) (is-a?/c build-trace%))]))
