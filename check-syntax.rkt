#lang racket/base
(require data/interval-map
         drracket/check-syntax
         racket/class
         racket/contract/base
         racket/list
         racket/match
         racket/set
         syntax/modread
         "interfaces.rkt"
         "msg-io.rkt")

(define (path->uri path)
  (string-append "file://" path))

(define build-trace%
  (class (annotations-mixin object%)
    (init-field src)
    (define hovers (make-interval-map))
    ;; pos -> (set pos ...)
    (define sym-decls (make-interval-map))
    ;; pos -> pos
    (define sym-bindings (make-interval-map))
    ;; Getters
    (define/public (get-hovers) hovers)
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
    ;; References
    (define/override (syncheck:add-arrow start-src-obj start-left start-right
                                         end-src-obj end-left end-right
                                         actual? phase-level)
      (when (= start-left start-right)
        (set! start-right (add1 start-right)))
      (when (= end-left end-right)
        (set! end-right (add1 end-right)))
      ;; Mapping from doc declaration to set of bindings.
      (define prev-bindings (interval-map-ref sym-decls start-left set))
      (define new-bindings (set-add prev-bindings (cons end-left end-right)))
      (interval-map-set! sym-decls start-left start-right new-bindings)
      ;; Mapping from binding to declaration.
      (define new-decl (cons start-left start-right))
      (interval-map-set! sym-bindings end-left end-right new-decl)
      ;; Each decl is considered to be bound to itself
      ;; i.e. it is a key in map from doc positions to declarations.
      (interval-map-set! sym-bindings start-left start-right new-decl))
    (super-new)))

(define (diagnostics-message uri diags)
  (hasheq 'jsonrpc "2.0"
          'method "textDocument/publishDiagnostics"
          'params (hasheq 'uri uri
                          'diagnostics diags)))

(define ((report-syntax-error src) exn)
  (define msg (exn-message exn))
  (define get-srclocs (exn:srclocs-accessor exn))
  (define srclocs (get-srclocs exn))
  (define diags
    (for/list ([sl (in-list srclocs)])
      (match-define (srcloc src line col pos span) sl)
      (Diagnostic #:range (Range #:start (Pos #:line (sub1 line) #:char col)
                                 #:end   (Pos #:line (sub1 line) #:char (+ col span)))
                  #:severity 4
                  #:source "Racket"
                  #:message msg)))
  (display-message/flush
   (diagnostics-message (path->uri src) diags)))

(define (check-syntax src text)
  (define ns (make-base-namespace))
  (define trace (new build-trace% [src src]))
  (match-define-values (src-dir _ #f)
    (split-path src))
  (define-values (add-syntax done)
    (make-traversal ns src))
  (define in (open-input-string text))
  (port-count-lines! in)
  (parameterize ([current-annotations trace]
                 [current-namespace ns]
                 [current-load-relative-directory src-dir])
    (with-handlers ([(or/c exn:fail:read? exn:fail:syntax?)
                     (report-syntax-error src)])
      (define stx (with-module-reading-parameterization
                      (λ () (read-syntax src in))))
      (add-syntax (expand stx))
      (display-message/flush
       (diagnostics-message (path->uri src) empty)))
    (done))
  trace)

(provide
 (contract-out
  [check-syntax (any/c string? . -> . (is-a?/c build-trace%))]))
