#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         data/interval-map
         drracket/check-syntax
         json
         racket/class
         racket/contract/base
         racket/function
         racket/gui/base
         racket/list
         racket/match
         (only-in racket/string string-prefix?)
         syntax/modread
         syntax/parse
         "error-codes.rkt"
         "json-util.rkt"
         "msg-io.rkt"
         "responses.rkt")

(struct doc (text trace) #:transparent #:mutable)

(define (uri-is-path? str)
  (string-prefix? str "file://"))

(define (uri->path uri)
  (substring uri 7))

(define (path->uri path)
  (string-append "file://" path))

(define (abs-pos->Pos t pos)
  (define line (send t position-paragraph pos))
  (define line-begin (send t paragraph-start-position line))
  (define char (- pos line-begin))
  (Pos #:line line #:char char))

;;
;; Match Expanders
;;;;;;;;;;;;;;;;;;;;

(define-json-expander Range
  [start any/c]
  [end any/c])

(define-json-expander Location
  [uri string?]
  [range any/c])

(define-json-expander ContentChangeEvent
  [range any/c]
  [rangeLength exact-nonnegative-integer?]
  [text string?])

;; VersionedTextDocumentIdentifier
(define-json-expander DocIdentifier
  [version exact-nonnegative-integer?]
  [uri string?])

;; TextDocumentItem
(define-json-expander DocItem
  [uri string?]
  [languageId string?]
  [version exact-nonnegative-integer?]
  [text string?])

(define-match-expander Pos
  (λ (stx)
    (syntax-parse stx
      [(_ #:line l #:char c)
       (syntax/loc stx
         (hash-table ['line (? exact-nonnegative-integer? l)]
                     ['character (? exact-nonnegative-integer? c)]))]))
  (λ (stx)
    (syntax-parse stx
      [(_ #:line l #:char c)
       (syntax/loc stx
         (hasheq 'line l
                 'character c))])))

(define-json-expander Diagnostic
  [range any/c]
  [severity (or/c 1 2 3 4)]
  [source string?]
  [message string?])

(define-json-expander DocHighlight
  [range any/c]
  [kind (or/c 1 2 3)])

;;
;; Methods
;;;;;;;;;;;;

(define open-docs (make-hasheq))

(define (did-open! params)
  (match-define (hash-table ['textDocument (DocItem #:uri uri #:text text)]) params)
  (unless (uri-is-path? uri)
    ;; TODO: send user diagnostic or something
    (error 'did-open "uri is not a path."))
  (define path (uri->path uri))
  (define trace (check-syntax path text))
  (define doc-text (new text%))
  (send doc-text insert text 0)
  (hash-set! open-docs (string->symbol uri) (doc doc-text trace)))

(define (did-close! params)
  (match-define (hash-table ['textDocument (DocItem #:uri uri)]) params)
  (when (uri-is-path? uri)
    (hash-remove! open-docs (string->symbol uri))))

(define (did-change! params)
  (match-define (hash-table ['textDocument (DocIdentifier #:uri uri)]
                            ['contentChanges content-changes]) params)
  (when (uri-is-path? uri)
    (define d (hash-ref open-docs (string->symbol uri)))
    (define t (doc-text d))
    (define content-changes*
      (cond [(eq? (json-null) content-changes) empty]
            [(list? content-changes) content-changes]
            [else (list content-changes)]))
    (for ([change (in-list content-changes*)])
      (match change
        [(ContentChangeEvent #:range (Range #:start (Pos #:line st-ln #:char st-ch))
                             #:rangeLength range-ln
                             #:text text)
         (define st-pos (+ st-ch (send t paragraph-start-position st-ln)))
         (define end-pos (+ st-pos range-ln))
         (send t insert text st-pos end-pos)]
        [(ContentChangeEvent #:text text)
         ;; TODO: is erase slower than doing it all in one insert?
         (send t erase)
         (send t insert text 0)]))
    (define path (uri->path uri))
    (define new-trace (check-syntax path (send t get-text)))
    (eprintf "\n~a\n" (send t get-text))
    (set-doc-trace! d new-trace)))

;; Hover request
;; Returns an object conforming to the Hover interface, to
;; be used as the result of the response message. 
(define (hover id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char ch)])
     (unless (uri-is-path? uri)
       (error 'hover "uri is not a path"))
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define hovers (send doc-trace get-hovers))
     (define pos (+ ch (send doc-text paragraph-start-position line)))
     (define-values (start end text)
       (interval-map-ref/bounds hovers pos #f))
     (define result
       (cond [text
              (hasheq 'contents text
                      'range (Range #:start (abs-pos->Pos doc-text start)
                                    #:end   (abs-pos->Pos doc-text end)))]
             [else (hasheq 'contents empty)]))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/hover failed")]))

;; Reference request
;; Returns a list of Location objects representing arrows that would
;; be drawn by DrRacket.
(define (references id params)
  (match params
    [(hash-table ['textDocument (DocIdentifier #:uri uri)]
                 ['position (Pos #:line line #:char ch)]
                 #;['context (hash-table ['includeDeclaration decl])])
     (unless (uri-is-path? uri)
       (error 'references "uri is not a path"))
     (match-define (doc doc-text doc-trace)
       (hash-ref open-docs (string->symbol uri)))
     (define all-arrows (send doc-trace get-arrows))
     (define pos (+ ch (send doc-text paragraph-start-position line)))
     (define arrows (interval-map-ref all-arrows pos empty))
     (define result
       (for/list ([arrow (in-list arrows)])
         (match-define (cons start end) arrow)
         (Location #:uri uri
                   #:range (Range #:start (abs-pos->Pos doc-text start)
                                  #:end (abs-pos->Pos doc-text end)))))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/references failed")]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define build-trace%
  (class (annotations-mixin object%)
    (init-field src)
    (define hovers (make-interval-map))
    (define arrows (make-interval-map))
    ;; Getters
    (define/public (get-hovers) hovers)
    (define/public (get-arrows) arrows)
    ;; Overrides
    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))
    ;; Mouse-over status
    (define/override (syncheck:add-mouse-over-status src-obj start finish text)
      (interval-map-set! hovers start finish text))
    ;; Arrows
    (define/override (syncheck:add-arrow/name-dup start-src-obj start-left start-right
                                                  end-src-obj end-left end-right
                                                  actual? phase-lvl req-arrow? name-dup?)
      (define prevs (interval-map-ref arrows start-left list))
      (define ends (cons (cons end-left end-right) prevs))
      (interval-map-set! arrows start-left start-right ends))
    (super-new)))

(define (diagnostics-message uri diags)
  (hasheq 'jsonrpc "2.0"
          'method "textDocument/publishDiagnostics"
          'params (hasheq 'uri uri
                          'diagnostics diags)))

(define (report-syntax-error src exn)
  (define msg (exn-message exn))
  (eprintf "\nCaught error during traversal:\n~a\n" msg)
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

(define report-syntax-error* (curry report-syntax-error))

(define (check-syntax src text)
  (define ns (make-base-namespace))
  (define trace (new build-trace% [src src]))
  (match-define-values (src-dir _ #f)
    (split-path src))
  (define-values (add-syntax done)
    (make-traversal ns src))
  (define in-port (open-input-string text))
  (port-count-lines! in-port)
  (parameterize ([current-annotations trace]
                 [current-namespace ns]
                 [current-load-relative-directory src-dir])
    (with-handlers ([(or/c exn:fail:read? exn:fail:syntax?)
                     (report-syntax-error* src)])
      (define stx (with-module-reading-parameterization
                      (λ () (read-syntax src in-port))))
      (add-syntax (expand stx))
      (display-message/flush
       (diagnostics-message (path->uri src) empty)))
    (done))
  trace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [did-open! (jsexpr? . -> . void?)]
  [did-close! (jsexpr? . -> . void?)]
  [did-change! (jsexpr? . -> . void?)]
  [hover (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]
  [references (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]))