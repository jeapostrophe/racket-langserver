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

;;
;; Match Expanders
;;;;;;;;;;;;;;;;;;;;

(define-json-expander Range
  [start any/c]
  [end any/c])

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

;;
;; Methods
;;;;;;;;;;;;

(define open-docs (make-hasheq))

(define (did-open! params)
  (match-define (hash-table ['textDocument (DocItem #:uri uri #:text text)]) params)
  (unless (uri-is-path? uri)
    ;; TODO: send user diagnostic or something
    (error 'did-open "uri is not a path."))
  (define path (substring uri 7))
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
        [(ContentChangeEvent #:range (Range #:start (Pos #:line st-ln #:char st-ch)
                                            #:end   (Pos #:line end-ln #:char end-ch))
                             #:text text)
         (define st-pos (+ st-ch (send t paragraph-start-position st-ln)))
         (define end-pos (+ end-ch (send t paragraph-start-position end-ln)))
         (send t insert text st-pos end-pos)]
        [(ContentChangeEvent #:text text)
         ;; TODO: is erase slower than doing it all in one insert?
         (send t erase)
         (send t insert text 0)]))
    (define path (substring uri 7))
    (define new-trace (check-syntax path (send t get-text)))
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
     ;; TODO: is empty-string really the best default?
     (define text (interval-map-ref hovers pos "")) 
     ;; TODO: calculate range? (maybe there's a get-word-for-pos method?)
     (define result (hasheq 'contents text))
     (success-response id result)]
    [_
     (error-response id INVALID-PARAMS "textDocument/hover failed")]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define build-trace%
  (class (annotations-mixin object%)
    (init-field src)
    (define hovers (make-interval-map))
    ;; Getters
    (define/public (get-hovers) hovers)
    ;; Overrides
    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))
    (define/override (syncheck:add-mouse-over-status src-obj start finish text)
      (interval-map-set! hovers start finish text))
    (super-new)))

(require racket/exn)

(define (report-syntax-error src exn)
  (eprintf "\n~v\n\n" (exn->string exn))
  (define msg (exn-message exn))
  (define srclocs (exn:fail:read-srclocs exn))
  (define diags
    (for/list ([sl (in-list srclocs)])
      (match-define (srcloc src line col pos span) sl)
      (Diagnostic #:range (Range #:start (Pos #:line (sub1 line) #:char col)
                                 #:end   (Pos #:line (sub1 line) #:char (add1 col)))
                  #:severity 4
                  #:source "racket"
                  #:message msg)))
  (display-message/flush
   (hasheq 'jsonrpc "2.0"
           'method "textDocument/publishDiagnostics"
           'params (hasheq 'uri (format "file://~a" src)
                           'diagnostics diags))))

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
                 [current-directory src-dir])
    (with-handlers ([exn:fail:read? (report-syntax-error* src)])
      (define stx (with-module-reading-parameterization
                      (λ () (read-syntax src in-port))))
      (add-syntax (expand stx))
      (display-message/flush
       (hasheq 'jsonrpc "2.0"
               'method "textDocument/publishDiagnostics"
               'params (hasheq 'uri (format "file://~a" src)
                               'diagnostics empty))))
    (done))
  trace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [did-open! (jsexpr? . -> . void?)]
  [did-close! (jsexpr? . -> . void?)]
  [did-change! (jsexpr? . -> . void?)]
  [hover (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]))