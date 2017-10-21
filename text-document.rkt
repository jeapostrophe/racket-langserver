#lang racket/base
(require (for-syntax racket/base)
         json
         racket/contract/base
         racket/list
         racket/match
         racket/string
         rnrs/bytevectors-6
         syntax/parse/define
         "json-util.rkt")
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
(define-json-expander VersionedDocIdentifier
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
                     ['character (? exact-nonnegative-integer? c)]))])))

(define-json-expander Diagnostic
  [range any/c]
  [severity? (or/c 1 2 3 4)]
  [source string?]
  [message string?])

;;
;; Helpers
;;;;;;;;;;;;

(define doc-store? (hash/c symbol? (listof string?)))

(define (string->lines str)
  (string-split str #rx"\n|(\r\n)|\r"))

;; The start-char and end-char values are counting individual 16-bit UTF-16 code units,
;; not complete code points. Lines are converted into UTF-16 byte arrays before being
;; indexed. 
(define (range-edit doc-lines start-line start-char end-line end-char text)
  (let* ([before-lines (drop-right doc-lines (- (length doc-lines) start-line 1))]
         [last-before-line (string->utf16 (last before-lines))]
         ;; TODO: Endianness?
         [before-str (utf16->string (subbytes last-before-line 0 (* 2 start-char)) 'big)]
         [after-lines (drop doc-lines end-line)]
         [first-after-line (string->utf16 (first after-lines))]
         ;; TODO: Endianness?
         [after-str (utf16->string (subbytes first-after-line (* 2 end-char)) 'big)]
         [middle (string-append before-str text after-str)])
    (append (drop-right before-lines 1)
            (string->lines middle)
            (drop after-lines 1))))

(define (range-edit_ doc-lines start-line start-char end-line end-char text)
  #f)

;;
;; Methods
;;;;;;;;;;;;

(define (did-open open-docs params)
  (match params
    [(hash-table ['textDocument (DocItem #:uri uri #:text text)])
     (hash-set open-docs (string->symbol uri) (string->lines text))]))

(define (did-close open-docs params)
  (match params
    [(hash-table ['textDocument (hash-table ['uri (? string? uri)])])
     (hash-remove open-docs (string->symbol uri))]))

(define (did-change open-docs params)
  (match-define
    (hash-table ['textDocument (VersionedDocIdentifier #:version version #:uri uri)]
                ['contentChanges content-changes]) params)
  (define uri* (string->symbol uri))
  ;; Some clients (*cough* emacs) will return json-null instead of empty when
  ;; there are no  content changes. This also checks for the case where the
  ;; client returns a single atomic change, without nesting it in a list.
  (define content-changes*
    (cond [(eq? (json-null) content-changes) empty]
          [(list? content-changes) content-changes]
          [else (list content-changes)]))
  (define doc-lines (hash-ref open-docs uri*))
  (define changed-lines
    (for/fold ([doc-lines doc-lines])
              ([change content-changes*])
      (match change
        [(ContentChangeEvent #:range (Range #:start (Pos #:line st-ln #:char st-ch)
                                            #:end   (Pos #:line end-ln #:char end-ch))
                             #:text text)
         (range-edit doc-lines st-ln st-ch end-ln end-ch text)]
        [(ContentChangeEvent #:text text)
         (string->lines text)])))
  ;; TODO: Report diagnostics based on changed-lines. Even if there are no errors,
  ;; we still need to send an emtpy diagnostic notification to clear existing errors.
  #|
    (define diags
      (hasheq 'uri uri
              'diagnostics (list
                            (Diagnostic #:range Range|#
  (hash-set open-docs uri* changed-lines))

(provide
 (contract-out
  [string->lines (string? . -> . (listof string?))]
  [range-edit (-> (listof string?)
                  exact-nonnegative-integer?
                  exact-nonnegative-integer?
                  exact-nonnegative-integer?
                  exact-nonnegative-integer?
                  string?
                  (listof string?))]
  [did-open (doc-store? jsexpr? . -> . doc-store?)]
  [did-close (doc-store? jsexpr? . -> . doc-store?)]
  [did-change (doc-store? jsexpr? . -> . doc-store?)])
 doc-store?)