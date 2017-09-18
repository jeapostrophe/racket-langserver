#lang racket/base
(require (for-syntax racket/base)
         json
         racket/contract/base
         racket/list
         racket/match
         racket/string
         syntax/parse/define
         "unicode-util.rkt")
;;
;; Match Expanders
;;;;;;;;;;;;;;;;;;;;

(define-simple-macro
  (define-simple-match-expander (name:id args:id ...) pat-expr)
  (define-match-expander name
    (λ (stx)
      (syntax-case stx ()
        [(name args ...)
         #'pat-expr]))))

(define-simple-match-expander (Position Start End)
  (hash-table ['start (? number? Start)]
              ['end (? number? End)]))

(define-simple-match-expander (Range StLine StChar EndLine EndChar)
  (hash-table ['start (Position StLine StChar)]
              ['end (Position EndLine EndChar)]))

(define-simple-match-expander (TextDocumentItem Uri LanguageId Version Text)
  (hash-table ['uri (? string? Uri)] ;; TODO: not really a string...
              ['languageId (? string? LanguageId)]
              ['version (? number? Version)]
              ['text (? string? Text)]))

(define-simple-match-expander (ContentChangeEvent Text)
  (hash-table ['text (? string? Text)]))

(define-simple-match-expander
  (ContentChangeEvent+Range StLine StChar EndLine EndChar RangeLength Text)
  (hash-table ['range (Range StLine StChar EndLine EndChar)]
              ['rangeLength (? number? RangeLength)]
              ['text (? string? Text)]))

(define-simple-match-expander (VersionedTextDocumentIdentifier Version Uri)
  (hash-table ['version (? number? Version)]
              ['uri (? string? Uri)]))

;; TODO: This is what I *want* the syntax to look like
#;
(define-simple-macro (define-hash-pattern name [key ctc] ...+)
  (define-simple-match-expander (name ??? ...)
    (hash-table ['key (? ctc ???)] ...)))
#;
(define-hash-pattern TextDocumentItem_
  [uri string?]
  [languageId string?]
  [version number?]
  [text string?])

;;
;; Helpers
;;;;;;;;;;;;

(define doc-store/c (hash/c string? (listof string?)))

(define (string->lines str)
  (string-split str #rx"\n|(\r\n)|\r"))

;; The start-char and end-char values are specified as counting UTF-16 code points,
;; NOT characters or bytes or anything else that would be reasonable. As a result,
;; it is necessary to convert the lines being indexed by these values into UTF-16
;; byte arrays before indexing into them. These byte arrays, after being split,
;; are then immediately re-encoded into normal UTF-8 strings before being joined with
;; the new text. 
(define (range-edit doc-lines start-line start-char end-line end-char text)
  (let* ([before-lines (drop-right doc-lines (- (length doc-lines) start-line 1))]
         [last-before-line (utf-8->utf-16 (last before-lines))]
         [before-str (utf-16->utf-8 (subbytes last-before-line 0 (* 2 start-char)))]
         [after-lines (drop doc-lines end-line)]
         [first-after-line (utf-8->utf-16 (first after-lines))]
         [after-str (utf-16->utf-8 (subbytes first-after-line (* 2 end-char)))]
         [middle (string-append before-str text after-str)])
    (append (drop-right before-lines 1)
            (string->lines middle)
            (drop after-lines 1))))

;;
;; Methods
;;;;;;;;;;;;

(define (did-open open-docs params)
  (match params
    [(hash-table ['textDocument (TextDocumentItem uri language-id version text)])
     (hash-set open-docs uri (string->lines text))]
    [_
     (log-warning "invalid DidOpenTextDocumentParams: ~a" (jsexpr->string params))
     open-docs]))

(define (did-close open-docs params)
  (match params
    [(hash-table ['textDocument (hash-table ['uri (? string? uri)])])
     (hash-remove open-docs uri)]
    [_
     (log-warning "invalid DidCloseTextDocumentParams: ~a" (jsexpr->string params))
     open-docs]))

(define (did-change open-docs params)
  (match params
    [(hash-table ['textDocument (VersionedTextDocumentIdentifier version uri)]
                 ['contentChanges (list content-changes ...)])
     (define doc-lines (hash-ref open-docs uri #f))
     (cond
       [doc-lines
        (define changed-lines
          (for/fold ([doc-lines doc-lines])
                    ([change content-changes])
            (match change
              [(ContentChangeEvent+Range start-line start-char end-line end-char range-length text)
               ;; Range edit
               (range-edit doc-lines start-line start-char end-line end-char text)]
              [(ContentChangeEvent text)
               ;; Full replace
               (string->lines text)]
              [_
               (log-warning
                "invalid TextDocumentContentChangeEvent (Doc: ~v) (Ver: ~a) ~a"
                uri version (jsexpr->string change))
               doc-lines])))
        (hash-set open-docs uri changed-lines)]
       [else
        (log-warning "couldn't find document ~v (version: ~a)" uri version)
        doc-lines])]
    [_
     (log-warning "invalid DidChangeTextDocumentParams: ~a" (jsexpr->string params))
     open-docs]))

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
  [did-open (doc-store/c jsexpr? . -> . doc-store/c)]
  [did-close (doc-store/c jsexpr? . -> . doc-store/c)]
  [did-change (doc-store/c jsexpr? . -> . doc-store/c)])
 doc-store/c)