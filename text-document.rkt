#lang racket/base
(require (for-syntax racket/base)
         json
         racket/contract/base
         racket/match
         racket/string
         syntax/parse/define)
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
  (hash-table ['version (? string? Version)]
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


(define doc-store/c (hash/c (cons/c string? number?) (listof string?)))

(define (string->lines str)
  (string-split str #rx"\r?\n"))

;;
;; Methods
;;;;;;;;;;;;

(define (did-open open-docs params)
  (match params
    [(hash-table ['textDocument (TextDocumentItem uri language-id verison text)])
     (hash-set open-docs (cons uri version) (string->lines text))]
    [_
     (log-warning "invalid textDocument/didOpen params: ~a" (jsexpr->string params))
     open-docs]))

(define (did-change open-docs params)
  (match params
    [(hash-table ['textDocument (VersionedTextDocumentIdentifier version uri)]
                 ['content-changes (list content-changes ...)])
     (for/fold ([ht open-docs])
               ([change content-changes])
       (match change
         [(ContentChangeEvent+Range start-line start-char end-line end-char range-length text)
          ;; TODO: range edit
          ht]
         [(ContentChangeEvent text)
          ;; Full replace
          (hash-set ht (cons uri version) (string->lines text))]
         [_
          (log-warning
           "invalid TextDocumentContentChangeEvent: ~a\n\tDocument: ~v Version: ~a"
           (jsexpr->string change) uri version)
          ht]))]
    [_
     (log-warning "invalid DidChangeTextDocumentParams: ~a" (jsexpr->string params))
     open-docs]))

(provide
 (contract-out
  [did-open (doc-store/c jsexpr? . -> . doc-store/c)]
  [did-change (doc-store/c jsexpr? . -> . doc-store/c)])
 doc-store/c)