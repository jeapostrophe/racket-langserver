#lang racket/base
;; Interface and protocol-facing data types.
;;
;; Placement rule:
;; - Put JSON-serializable structs here when they are part of LSP request/response
;;   payloads or otherwise cross protocol boundaries.
;; - Put shared interface structs here when they are consumed across module
;;   boundaries (for example formatting options and semantic-token metadata).
;; - JSON payload structs are expected to be encoded with `jsexpr-encode`.
;;
;; Internal-only runtime/domain structs belong in `internal-types.rkt`.
(require (for-syntax racket/base
                     syntax/parse)
         racket/class
         racket/contract/base
         racket/contract
         racket/match
         syntax/parse
         "json-util.rkt")

(provide (json-struct-out Pos)
         (json-struct-out Range)
         (json-struct-out TextEdit)
         (json-struct-out WorkspaceEdit)
         (json-struct-out CodeAction)
         (json-struct-out Diagnostic)
         (json-struct-out Location)
         (json-struct-out DocumentHighlight)
         (json-struct-out SymbolInformation)
         (json-struct-out Hover)
         (json-struct-out SignatureInformation)
         (json-struct-out SignatureHelp)
         (json-struct-out CompletionItem)
         (json-struct-out CompletionList)
         (json-struct-out ContentChangeEvent)
         (json-struct-out DocIdentifier)
         (json-struct-out DocItem)
         (json-struct-out InlayHint)
         (json-struct-out ConfigurationItem)
         (json-struct-out ConfigurationParams)
         FormattingOptions
         FormattingOptions-tab-size
         FormattingOptions-trim-trailing-whitespace
         as-FormattingOptions
         (struct-out SemanticToken)
         *semantic-token-types*
         *semantic-token-modifiers*
         abs-pos->Pos)

(define-json-struct Pos
  [line exact-nonnegative-integer?]
  [char exact-nonnegative-integer? #:json character])

(define-json-struct Range
  [start Pos?]
  [end Pos?])

(define-json-struct TextEdit
  [range Range?]
  [newText string?])

(define-json-struct WorkspaceEdit
  [changes hash?])

(define-json-struct CodeAction
  [title string?]
  [kind string?]
  [diagnostics any/c]
  [isPreferred boolean?]
  [edit any/c])

(define-json-struct Diagnostic
  [range Range?]
  [severity (or/c 1 2 3 4)]
  [source string?]
  [message string?])

(define-json-struct Location
  [uri string?]
  [range Range?])

(define-json-struct DocumentHighlight
  [range Range?])

(define-json-struct SymbolInformation
  [name string?]
  [kind exact-positive-integer?]
  [location Location?])

(define-json-struct Hover
  [contents string?]
  [range Range?])

(define-json-struct SignatureInformation
  [label string?]
  [documentation any/c])

(define-json-struct SignatureHelp
  [signatures (listof SignatureInformation?)])

(define-json-struct CompletionItem
  [label string?])

(define-json-struct CompletionList
  [isIncomplete boolean?]
  [items (listof CompletionItem?)])

(define-json-struct ContentChangeEvent
  [range any/c]
  [rangeLength exact-nonnegative-integer?]
  [text string?])

;; VersionedTextDocumentIdentifier
(define-json-struct DocIdentifier
  [version exact-nonnegative-integer?]
  [uri string?])

;; TextDocumentItem
(define-json-struct DocItem
  [uri string?]
  [languageId string?]
  [version exact-nonnegative-integer?]
  [text string?])

(define-json-struct InlayHint
  [position any/c]
  [label string?])

(define-json-struct ConfigurationItem
  [scopeUri string?]
  [section string?])

(define-json-struct ConfigurationParams
  [items (listof ConfigurationItem?)])

;; optional arguments used by FormattingOptions
(define undef-object (gensym 'undef))

(define (undef? x)
  (eq? x undef-object))

(define (undef/c pred?)
  (λ (x)
    (or/c (undef? x) (pred? x))))

(define uinteger-upper-limit (sub1 (expt 2 31)))

(define (uinteger? x)
  (and (integer? x) (<= 0 x uinteger-upper-limit)))

(define-json-struct FormattingOptions
  [tab-size uinteger? #:json tabSize]
  [insert-spaces boolean? #:json insertSpaces]
  [trim-trailing-whitespace (undef/c boolean?) #:json trimTrailingWhitespace]
  [insert-final-newline (undef/c boolean?) #:json insertFinalNewline]
  [trim-final-newlines (undef/c boolean?) #:json trimFinalNewlines]
  [key (or/c false/c (undef/c hash?))])

(define (jsexpr->FormattingOptions jsexpr)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (FormattingOptions #:tab-size (hash-ref jsexpr 'tabSize)
                       #:insert-spaces (hash-ref jsexpr 'insertSpaces)
                       #:trim-trailing-whitespace (hash-ref jsexpr 'trimTrailingWhitespace undef-object)
                       #:insert-final-newline (hash-ref jsexpr 'insertFinalNewline undef-object)
                       #:trim-final-newlines (hash-ref jsexpr 'trimFinalNewlines undef-object)
                       #:key (hash-ref jsexpr 'key undef-object))))

;; usage:
;; (match jsexpr
;;   [(as-FormattingOptions opts) ...])
(define-match-expander as-FormattingOptions
  (lambda (stx)
    (syntax-parse stx
      [(_ name)
       #'(and (? hash?)
              (app jsexpr->FormattingOptions (and name (not #f))))])))

(struct SemanticToken
  (start end type modifiers)
  #:transparent)

;; The order of this list is irrelevant.
;; The client receives this list from server ability declaration during
;; initialize handshake then use it to decode server semantic tokens messages.
;; Different order produces different encoding results of semantic tokens,
;; but does not affect client and server behavior.
;; To change the order, simply change it here, don't need to change other code.
(define *semantic-token-types*
  '(variable
     function
     string
     number
     regexp))

;; The order of this list is irrelevant, similar to *semantic-token-types*.
(define *semantic-token-modifiers*
  '(definition))

(define (abs-pos->Pos editor pos)
  (match-define (list line char) (send editor pos->line/char pos))
  (Pos #:line line #:char char))

