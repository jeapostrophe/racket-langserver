#lang racket/base
;; Interface and protocol-facing data types.
;;
;; Placement rule:
;; - Put JSON-serializable structs here when they are part of LSP request/response
;;   payloads or otherwise cross protocol boundaries.
;; - Put shared interface structs here when they are consumed across module
;;   boundaries (for example formatting options and semantic-token metadata).
;; - JSON payload structs are expected to be encoded with `->jsexpr`.
;;
;; Internal-only runtime/domain structs belong in `internal-types.rkt`.
(require racket/class
         racket/contract
         racket/match
         "json-util.rkt")

(provide (json-type-out Pos)
         (json-type-out Range)
         (json-type-out TextEdit)
         (json-type-out WorkspaceEdit)
         (json-type-out CodeAction)
         (json-type-out Diagnostic)
         (json-type-out Location)
         (json-type-out DocumentHighlight)
         (json-type-out SymbolInformation)
         (json-type-out Hover)
         (json-type-out SignatureInformation)
         (json-type-out SignatureHelp)
         (json-type-out CompletionItem)
         (json-type-out CompletionList)
         (json-type-out ContentChangeEvent)
         (json-type-out DocIdentifier)
         (json-type-out DocItem)
         (json-type-out InlayHint)
         (json-type-out ConfigurationItem)
         (json-type-out ConfigurationParams)
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
  [start Pos]
  [end Pos])

(define-json-struct TextEdit
  [range Range]
  [newText string?])

(define-json-struct WorkspaceEdit
  [changes (hash/c symbol? (listof TextEdit))])

(define-json-struct Diagnostic
  [range Range]
  [severity (or/c 1 2 3 4)]
  [source string?]
  [message string?])

(define-json-struct CodeAction
  [title string?]
  [kind string?]
  [diagnostics (listof Diagnostic)]
  [isPreferred boolean?]
  [edit WorkspaceEdit])

(define-json-struct Location
  [uri string?]
  [range Range])

(define-json-struct DocumentHighlight
  [range Range])

(define-json-struct SymbolInformation
  [name string?]
  [kind exact-positive-integer?]
  [location Location])

(define-json-struct Hover
  [contents string?]
  [range Range])

(define-json-struct SignatureInformation
  [label string?]
  [documentation string?])

(define-json-struct SignatureHelp
  [signatures (listof SignatureInformation)])

(define-json-struct CompletionItem
  [label string?])

(define-json-struct CompletionList
  [isIncomplete boolean?]
  [items (listof CompletionItem)])

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
  [items (listof ConfigurationItem)])

(define uinteger-upper-limit (sub1 (expt 2 31)))

(define (uinteger? x)
  (and (integer? x) (<= 0 x uinteger-upper-limit)))

(define-json-struct FormattingOptions
  [tab-size uinteger? #:json tabSize]
  [insert-spaces boolean? #:json insertSpaces]
  [trim-trailing-whitespace (maybe boolean?) #:json trimTrailingWhitespace]
  [insert-final-newline (maybe boolean?) #:json insertFinalNewline]
  [trim-final-newlines (maybe boolean?) #:json trimFinalNewlines]
  [key (or/c false/c (maybe/c hash?))])

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

