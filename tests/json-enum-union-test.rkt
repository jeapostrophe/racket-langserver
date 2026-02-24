#lang racket
(require "../json-util.rkt" rackunit)

(module+ test
  ;; define-json-enum
  (define-json-enum SymbolKind
    [file 1]
    [module 2]
    [constant 14])

  (test-case "enum: construction and predicate"
    (check-true (SymbolKind? (SymbolKind 'constant)))
    (check-equal? (SymbolKind-v (SymbolKind 'file)) 'file))

  (test-case "enum: encoding and json predicate"
    (check-equal? (->jsexpr (SymbolKind 'constant)) 14)
    (check-equal? (->jsexpr (SymbolKind 'file)) 1)
    (check-true (SymbolKind-js? 14))
    (check-true (SymbolKind-js? 1))
    (check-false (SymbolKind-js? 999)))

  (test-case "enum: decode and json matcher"
    (check-equal? (jsexpr->SymbolKind 1) (SymbolKind 'file))
    (check-equal? (jsexpr->SymbolKind 14) (SymbolKind 'constant))
    (check-exn exn:fail? (lambda () (jsexpr->SymbolKind 999))))

  (test-case "enum: jsexpr match styles"
    (check-equal? (match 1 [(SymbolKind-js 'file) 'ok] [_ #f]) 'ok)
    (check-equal? (match 14 [(? SymbolKind-js? (SymbolKind-js k)) k] [_ #f]) 'constant))

  (test-case "enum: as-Name"
    (check-equal? (match 1 [(as-SymbolKind k) (SymbolKind-v k)] [_ #f]) 'file)
    (check-false (match 999 [(as-SymbolKind _) #t] [_ #f])))

  ;; define-json-union
  (define-json-struct Markup
    [kind string?]
    [value string?])

  (define-json-struct MarkupBox
    [payload Markup])

  ;; Union of primitive + compound
  (define-json-union DocContent string? Markup)

  (test-case "union: runtime predicate"
    (check-true (DocContent? "hello"))
    (check-true (DocContent? (Markup #:kind "markdown" #:value "# hi")))
    (check-false (DocContent? 42)))

  (test-case "union: json predicate"
    (check-true (DocContent-js? "hello"))
    (check-true (DocContent-js? (hasheq 'kind "markdown" 'value "# hi")))
    (check-false (DocContent-js? 42)))

  (test-case "union: decode and match expander"
    (check-equal? (jsexpr->DocContent "hello") "hello")
    (check-true (Markup? (jsexpr->DocContent (hasheq 'kind "markdown" 'value "# hi"))))
    (check-equal? (Markup-kind (jsexpr->DocContent (hasheq 'kind "markdown" 'value "# hi")))
                  "markdown")
    (check-exn exn:fail? (lambda () (jsexpr->DocContent 42)))
    (check-equal? (match "hi" [(DocContent-js v) v]) "hi")
    (check-true (Markup? (match (hasheq 'kind "md" 'value "x")
                           [(DocContent-js v) v]))))

  (test-case "union: as-Name"
    (check-equal? (match "ok" [(as-DocContent v) v] [_ #f]) "ok")
    (check-true (Markup? (match (hasheq 'kind "md" 'value "x")
                           [(as-DocContent v) v]
                           [_ #f])))
    (check-false (match 42 [(as-DocContent _) #t] [_ #f])))

  (test-case "union: decode pattern with as-Name"
    (check-equal?
      (match (hasheq 'kind "md" 'value "x")
        [(as-DocContent (Markup #:kind k #:value v)) (list k v)]
        [_ #f])
      '("md" "x"))
    (check-false
      (match "plain"
        [(as-DocContent (Markup #:kind _ #:value _)) #t]
        [_ #f])))

  (test-case "union: encode/decode nested struct field"
    (check-equal? (->jsexpr "hello") "hello")
    (check-equal? (->jsexpr (Markup #:kind "md" #:value "x"))
                  (hasheq 'kind "md" 'value "x"))
    (define markup-box-json
      (hasheq 'payload (hasheq 'kind "markdown" 'value "# hello")))
    (define decoded-box (jsexpr->MarkupBox markup-box-json))
    (check-true (Markup? (MarkupBox-payload decoded-box)))
    (check-equal? (->jsexpr decoded-box) markup-box-json)))

