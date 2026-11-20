#lang racket/base

(module+ test
  (require json
           racket/match
           rackunit
           syntax/macro-testing
           "../common/json-util.rkt")
  (define-json-enum SymbolKind
    [file 1]
    [module 2]
    [constant 14])

  (define-json-struct Markup
    [kind string?]
    [value string?])

  (define-json-union DocContent string? Markup)

  (define-json-struct DocBox
    [payload DocContent])

  (define-json-union NestedContent (optional (listof (union string? Markup))))
  (define-json-union TableContent (hashof Markup))

  (define-json-union RecursiveContent string? RecursiveNode)
  (define-json-struct RecursiveNode [children (listof RecursiveContent)])

  (test-case "enum: construction, predicate, aliases, and encoding"
    (check-true (SymbolKind? SymbolKind-constant))
    (check-equal? (SymbolKind-v SymbolKind-file) 'file)
    (check-eq? SymbolKind-file (SymbolKind 'file))
    (check-equal? (->jsexpr SymbolKind-constant) 14)
    (check-equal? (->jsexpr SymbolKind-file) 1)
    (check-exn exn:fail:contract? (lambda () (SymbolKind 'invalid)))
    (define eval-count 0)
    (define-json-enum EvalOnce
      [a (begin (set! eval-count (add1 eval-count)) "a")])
    (check-equal? eval-count 1)
    (check-equal? (->jsexpr EvalOnce-a) "a")
    (check-equal? (->jsexpr EvalOnce-a) "a")
    (check-equal? eval-count 1))

  (test-case "enum: decode and jsexpr / ^Name match expanders"
    (check-eq? (jsexpr-> SymbolKind 1) SymbolKind-file)
    (check-eq? ((jsexpr-> SymbolKind) 1) SymbolKind-file)
    (check-false ((jsexpr-> SymbolKind) 999 #f))
    (check-equal? (jsexpr-> SymbolKind 999 'none) 'none)
    (check-eq? (jsexpr-> SymbolKind 14) SymbolKind-constant)
    (check-exn exn:fail:contract? (lambda () (jsexpr-> SymbolKind 999)))
    (check-false (jsexpr-> SymbolKind 999 #f))
    (check-equal? (match 1 [(jsexpr SymbolKind (SymbolKind 'file)) 'ok] [_ #f]) 'ok)
    (check-equal? (match 1 [(^SymbolKind 'file) 'ok] [_ #f]) 'ok)
    (check-equal? (match 14 [(^SymbolKind tag) tag] [_ #f]) 'constant)
    (check-false (match 999 [(^SymbolKind _) #t] [_ #f])))

  (test-case "enum: duplicate variant and JSON value rejection (compile-time and runtime)"
    (check-exn
      (lambda (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"duplicate variant" (exn-message e))))
      (lambda () (convert-compile-time-error (define-json-enum DupVariant [a 1] [a 2]))))
    (check-exn
      (lambda (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"duplicate JSON value" (exn-message e))))
      (lambda () (convert-compile-time-error (define-json-enum DupValue [a 1] [b 1]))))
    (check-exn
      (lambda (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"reserved" (exn-message e))))
      (lambda () (convert-compile-time-error (define-json-enum ReservedV [v 1]))))
    (check-exn
      (lambda (e)
        (and (exn:fail? e)
             (regexp-match? #rx"duplicate JSON value" (exn-message e))))
      (lambda ()
        (let ()
          (define-json-enum NullDup [a (json-null)] [b (json-null)])
          NullDup-a))))

  (test-case "union: runtime predicate, decode, and jsexpr match"
    (check-true (DocContent? "hello"))
    (check-true (DocContent? (Markup #:kind "markdown" #:value "# hi")))
    (check-false (DocContent? 42))
    (check-equal? (jsexpr-> DocContent "hello") "hello")
    (check-equal? ((jsexpr-> DocContent) "hello") "hello")
    (check-false (jsexpr-> DocContent 42 #f))
    (check-equal? (jsexpr-> DocContent (hasheq 'kind "markdown" 'value "# hi"))
                  (Markup "markdown" "# hi"))
    (check-exn exn:fail:contract? (lambda () (jsexpr-> DocContent 42)))
    (check-equal? (match "ok" [(jsexpr DocContent v) v] [_ #f]) "ok")
    (check-equal?
      (match (hasheq 'kind "md" 'value "x")
        [(jsexpr DocContent (Markup #:kind k #:value v)) (list k v)]
        [_ #f])
      '("md" "x"))
    (check-false (match 42 [(jsexpr DocContent _) #t] [_ #f])))

  (test-case "union: optional omitted does not collide with decode failure"
    (check-true (NestedContent? omitted))
    (check-true (NestedContent? (list "text" (Markup "md" "value"))))
    (check-false (NestedContent? (list "text" 1)))
    (check-true (omitted? (match omitted [(jsexpr NestedContent v) v] [_ #f])))
    (check-false (match 42 [(jsexpr NestedContent _) #t] [_ #f])))

  (test-case "union: hashof and struct fields"
    (check-true (TableContent? (hasheq 'a (Markup "md" "value"))))
    (check-false (TableContent? (hasheq 'a "bad")))
    (check-false (TableContent? (make-hasheq (list (cons 'a (Markup "md" "value"))))))
    (define markup-json (hasheq 'payload (hasheq 'kind "md" 'value "x")))
    (check-equal? (jsexpr-> DocBox markup-json) (DocBox (Markup "md" "x")))
    (check-equal? (->jsexpr (DocBox "hello")) (hasheq 'payload "hello")))

  (test-case "recursive union and struct types resolve lazily"
    (define json (hasheq 'children (list "text" (hasheq 'children '()))))
    (define decoded (jsexpr-> RecursiveContent json))
    (check-equal? decoded (RecursiveNode (list "text" (RecursiveNode '()))))
    (check-true (RecursiveContent? decoded))
    (check-equal? (->jsexpr decoded) json)))
