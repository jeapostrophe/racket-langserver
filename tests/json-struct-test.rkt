#lang racket/base

;; Verify standard `(struct-out ...)` works across module boundaries alongside `(json-out ...)`.
(module export-struct-out racket/base
  (require "../common/json-util.rkt")
  (define-json-struct ExportedStructOut [x exact-integer?])
  (provide (struct-out ExportedStructOut)))

(module export-json-out racket/base
  (require "../common/json-util.rkt")
  (define-json-struct ExportedJsonOut [x exact-integer?])
  (provide (json-out ExportedJsonOut)))

(module+ test
  (require json
           racket/list
           racket/match
           rackunit
           syntax/macro-testing
           "../common/json-util.rkt"
           (submod ".." export-struct-out)
           (submod ".." export-json-out))

  (define-json-struct Pos
    [line exact-integer?]
    [char exact-integer? #:json character])

  (define-json-struct Range
    [start Pos]
    [end Pos])

  (define-json-struct MaybePosHolder
    [pos (optional Pos)])

  (define-json-struct PosListHolder
    [items (listof Pos)])

  (define-json-struct PosHashHolder
    [items (hashof Pos)])

  (define-json-struct OrPosHolder
    [item (union string? Pos)])

  (define-json-struct MaybeOrPosHolder
    [item (optional (union string? Pos))])

  (define-json-struct OrPosListHolder
    [items (listof (union string? Pos))])

  (define-json-struct CustomContractHolder
    [item (contract integer?)])

  (define-json-struct NestedOptionalHolder
    [xs (listof (optional string?))]
    [u (union string? (optional exact-integer?))])

  (define-json-struct TreeNode
    [value string?]
    [children (listof TreeNode)])

  (define-json-struct Extensible
    [name string?]
    [enabled (optional boolean?)]
    #:rest extras)

  (define pos-1 (Pos 1 10))
  (define pos-2 (Pos #:char 20 #:line 2))
  (define p-hash (hasheq 'line 10 'character 5))
  (define p-hash-2 (hasheq 'line 3 'character 30))
  (define p-hash-invalid (hasheq 'line "not-an-int" 'character 5))
  (define range-val (Range pos-1 pos-2))
  (define r-hash (hasheq 'start p-hash 'end (hasheq 'line 20 'character 15)))

  (test-case "construction: positional, keyword, 0-arg all-optional, and first-class procedure"
    (check-equal? (Pos-line pos-1) 1)
    (check-equal? (Pos-char pos-1) 10)
    (check-equal? (Pos-line pos-2) 2)
    (check-equal? (Pos-char pos-2) 20)
    (check-true (procedure? Pos))
    (check-equal? (map Pos '(1 2) '(10 20)) (list (Pos 1 10) (Pos 2 20)))
    (check-equal? (MaybePosHolder) (MaybePosHolder omitted))
    (check-exn exn:fail:contract? (lambda () (Pos "1" 10)))
    (check-exn exn:fail:contract? (lambda () (Pos #:line 1 #:char "10")))
    (check-exn exn:fail:contract? (lambda () (map Pos '("bad") '(1))))
    (check-exn
      (lambda (e)
        (and (exn:fail:contract:arity? e)
             (regexp-match? #rx"^Pos:" (exn-message e))))
      (lambda () (Pos 1))))

  (test-case "keyword arguments evaluate once in caller order"
    (define observed '())
    (define (observe label value)
      (set! observed (append observed (list label)))
      value)
    (define value (Pos #:char (observe 'char 20) #:line (observe 'line 2)))
    (check-equal? observed '(char line))
    (check-equal? value (Pos 2 20)))

  (test-case "keyword constructors default omitted optional and rest fields"
    (define ext (Extensible #:name "tool"))
    (check-equal? ext (Extensible "tool" omitted (hasheq)))
    (check-equal? (->jsexpr ext) (hasheq 'name "tool"))
    (define ext2 (Extensible #:name "tool" #:enabled #f))
    (check-equal? ext2 (Extensible "tool" #f (hasheq)))
    (check-equal? (->jsexpr ext2) (hasheq 'name "tool" 'enabled #f)))

  (test-case "construction: syntax errors for invalid keyword usage and duplicates"
    (check-exn
      (lambda (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"missing keyword argument #:char" (exn-message e))))
      (lambda () (convert-compile-time-error (Pos #:line 1))))
    (check-exn
      (lambda (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"unknown keyword argument" (exn-message e))))
      (lambda () (convert-compile-time-error (Pos #:line 1 #:char 2 #:other 3))))
    (check-exn
      (lambda (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"mixed or malformed keyword arguments are not allowed" (exn-message e))))
      (lambda () (convert-compile-time-error (Pos 1 #:char 2))))
    (check-exn
      (lambda (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"duplicate keyword argument #:line" (exn-message e))))
      (lambda () (convert-compile-time-error (Pos #:line 1 #:line 2 #:char 3))))
    (check-exn
      (lambda (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"duplicate field" (exn-message e))))
      (lambda ()
        (convert-compile-time-error
          (define-json-struct DupField [x string?] [x exact-integer?]))))
    (check-exn
      (lambda (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"duplicate field" (exn-message e))))
      (lambda ()
        (convert-compile-time-error
          (define-json-struct DupRest [x string?] #:rest x))))
    (check-exn
      (lambda (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"duplicate json key" (exn-message e))))
      (lambda ()
        (convert-compile-time-error
          (define-json-struct DupJsonKey [a string? #:json k] [b string? #:json k])))))

  (test-case "encoding: simple, nested, optional, rest, and containers"
    (check-equal? (->jsexpr pos-1) (hasheq 'line 1 'character 10))
    (check-equal? (->jsexpr range-val)
                  (hasheq 'start (hasheq 'line 1 'character 10)
                          'end (hasheq 'line 2 'character 20)))
    (define-json-struct TagList [tags (listof string?)])
    (define tags (list "a" "b"))
    (check-eq? (hash-ref (->jsexpr (TagList tags)) 'tags) tags)
    (check-equal? (->jsexpr (PosListHolder (list pos-1 pos-2)))
                  (hasheq 'items (list (hasheq 'line 1 'character 10)
                                       (hasheq 'line 2 'character 20))))
    (check-equal? (->jsexpr (PosHashHolder (hasheq 'a pos-1)))
                  (hasheq 'items (hasheq 'a (hasheq 'line 1 'character 10))))
    ;; Nested optional values encode `omitted` to `(json-null)`, never leaking the sentinel.
    (define nested-encoded
      (->jsexpr (NestedOptionalHolder (list omitted "hi") omitted)))
    (check-true (jsexpr? nested-encoded))
    (check-equal? nested-encoded
                  (hasheq 'xs (list (json-null) "hi") 'u (json-null)))
    ;; Declared required fields override #:rest; omitted optional fields strip stale #:rest keys.
    (check-equal?
      (->jsexpr (Extensible #:name "declared"
                            #:enabled omitted
                            #:extras (hasheq 'name "extra" 'enabled "bogus" 'count 1)))
      (hasheq 'name "declared" 'count 1)))

  (test-case "contract: supports flat contracts and guards narrow-domain predicates"
    (define-json-struct FlatContracts
      [tag (contract 'ready)]
      [num (contract positive?)])
    (check-equal? (jsexpr-> FlatContracts (hasheq 'tag 'ready 'num 3))
                  (FlatContracts 'ready 3))
    (check-false (jsexpr-> FlatContracts (hasheq 'tag 'ready 'num "bad") #f))
    (check-false (match (hasheq 'tag 'ready 'num "bad") [(^FlatContracts _ _) #t] [_ #f]))
    (check-exn exn:fail:contract?
               (lambda () (jsexpr-> FlatContracts (hasheq 'tag 'other 'num 3)))))

  (test-case "decoding: jsexpr-> (strict, default, and 1-arg decoder) on structs and combinators"
    (check-equal? (jsexpr-> Pos p-hash) (Pos 10 5))
    (check-false (jsexpr-> Pos p-hash-invalid #f))
    (check-equal? (jsexpr-> Pos p-hash-invalid 'fallback) 'fallback)
    (define default-ran? #f)
    (check-equal? (jsexpr-> Pos p-hash (begin (set! default-ran? #t) 'unused))
                  (Pos 10 5))
    (check-false default-ran?)

    (define decode-pos (jsexpr-> Pos))
    (check-equal? (decode-pos p-hash) (Pos 10 5))
    (check-false (decode-pos p-hash-invalid #f))
    (check-equal? (decode-pos p-hash-invalid 'fallback) 'fallback)
    (check-equal? (map decode-pos (list p-hash p-hash-2))
                  (list (Pos 10 5) (Pos 3 30)))

    (check-equal? (jsexpr-> Range r-hash)
                  (Range (Pos 10 5) (Pos 20 15)))
    (check-equal? (jsexpr-> (listof Pos) (list p-hash p-hash-2))
                  (list (Pos 10 5) (Pos 3 30)))
    (check-false (jsexpr-> (listof Pos) (list p-hash "bad") #f))

    (check-equal? (jsexpr-> MaybePosHolder (hasheq)) (MaybePosHolder omitted))
    (check-equal? (jsexpr-> MaybePosHolder (hasheq 'pos p-hash)) (MaybePosHolder (Pos 10 5)))
    (check-equal? (jsexpr-> PosListHolder (hasheq 'items (list p-hash p-hash-2)))
                  (PosListHolder (list (Pos 10 5) (Pos 3 30))))
    (check-equal? (jsexpr-> PosHashHolder (hasheq 'items (hasheq 'a p-hash)))
                  (PosHashHolder (hasheq 'a (Pos 10 5))))
    (check-exn exn:fail:contract?
               (lambda () (->jsexpr (hash "foo" 42))))
    (check-exn exn:fail:contract?
               (lambda () (jsexpr-> (hashof exact-integer?) (hash "foo" 42))))
    (check-false (jsexpr-> (hashof exact-integer?) (hash "foo" 42) #f))
    (check-exn exn:fail:syntax?
               (lambda ()
                 (convert-compile-time-error
                   (jsexpr-> (hashof string? exact-integer?) (hash "foo" 42)))))
    (check-exn exn:fail:contract?
               (lambda () (PosHashHolder (make-hasheq (list (cons 'a pos-1))))))

    (check-equal? (jsexpr-> OrPosHolder (hasheq 'item "hello")) (OrPosHolder "hello"))
    (check-equal? (jsexpr-> OrPosHolder (hasheq 'item p-hash)) (OrPosHolder (Pos 10 5)))
    (check-equal? (jsexpr-> MaybeOrPosHolder (hasheq)) (MaybeOrPosHolder omitted))
    (check-equal? (jsexpr-> OrPosListHolder (hasheq 'items (list "a" p-hash)))
                  (OrPosListHolder (list "a" (Pos 10 5))))
    (check-equal? (jsexpr-> CustomContractHolder (hasheq 'item 1.0))
                  (CustomContractHolder 1.0)))

  (test-case "decoding: strict error paths for missing, invalid, non-hash, rest, and hashof"
    (check-exn
      (lambda (e)
        (and (exn:fail:contract? e)
             (regexp-match? #rx"failed to decode JSON value" (exn-message e))))
      (lambda () (jsexpr-> Pos 42)))
    (check-exn
      (lambda (e)
        (and (exn:fail:contract? e)
             (regexp-match? #rx"missing required field" (exn-message e))
             (regexp-match? #rx"field: 'char" (exn-message e))))
      (lambda () (jsexpr-> Pos (hasheq 'line 1))))
    (check-exn
      (lambda (e)
        (and (exn:fail:contract? e)
             (regexp-match? #rx"failed to decode field" (exn-message e))
             (regexp-match? #rx"field: 'pos" (exn-message e))))
      (lambda () (jsexpr-> MaybePosHolder (hasheq 'pos "bad"))))
    (check-exn exn:fail:contract?
               (lambda () (jsexpr-> CustomContractHolder (hasheq 'item "bad"))))
    (check-exn exn:fail:contract?
               (lambda () (jsexpr-> PosHashHolder (hasheq 'items "not-a-hash"))))
    (check-exn exn:fail:contract?
               (lambda () (Extensible "ok" omitted "not-a-hash"))))

  (test-case "decoding: rest fields and recursive structs"
    (define json (hasheq 'name "tool" 'enabled #t 'count 3 'metadata (hasheq 'stable #t)))
    (define decoded (jsexpr-> Extensible json))
    (check-equal? decoded
                  (Extensible "tool" #t (hasheq 'count 3 'metadata (hasheq 'stable #t))))
    (check-equal? (->jsexpr decoded) json)

    (define tree-json (hasheq 'value "root" 'children (list (hasheq 'value "leaf" 'children '()))))
    (check-equal? (jsexpr-> TreeNode tree-json)
                  (TreeNode "root" (list (TreeNode "leaf" '()))))
    (check-exn
      (lambda (e)
        (and (exn:fail:contract? e)
             (regexp-match? #rx"failed to decode field" (exn-message e))
             (regexp-match? #rx"type: 'TreeNode" (exn-message e))
             (regexp-match? #rx"field: 'children" (exn-message e))))
      (lambda ()
        (jsexpr-> TreeNode (hasheq 'value "root" 'children (list "bad"))))))

  (test-case "matching: struct patterns, jsexpr, and ^Name match expanders"
    (check-equal? (match pos-1 [(Pos #:line l) l] [_ #f]) 1)
    (check-equal? (match pos-2 [(Pos l c) (list l c)] [_ #f]) '(2 20))
    (check-false (match p-hash [(Pos #:line l) l] [_ #f]))

    (check-equal? (match p-hash [(jsexpr Pos pos) (Pos-line pos)] [_ #f]) 10)
    (check-equal? (match p-hash [(^Pos #:line l #:char c) (list l c)] [_ #f]) '(10 5))
    (check-equal? (match p-hash [(^Pos l c) (list l c)] [_ #f]) '(10 5))
    (check-false (match p-hash-invalid [(^Pos _ _) #t] [_ #f]))
    (check-false (match p-hash-invalid [(jsexpr Pos _) #t] [_ #f]))
    (check-exn exn:fail:syntax?
               (lambda ()
                 (convert-compile-time-error
                   (match p-hash [(^Pos pos) pos] [_ #f]))))
    (check-exn exn:fail:syntax?
               (lambda ()
                 (convert-compile-time-error
                   (match pos-1 [(Pos #:line l #:char) l] [_ #f]))))
    (check-exn exn:fail:syntax?
               (lambda ()
                 (convert-compile-time-error
                   (match p-hash [(^Pos #:line) #t] [_ #f]))))
    (check-false (match pos-1 [(jsexpr Pos _) #t] [_ #f]))
    (check-equal?
      (match r-hash
        [(jsexpr Range (Range #:start (Pos #:line sl #:char sc)
                              #:end (Pos #:line el #:char ec)))
         (list sl sc el ec)]
        [_ #f])
      '(10 5 20 15))
    (check-equal?
      (match (hasheq 'name "tool" 'count 3)
        [(^Extensible #:name name #:extras extras) (list name extras)]
        [_ #f])
      (list "tool" (hasheq 'count 3)))
    (check-equal?
      (match (list p-hash p-hash-2)
        [(jsexpr (listof Pos) ps) (map Pos-line ps)]
        [_ #f])
      '(10 3)))

  (test-case "jsexpr-has-key? handles non-hash intermediates safely"
    (check-true (jsexpr-has-key? (hasheq 'a (hasheq 'b 1)) '(a b)))
    (check-false (jsexpr-has-key? (hasheq 'a 1) '(a b)))
    (check-false (jsexpr-has-key? 42 '(a)))
    (check-true (jsexpr-has-key? 42 '())))

  (test-case "cross-module exports work with both struct-out and json-out"
    (check-equal? (ExportedStructOut-x (ExportedStructOut 10)) 10)
    (check-equal? (ExportedStructOut-x (ExportedStructOut #:x 20)) 20)
    (check-equal? (jsexpr-> ExportedStructOut (hasheq 'x 30)) (ExportedStructOut 30))
    (check-equal? (match (hasheq 'x 40) [(jsexpr ExportedStructOut (ExportedStructOut #:x x)) x] [_ #f])
                  40)

    (check-equal? (ExportedJsonOut-x (ExportedJsonOut 11)) 11)
    (check-equal? (jsexpr-> ExportedJsonOut (hasheq 'x 31)) (ExportedJsonOut 31))
    (check-false (jsexpr-> ExportedJsonOut (hasheq 'x "bad") #f))
    (check-equal? (match (hasheq 'x 41) [(jsexpr ExportedJsonOut ex) (ExportedJsonOut-x ex)] [_ #f])
                  41)
    (check-equal? (match (hasheq 'x 41) [(^ExportedJsonOut x) x] [_ #f]) 41)
    (check-equal? (match (hasheq 'x 41) [(^ExportedJsonOut #:x x) x] [_ #f]) 41)))
