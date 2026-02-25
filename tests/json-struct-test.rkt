#lang racket
(require "../json-util.rkt" rackunit syntax/macro-testing)

(module+ test
  ;; Define test structs
  (define-json-struct Pos
    [line integer?]
    [char integer? #:json character])

  (define p1 (Pos 1 10))
  (define p2 (Pos #:char 20 #:line 2))

  (define-json-struct Range
    [start Pos]
    [end Pos])

  (define-json-struct MaybePosHolder
    [pos (optional Pos)])

  (define-json-struct PosListHolder
    [items (listof Pos)])

  (define-json-struct PosHashHolder
    [items (hash/c symbol? Pos)])

  (define-json-struct OrPosHolder
    [item (or/c string? Pos)])

  (define-json-struct MaybeOrPosHolder
    [item (optional (or/c string? Pos))])

  (define-json-struct OrPosListHolder
    [items (listof (or/c string? Pos))])

  (define p-hash (hasheq 'line 10 'character 5))
  (define p-hash-2 (hasheq 'line 3 'character 30))
  (define p-hash-invalid (hasheq 'line "not-an-int" 'character 5))
  (define r (Range p1 p2))
  (define r-hash (hasheq 'start p-hash 'end (hasheq 'line 20 'character 15)))

  (test-case "construction: positional and keyword"
    (check-equal? (Pos-line p1) 1)
    (check-equal? (Pos-char p1) 10)
    (check-equal? (Pos-line p2) 2)
    (check-equal? (Pos-char p2) 20))

  (test-case "construction: contract enforcement"
    (check-exn exn:fail:contract? (λ () (Pos "1" 10)))
    (check-exn exn:fail:contract? (λ () (Pos #:line 1 #:char "10"))))

  (test-case "construction: syntax errors for mixed and duplicate keywords"
    (check-exn
      (λ (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"mixed positional and keyword arguments are not allowed"
                            (exn-message e))))
      (λ ()
        (convert-compile-time-error
          (Pos 1 #:char 2))))
    (check-exn
      (λ (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"duplicate keyword argument #:line"
                            (exn-message e))))
      (λ ()
        (convert-compile-time-error
          (Pos #:line 1 #:line 2 #:char 3)))))

  (test-case "matching: Pos matches structs only"
    (check-equal?
      (match p1 [(Pos #:line l) l] [_ #f])
      1)
    (check-equal?
      (match p2 [(Pos l c) (list l c)] [_ #f])
      '(2 20))
    (check-equal?
      (match p-hash [(Pos #:line l) l] [_ #f])
      #f))

  (test-case "matching: Pos-js matches hashes"
    (check-equal?
      (match p-hash [(Pos-js #:line l) l] [_ #f])
      10)
    (check-equal?
      (match p-hash-2 [(Pos-js l c) (list l c)] [_ #f])
      '(3 30))
    (check-equal?
      (match p1 [(Pos-js l c) (list l c)] [_ #f])
      #f))

  (test-case "validation: Pos-js? validates JSON hash only"
    (check-true (Pos-js? p-hash))
    (check-false (Pos-js? p1))
    (check-false (Pos-js? p-hash-invalid)))

  (test-case "matching: as-Pos decodes valid JSON hash"
    (check-equal?
      (match p-hash
        [(as-Pos pos) (Pos-line pos)]
        [_ #f])
      10)
    (check-equal?
      (match p-hash-invalid
        [(as-Pos _pos) #t]
        [_ #f])
      #f)
    (check-equal?
      (match p1
        [(as-Pos _pos) #t]
        [_ #f])
      #f))

  (test-case "matching: jsexpr styles for Pos"
    ;; optional schema guard for raw JSON
    (check-equal? (match p-hash [(? Pos-js? (Pos-js #:line l)) l] [_ #f])
                  10)
    ;; decode-then-match with as-Name
    (check-equal? (match p-hash [(as-Pos (Pos #:line l #:char c)) (list l c)] [_ #f])
                  '(10 5))
    ;; generated alias: ^Name => (as-Name (Name ...))
    (check-equal? (match p-hash [(^Pos #:line l #:char c) (list l c)] [_ #f])
                  '(10 5))
    (check-equal? (match p-hash [(^Pos l c) (list l c)] [_ #f])
                  '(10 5)))

  (test-case "encoding: simple and recursive"
    (check-equal? (->jsexpr p1)
                  (hasheq 'line 1 'character 10))
    (check-equal? (->jsexpr r)
                  (hasheq 'start (hasheq 'line 1 'character 10)
                          'end (hasheq 'line 2 'character 20))))

  (test-case "decoding: recursive jsexpr->Range"
    (define decoded (jsexpr->Range r-hash))
    (check-true (Range? decoded))
    (check-true (Pos? (Range-start decoded)))
    (check-equal? (Pos-line (Range-start decoded)) 10)
    (check-equal? (Pos-char (Range-end decoded)) 15))

  (test-case "validation: Range-js? validates nested json shape"
    (check-true (Range-js? r-hash))
    (check-false (Range-js? (hasheq 'start p1 'end p2)))
    (check-false (Range-js? (hasheq 'start p-hash))))

  (test-case "decoding: optional field can be absent"
    (define holder (jsexpr->MaybePosHolder (hasheq)))
    (check-true (Nothing? (MaybePosHolder-pos holder)))
    (define holder2 (jsexpr->MaybePosHolder (hasheq 'pos p-hash)))
    (check-true (Pos? (MaybePosHolder-pos holder2))))

  (test-case "decoding: listof decodes compound entries"
    (define holder (jsexpr->PosListHolder (hasheq 'items (list p-hash p-hash-2))))
    (check-true (Pos? (first (PosListHolder-items holder))))
    (check-equal? (Pos-line (second (PosListHolder-items holder))) 3))

  (test-case "decoding: hash/c decodes compound values"
    (define holder (jsexpr->PosHashHolder (hasheq 'items (hasheq 'a p-hash))))
    (define decoded-pos (hash-ref (PosHashHolder-items holder) 'a))
    (check-true (Pos? decoded-pos))
    (check-equal? (Pos-char decoded-pos) 5))

  (test-case "decoding: or/c chooses matching decoded variant"
    (define from-str (jsexpr->OrPosHolder (hasheq 'item "hello")))
    (check-equal? (OrPosHolder-item from-str) "hello")
    (define from-pos (jsexpr->OrPosHolder (hasheq 'item p-hash)))
    (check-true (Pos? (OrPosHolder-item from-pos)))
    (check-equal? (Pos-line (OrPosHolder-item from-pos)) 10)
    (check-exn exn:fail? (lambda () (jsexpr->OrPosHolder (hasheq 'item 42)))))

  (test-case "validation: or/c json predicate"
    (check-true (OrPosHolder-js? (hasheq 'item "s")))
    (check-true (OrPosHolder-js? (hasheq 'item p-hash)))
    (check-false (OrPosHolder-js? (hasheq 'item 42))))

  (test-case "decoding: optional with or/c"
    (define missing (jsexpr->MaybeOrPosHolder (hasheq)))
    (check-true (Nothing? (MaybeOrPosHolder-item missing)))
    (define from-str (jsexpr->MaybeOrPosHolder (hasheq 'item "ok")))
    (check-equal? (MaybeOrPosHolder-item from-str) "ok")
    (define from-pos (jsexpr->MaybeOrPosHolder (hasheq 'item p-hash-2)))
    (check-true (Pos? (MaybeOrPosHolder-item from-pos)))
    (check-equal? (Pos-char (MaybeOrPosHolder-item from-pos)) 30))

  (test-case "decoding: listof with or/c"
    (define holder
      (jsexpr->OrPosListHolder (hasheq 'items (list "a" p-hash "b" p-hash-2))))
    (define items (OrPosListHolder-items holder))
    (check-equal? (first items) "a")
    (check-true (Pos? (second items)))
    (check-equal? (third items) "b")
    (check-true (Pos? (fourth items)))
    (check-exn exn:fail?
               (lambda ()
                 (jsexpr->OrPosListHolder (hasheq 'items (list "a" 99))))))

  (test-case "matching: nested Range-js hash"
    (match r-hash
      [(Range-js #:start (Pos-js #:line l))
       (check-equal? l 10)]))

  (test-case "matching: explicit schema guard for invalid hash"
    (match p-hash-invalid
      [(? Pos-js? (Pos-js l c)) (fail "Should not match invalid types")]
      [_ (void)]))

  (test-case "matching: variable capture against hash returns hash"
    (match r-hash
      [(Range-js s e)
       (check-true (hash? s))
       (check-equal? (hash-ref s 'line) 10)]))

  (test-case "matching: native struct destructuring"
    (match (Range (Pos 10 5) (Pos 20 15))
      [(Range (Pos l1 c1) (Pos l2 c2))
       (check-equal? l1 10)
       (check-equal? c2 15)]))

  (test-case "matching: variable capture against struct returns struct"
    (match (Range (Pos 10 5) (Pos 20 15))
      [(Range s e)
       (check-true (Pos? s))
       (check-equal? (Pos-line s) 10)]))

  (test-case "matching: nested decode pattern with as-Range"
    (check-equal?
      (match r-hash
        [(as-Range (Range #:start (Pos #:line sl #:char sc)
                          #:end (Pos #:line el #:char ec)))
         (list sl sc el ec)]
        [_ #f])
      '(10 5 20 15)))
  )

;; 6. Export Test
(module export-test racket
  (require "../json-util.rkt")
  (define-json-struct Exported [x integer?])
  (provide (json-type-out Exported))
  )

(module+ test
  (require (submod ".." export-test) rackunit)

  (test-case "Verify json-type-out exports"
    ;; Struct constructor and predicate
    (define e (Exported 10))
    (check-true (Exported? e))
    (check-equal? (Exported-x e) 10)

    ;; Keyword wrapper (Exported supports keywords)
    (define e-kw (Exported #:x 20))
    (check-equal? (Exported-x e-kw) 20)

    ;; JSON matcher
    (check-match (hasheq 'x 30) (Exported-js 30))

    ;; Contract: JSON-only
    (check-false (Exported-js? e))
    (check-true (Exported-js? (hasheq 'x 40)))

    ;; as-Name should be exported by json-type-out
    (check-equal?
      (match (hasheq 'x 41)
        [(as-Exported ex) (Exported-x ex)]
        [_ #f])
      41)
    ;; ^Name should be exported by json-type-out
    (check-equal?
      (match (hasheq 'x 42)
        [(^Exported x) x]
        [_ #f])
      42)))

