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
    [start Pos?]
    [end Pos?])

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

  (test-case "encoding: simple and recursive"
    (check-equal? (jsexpr-encode p1)
                  (hasheq 'line 1 'character 10))
    (check-equal? (jsexpr-encode r)
                  (hasheq 'start (hasheq 'line 1 'character 10)
                          'end (hasheq 'line 2 'character 20))))

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
  )

;; 6. Export Test
(module export-test racket
  (require "../json-util.rkt")
  (define-json-struct Exported [x integer?])
  (provide (json-struct-out Exported))
  )

(module+ test
  (require (submod ".." export-test) rackunit)

  (test-case "Verify json-struct-out exports"
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
    (check-true (Exported-js? (hasheq 'x 40)))))

