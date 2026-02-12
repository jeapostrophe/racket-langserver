#lang racket
(require "../json-util.rkt" rackunit)

(module+ test
  ;; Define a test struct
  (define-json-struct Pos
    [line integer?]
    [char integer? #:json character])

  ;; 1. Construction
  ;; Positional
  (define p1 (Pos 1 10))
  (check-equal? (Pos-line p1) 1)
  (check-equal? (Pos-char p1) 10) ; Accessors use Racket field names

  ;; Keyword (out-of-order)
  (define p2 (Pos #:char 20 #:line 2))
  (check-equal? (Pos-line p2) 2)
  (check-equal? (Pos-char p2) 20)

  ;; 2. Contract Enforcement
  (check-exn exn:fail:contract? (λ () (Pos "1" 10)))
  (check-exn exn:fail:contract? (λ () (Pos #:line 1 #:char "10")))

  ;; 3. Pattern Matching (Polymorphic)
  (define (get-line x)
    (match x
      [(Pos #:line l) l] ; Partial keyword match
      [_ #f]))

  (check-equal? (get-line p1) 1)
  (check-equal? (get-line (hasheq 'line 5 'character 50)) 5)

  (define (get-pos-list x)
    (match x
      [(Pos l c) (list l c)] ; Positional match (full arity)
      [_ #f]))

  (check-equal? (get-pos-list p2) '(2 20))
  (check-equal? (get-pos-list (hasheq 'line 3 'character 30)) '(3 30))

  ;; 4. JSON Encoding
  (check-equal? (jsexpr-encode p1)
                (hasheq 'line 1 'character 10))

  ;; 5. Recursive Encoding
  (define-json-struct Range
    [start Pos?]
    [end Pos?])

  (define r (Range p1 p2))
  (check-equal? (jsexpr-encode r)
                (hasheq 'start (hasheq 'line 1 'character 10)
                        'end   (hasheq 'line 2 'character 20)))

)
