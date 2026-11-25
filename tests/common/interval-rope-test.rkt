#lang racket/base

(require rackunit
         (prefix-in native: data/interval-map)
         racket/dict
         racket/match
         (prefix-in rope: "../../common/interval-rope.rkt")
         (prefix-in rope: (submod "../../common/interval-rope.rkt" test-support)))

(module+ test
  ;; Helper to verify tree invariants and equivalence against a native reference map
  (define (check-rope-equivalence actual expected [probe-keys '(-50 -10 -1 0 1 5 12 25 50 100 200)])
    (rope:check-interval-rope! actual)
    (check-equal? (rope:interval-rope->list actual) (dict->list expected))
    (check-equal?
      (for/list ([(start end val) (rope:in-interval-rope actual)])
        (cons (cons start end) val))
      (dict->list expected))
    (check-equal? (rope:interval-rope-count actual) (length (dict->list expected)))
    (for ([key (in-list probe-keys)])
      (check-equal?
        (call-with-values
          (lambda () (rope:interval-rope-ref/bounds actual key #f))
          list)
        (call-with-values
          (lambda () (native:interval-map-ref/bounds expected key #f))
          list))))

  ;; ---------------------------------------------------------------------------
  ;; 1. Construction and Predicates
  ;; ---------------------------------------------------------------------------
  (test-case "constructor and predicate behavior"
    (define empty-rope (rope:interval-rope))
    (check-true (rope:interval-rope? empty-rope))
    (check-false (rope:interval-rope? (native:make-interval-map)))
    (check-false (rope:interval-rope? (hasheq)))
    (check-true (rope:interval-rope-empty? empty-rope))
    (check-equal? (rope:interval-rope-count empty-rope) 0)
    (check-equal? (rope:interval-rope-span empty-rope) 0)
    (check-equal? (rope:interval-rope-origin empty-rope) 0)
    (check-equal? (rope:interval-rope->list empty-rope) '())
    (rope:check-interval-rope! empty-rope)

    ;; Construct with initial entries
    (define init-rope
      (rope:interval-rope (list (cons (cons 5 10) 'val-a)
                                (cons (cons 20 30) 'val-b))))
    (check-equal? (rope:interval-rope->list init-rope)
                  '(((5 . 10) . val-a) ((20 . 30) . val-b)))
    (check-equal? (rope:interval-rope-count init-rope) 2)
    (check-equal? (rope:interval-rope-origin init-rope) 5)
    (check-equal? (rope:interval-rope-span init-rope) 25)
    (rope:check-interval-rope! init-rope)

    ;; Invariant checker rejects malformed nodes and unnormalized zero-count roots
    (define malformed-with-empty-child
      (rope:IntervalRope 0 (rope:IntervalNode rope:empty-tree
                                              (rope:IntervalLeaf 10 'v)
                                              10 2 1)))
    (check-exn #rx"internal node has empty child"
               (lambda () (rope:check-interval-rope! malformed-with-empty-child)))
    (match-define (rope:IntervalLeaf _ gap-val) rope:empty-tree)
    (define unnormalized-gap
      (rope:IntervalRope 0 (rope:IntervalLeaf 10 gap-val)))
    (check-exn #rx"zero-count rope has non-empty root"
               (lambda () (rope:check-interval-rope! unnormalized-gap))))

  ;; ---------------------------------------------------------------------------
  ;; 2. Sequence Iteration & Argument Validation
  ;; ---------------------------------------------------------------------------
  (test-case "in-interval-rope sequence and argument validation"
    (define multi
      (rope:interval-rope '(((-50 . -20) . shared)
                            ((-10 . 10) . specific)
                            ((25 . 30) . shared))))

    ;; in-interval-rope sequence yields 3 values: start end value
    (check-equal?
      (for/list ([(start end val) (rope:in-interval-rope multi)])
        (list start end val))
      '((-50 -20 shared)
        (-10 10 specific)
        (25 30 shared)))

    ;; in-interval-rope on empty rope
    (check-equal?
      (for/list ([(start end val) (rope:in-interval-rope rope:empty-interval-rope)])
        (list start end val))
      '())

    ;; Early termination only evaluates needed elements
    (check-equal?
      (for/first ([(start end val) (rope:in-interval-rope multi)]
                  #:when (> start -20))
        (list start end val))
      '(-10 10 specific))

    ;; Public operations reject non-IntervalRope arguments with contract errors
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope-empty? 'not-a-rope)))
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope-origin 'not-a-rope)))
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope-span 'not-a-rope)))
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope-count 'not-a-rope)))
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope-ref 'not-a-rope 0)))
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope-ref/bounds 'not-a-rope 0)))
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope-overlap-values 'not-a-rope 0 1)))
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope-set 'not-a-rope 0 1 'v)))
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope-remove 'not-a-rope 0 1)))
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope-expand 'not-a-rope 0 1)))
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope-contract 'not-a-rope 0 1)))
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope-update 'not-a-rope 0 1 add1 0)))
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope->list 'not-a-rope)))
    (check-exn exn:fail:contract? (lambda () (rope:interval-rope-for-each 'not-a-rope void)))
    (check-exn exn:fail:contract? (lambda () (rope:in-interval-rope 'not-a-rope))))

  ;; ---------------------------------------------------------------------------
  ;; 3. Point Queries (ref and ref/bounds)
  ;; ---------------------------------------------------------------------------
  (test-case "point lookup: interval-rope-ref and interval-rope-ref/bounds"
    (define r (rope:interval-rope '(((10 . 20) . first) ((30 . 40) . second))))

    ;; Hit cases
    (check-equal? (rope:interval-rope-ref r 10) 'first)
    (check-equal? (rope:interval-rope-ref r 15) 'first)
    (check-equal? (rope:interval-rope-ref r 19) 'first)
    (check-equal? (rope:interval-rope-ref r 30) 'second)
    (check-equal? (rope:interval-rope-ref r 39) 'second)

    (define-values (s1 e1 v1) (rope:interval-rope-ref/bounds r 15))
    (check-equal? (list s1 e1 v1) '(10 20 first))

    ;; Miss cases: before origin, in gap, at boundary, after end
    (check-equal? (rope:interval-rope-ref r 5 'default) 'default)
    (check-equal? (rope:interval-rope-ref r 20 'default) 'default)
    (check-equal? (rope:interval-rope-ref r 25 'default) 'default)
    (check-equal? (rope:interval-rope-ref r 40 'default) 'default)
    (check-equal? (rope:interval-rope-ref r 50 (lambda () 'thunk-val)) 'thunk-val)

    (define-values (ms me mv) (rope:interval-rope-ref/bounds r 25 'miss))
    (check-equal? (list ms me mv) '(#f #f miss))

    ;; Unmapped key with absent default raises error blaming the called function
    (check-exn #rx"interval-rope-ref: no mapping found"
               (lambda () (rope:interval-rope-ref r 25)))
    (check-exn #rx"interval-rope-ref/bounds: no mapping found"
               (lambda () (rope:interval-rope-ref/bounds r 25)))

    ;; Argument contract violations (non-integers and inexact numbers)
    (check-exn exn:fail:contract?
               (lambda () (rope:interval-rope-ref r "invalid-key")))
    (check-exn exn:fail:contract?
               (lambda () (rope:interval-rope-ref r 3.14)))
    (check-exn exn:fail:contract?
               (lambda () (rope:interval-rope-ref/bounds r 1.0))))

  ;; ---------------------------------------------------------------------------
  ;; 4. Range Overlap Queries (interval-rope-overlap-values)
  ;; ---------------------------------------------------------------------------
  (test-case "range overlap: interval-rope-overlap-values"
    (define r (rope:interval-rope '(((10 . 20) . a) ((30 . 40) . b) ((50 . 60) . c))))

    ;; Range before origin: empty
    (check-equal? (rope:interval-rope-overlap-values r 0 5) '())
    ;; Range matching exact interval
    (check-equal? (rope:interval-rope-overlap-values r 10 20) '(a))
    ;; Range inside an interval
    (check-equal? (rope:interval-rope-overlap-values r 12 18) '(a))
    ;; Range inside a gap
    (check-equal? (rope:interval-rope-overlap-values r 22 28) '())
    ;; Range overlapping first and second
    (check-equal? (rope:interval-rope-overlap-values r 15 35) '(a b))
    ;; Range spanning all intervals
    (check-equal? (rope:interval-rope-overlap-values r 5 65) '(a b c))
    ;; Range past the end: empty
    (check-equal? (rope:interval-rope-overlap-values r 65 100) '())

    ;; Empty / point cursor range: selects strictly containing interval
    (check-equal? (rope:interval-rope-overlap-values r 15 15) '(a))
    (check-equal? (rope:interval-rope-overlap-values r 20 20) '())
    (check-equal? (rope:interval-rope-overlap-values r 25 25) '())

    ;; Validation error on inverted range
    (check-exn #rx"query interval start is greater than end"
               (lambda () (rope:interval-rope-overlap-values r 30 20))))

  ;; ---------------------------------------------------------------------------
  ;; 5. Pure Set Transformations and Immutability
  ;; ---------------------------------------------------------------------------
  (test-case "interval-rope-set pure transformations and immutability"
    (define r0 (rope:interval-rope))
    (define ref (native:make-interval-map))

    ;; Insert into empty
    (define r1 (rope:interval-rope-set r0 20 30 'base))
    (native:interval-map-set! ref 20 30 'base)
    (check-rope-equivalence r1 ref)
    ;; Immutability: r0 remains empty!
    (check-true (rope:interval-rope-empty? r0))

    ;; Prepend before origin
    (define r2 (rope:interval-rope-set r1 5 15 'prefix))
    (native:interval-map-set! ref 5 15 'prefix)
    (check-rope-equivalence r2 ref)
    ;; Immutability: r1 has only 1 interval
    (check-equal? (rope:interval-rope-count r1) 1)

    ;; Append after current span
    (define r3 (rope:interval-rope-set r2 40 50 'suffix))
    (native:interval-map-set! ref 40 50 'suffix)
    (check-rope-equivalence r3 ref)

    ;; Insert within a gap
    (define r4 (rope:interval-rope-set r3 32 36 'gap-filler))
    (native:interval-map-set! ref 32 36 'gap-filler)
    (check-rope-equivalence r4 ref)

    ;; Overwrite exact interval
    (define r5 (rope:interval-rope-set r4 32 36 'gap-replaced))
    (native:interval-map-set! ref 32 36 'gap-replaced)
    (check-rope-equivalence r5 ref)

    ;; Overwrite middle of an interval (split)
    (define r6 (rope:interval-rope-set r5 23 27 'middle))
    (native:interval-map-set! ref 23 27 'middle)
    (check-rope-equivalence r6 ref)

    ;; Partial overlap on the left boundary
    (define r7 (rope:interval-rope-set r6 3 8 'left-overlap))
    (native:interval-map-set! ref 3 8 'left-overlap)
    (check-rope-equivalence r7 ref)

    ;; Partial overlap on the right boundary
    (define r8 (rope:interval-rope-set r7 48 55 'right-overlap))
    (native:interval-map-set! ref 48 55 'right-overlap)
    (check-rope-equivalence r8 ref)

    ;; Broad overwrite spanning multiple intervals
    (define r9 (rope:interval-rope-set r8 10 45 'broad-coverage))
    (native:interval-map-set! ref 10 45 'broad-coverage)
    (check-rope-equivalence r9 ref)

    ;; Bignum coordinates support (coordinates >= 2^70)
    (define bignum-start (expt 2 70))
    (define bignum-end (+ bignum-start 1000))
    (define r-bignum (rope:interval-rope-set r9 bignum-start bignum-end 'bignum-val))
    (native:interval-map-set! ref bignum-start bignum-end 'bignum-val)
    (check-equal? (rope:interval-rope-ref r-bignum (+ bignum-start 500)) 'bignum-val)
    (check-rope-equivalence r-bignum ref (list (+ bignum-start 500)))

    ;; Inverted bounds and non-exact numbers validation
    (check-exn #rx"strictly less than"
               (lambda () (rope:interval-rope-set r9 10 10 'x)))
    (check-exn #rx"strictly less than"
               (lambda () (rope:interval-rope-set r9 20 10 'x)))
    (check-exn exn:fail:contract?
               (lambda () (rope:interval-rope-set r9 1.0 10 'x)))
    (check-exn exn:fail:contract?
               (lambda () (rope:interval-rope-set r9 0 10.5 'x))))

  ;; ---------------------------------------------------------------------------
  ;; 6. Pure Remove Transformations
  ;; ---------------------------------------------------------------------------
  (test-case "interval-rope-remove deletion scenarios"
    (define r0 (rope:interval-rope))

    ;; Remove on empty is a no-op
    (check-eq? (rope:interval-rope-remove r0 0 10) r0)

    ;; Populate rope
    (define r-pop
      (for/fold ([r r0])
                ([i (in-range 5)])
        (rope:interval-rope-set r (* i 20) (+ (* i 20) 10) i)))
    (define ref (native:make-interval-map))
    (for ([i (in-range 5)])
      (native:interval-map-set! ref (* i 20) (+ (* i 20) 10) i))
    (check-rope-equivalence r-pop ref)

    ;; Remove in non-overlapping regions (before origin, in gap, after end)
    (define r1 (rope:interval-rope-remove r-pop -20 -10))
    (check-rope-equivalence r1 ref)
    (define r2 (rope:interval-rope-remove r1 12 18))
    (check-rope-equivalence r2 ref)
    (define r3 (rope:interval-rope-remove r2 100 120))
    (check-rope-equivalence r3 ref)

    ;; Partial overlap removals
    (define r4 (rope:interval-rope-remove r3 5 15))
    (native:interval-map-remove! ref 5 15)
    (check-rope-equivalence r4 ref)

    ;; Remove middle of an interval
    (define r5 (rope:interval-rope-remove r4 23 27))
    (native:interval-map-remove! ref 23 27)
    (check-rope-equivalence r5 ref)

    ;; Remove multiple intervals
    (define r6 (rope:interval-rope-remove r5 35 65))
    (native:interval-map-remove! ref 35 65)
    (check-rope-equivalence r6 ref)

    ;; Remove everything left
    (define r-empty (rope:interval-rope-remove r6 -100 200))
    (check-true (rope:interval-rope-empty? r-empty))
    (check-equal? (rope:interval-rope-count r-empty) 0)
    (check-equal? (rope:interval-rope-origin r-empty) 0)
    (rope:check-interval-rope! r-empty)

    ;; Immutability: r-pop still has 5 intervals!
    (check-equal? (rope:interval-rope-count r-pop) 5))

  ;; ---------------------------------------------------------------------------
  ;; 7. Pure Shifting (expand and contract)
  ;; ---------------------------------------------------------------------------
  (test-case "interval-rope-expand and interval-rope-contract shifting"
    (define r0 (rope:interval-rope))

    ;; Empty expand/contract are no-ops
    (check-eq? (rope:interval-rope-expand r0 10 20) r0)
    (check-eq? (rope:interval-rope-contract r0 10 20) r0)

    ;; Setup initial intervals: [10, 20) and [40, 50)
    (define r1 (rope:interval-rope-set
                 (rope:interval-rope-set r0 10 20 'first)
                 40 50 'second))
    (define ref (native:make-interval-map))
    (native:interval-map-set! ref 10 20 'first)
    (native:interval-map-set! ref 40 50 'second)

    ;; Expand before origin (origin shifts right)
    (define r2 (rope:interval-rope-expand r1 0 5))
    (native:interval-map-expand! ref 0 5)
    (check-rope-equivalence r2 ref)

    ;; Expand inside a gap
    (define r3 (rope:interval-rope-expand r2 25 30))
    (native:interval-map-expand! ref 25 30)
    (check-rope-equivalence r3 ref)

    ;; Expand inside an interval (splits the interval)
    (define r4 (rope:interval-rope-expand r3 18 22))
    (native:interval-map-expand! ref 18 22)
    (check-rope-equivalence r4 ref)

    ;; Contract before origin
    (define r5 (rope:interval-rope-contract r4 0 3))
    (native:interval-map-contract! ref 0 3)
    (check-rope-equivalence r5 ref)

    ;; Contract inside a gap
    (define r6 (rope:interval-rope-contract r5 25 28))
    (native:interval-map-contract! ref 25 28)
    (check-rope-equivalence r6 ref)

    ;; Contract across interval boundaries
    (define r7 (rope:interval-rope-contract r6 15 25))
    (native:interval-map-contract! ref 15 25)
    (check-rope-equivalence r7 ref)

    ;; Contract errors on invalid coordinates
    (check-exn exn:fail:contract?
               (lambda () (rope:interval-rope-expand r7 2.5 5)))
    (check-exn exn:fail:contract?
               (lambda () (rope:interval-rope-contract r7 0 2.5))))

  (test-case "contract coalescing semantics"
    (define shared-val (list "shared"))
    (define distinct-val (list "shared"))

    ;; Setup intervals: [0, 5) and [10, 15) with identical pointer (eq?)
    (define r-eq (rope:interval-rope-set
                   (rope:interval-rope-set (rope:interval-rope) 0 5 shared-val)
                   10 15 shared-val))
    (define ref-eq (native:make-interval-map))
    (native:interval-map-set! ref-eq 0 5 shared-val)
    (native:interval-map-set! ref-eq 10 15 shared-val)

    ;; Delete the gap [5, 10) so they touch
    (define r-eq-contracted (rope:interval-rope-contract r-eq 5 10))
    (native:interval-map-contract! ref-eq 5 10)
    ;; In data/interval-map, touching eq? values coalesce into a single interval [0, 10)
    (check-rope-equivalence r-eq-contracted ref-eq)
    (check-equal? (rope:interval-rope->list r-eq-contracted) `(((0 . 10) . ,shared-val)))
    (check-equal? (rope:interval-rope-count r-eq-contracted) 1)

    ;; Setup intervals: [0, 5) and [10, 15) with equal? but not eq? values
    (define r-neq (rope:interval-rope-set
                    (rope:interval-rope-set (rope:interval-rope) 0 5 shared-val)
                    10 15 distinct-val))
    (define ref-neq (native:make-interval-map))
    (native:interval-map-set! ref-neq 0 5 shared-val)
    (native:interval-map-set! ref-neq 10 15 distinct-val)

    ;; Delete the gap [5, 10) so they touch
    (define r-neq-contracted (rope:interval-rope-contract r-neq 5 10))
    (native:interval-map-contract! ref-neq 5 10)
    ;; Distinct values must remain separate intervals [0, 5) and [5, 10)
    (check-rope-equivalence r-neq-contracted ref-neq)
    (check-equal? (rope:interval-rope->list r-neq-contracted)
                  `(((0 . 5) . ,shared-val)
                    ((5 . 10) . ,distinct-val)))
    (check-equal? (rope:interval-rope-count r-neq-contracted) 2))

  ;; ---------------------------------------------------------------------------
  ;; 8. Pure In-Place Updates (update)
  ;; ---------------------------------------------------------------------------
  (test-case "interval-rope-update"
    (define r0 (rope:interval-rope-set (rope:interval-rope) 10 20 '(a)))
    (define ref (native:make-interval-map))
    (native:interval-map-set! ref 10 20 '(a))

    ;; update over existing interval
    (define r1 (rope:interval-rope-update r0 12 18 (lambda (old) (cons 'b old))))
    (native:interval-map-cons*! ref 12 18 'b)
    (check-rope-equivalence r1 ref)

    ;; update across unmapped gaps using default
    (define r2 (rope:interval-rope-update r1 5 25 (lambda (old) (cons 'c old)) '()))
    (native:interval-map-cons*! ref 5 25 'c '())
    (check-rope-equivalence r2 ref)

    ;; update with numeric transformation
    (define num-r (rope:interval-rope-set (rope:interval-rope) 10 20 100))
    (define num-ref (native:make-interval-map))
    (native:interval-map-set! num-ref 10 20 100)
    (define num-r-updated (rope:interval-rope-update num-r 10 20 add1))
    (native:interval-map-update*! num-ref 10 20 add1)
    (check-rope-equivalence num-r-updated num-ref)

    ;; update across a gap without default raises error
    (check-exn #rx"no mapping found"
               (lambda () (rope:interval-rope-update num-r 5 15 add1))))

  ;; ---------------------------------------------------------------------------
  ;; 9. Structural AVL Invariants & Compaction
  ;; ---------------------------------------------------------------------------
  (test-case "structural AVL invariants and gap compaction"
    (define r0
      (for/fold ([r (rope:interval-rope)])
                ([i (in-range 60)])
        (define updated (rope:interval-rope-set r (* i 15) (+ (* i 15) 8) i))
        (rope:check-interval-rope! updated)
        updated))

    ;; Perform repeated splits and contract operations
    (define r1
      (for/fold ([r r0])
                ([i (in-range 10)])
        (define updated (rope:interval-rope-contract r (+ (* i 30) 4) (+ (* i 30) 12)))
        (rope:check-interval-rope! updated)
        updated))

    ;; Expand within gaps repeatedly; gap merging must keep tree bounded
    (void
      (for/fold ([r r1])
                ([i (in-range 10)])
        (define updated (rope:interval-rope-expand r 10 15))
        (rope:check-interval-rope! updated)
        updated)))

  ;; ---------------------------------------------------------------------------
  ;; 10. Deterministic Differential Verification against native data/interval-map
  ;; ---------------------------------------------------------------------------
  (test-case "deterministic differential verification against data/interval-map"
    (random-seed 424242)
    (for ([trial (in-range 10)])
      (define ref (native:make-interval-map))
      (define current-rope (rope:interval-rope))

      ;; Populate initial intervals
      (set! current-rope
            (for/fold ([r current-rope])
                      ([i (in-range (+ 5 (random 15)))])
              (define start (- (* i 10) 20))
              (define end (+ start 1 (random 8)))
              (native:interval-map-set! ref start end i)
              (rope:interval-rope-set r start end i)))
      (check-rope-equivalence current-rope ref)

      ;; Perform mixed sequence of 25 operations
      (for ([step (in-range 25)])
        (define start (- (random 150) 30))
        (define end (+ start 1 (random 20)))
        (case (modulo step 5)
          [(0)
           (define val (random 1000))
           (native:interval-map-set! ref start end val)
           (set! current-rope (rope:interval-rope-set current-rope start end val))]
          [(1)
           (native:interval-map-expand! ref start end)
           (set! current-rope (rope:interval-rope-expand current-rope start end))]
          [(2)
           (native:interval-map-contract! ref start end)
           (set! current-rope (rope:interval-rope-contract current-rope start end))]
          [(3)
           (native:interval-map-remove! ref start end)
           (set! current-rope (rope:interval-rope-remove current-rope start end))]
          [(4)
           (native:interval-map-update*! ref start end (lambda (old) (+ old 10)) 0)
           (set! current-rope (rope:interval-rope-update current-rope start end (lambda (old) (+ old 10)) 0))])
        (rope:check-interval-rope! current-rope))

      ;; End of trial verification
      (check-rope-equivalence current-rope ref)))

  (test-case "interval-rope iteration: for-each"
    (define r (rope:interval-rope '(((10 . 20) . a) ((30 . 40) . b) ((50 . 60) . c))))
    (define collected '())
    (define ret
      (rope:interval-rope-for-each r
                                   (lambda (s e v)
                                     (set! collected (cons (list s e v) collected)))))
    (check-equal? (reverse collected)
                  '((10 20 a) (30 40 b) (50 60 c)))
    (check-equal? ret (void))

    ;; Callbacks returning zero values or multiple values must not fail
    (check-not-exn
      (lambda ()
        (rope:interval-rope-for-each r (lambda (_s _e _v) (values)))))
    (check-not-exn
      (lambda ()
        (rope:interval-rope-for-each r (lambda (_s _e _v) (values 1 2 3))))))

  (test-case "interval-rope interior expand policies"
    (define r (rope:interval-rope '(((10 . 20) . sym))))
    ;; Interior split: splits into two pieces with gap
    (define r-split (rope:interval-rope-expand r 14 17 #:interior 'split))
    (rope:check-interval-rope! r-split)
    (check-equal? (rope:interval-rope->list r-split)
                  '(((10 . 14) . sym) ((17 . 23) . sym)))

    ;; Interior extend: extends existing interval
    (define r-ext (rope:interval-rope-expand r 14 17 #:interior 'extend))
    (rope:check-interval-rope! r-ext)
    (check-equal? (rope:interval-rope->list r-ext)
                  '(((10 . 23) . sym)))

    ;; Extend expansion supports false interval values
    (define r-false
      (rope:interval-rope-expand (rope:interval-rope-set (rope:interval-rope) 10 20 #f)
                                 15 18
                                 #:interior 'extend))
    (rope:check-interval-rope! r-false)
    (check-equal? (rope:interval-rope->list r-false)
                  '(((10 . 23) . #f)))

    ;; Boundary insertion: preserves split policy
    (define r-start (rope:interval-rope-expand r 10 13 #:interior 'extend))
    (rope:check-interval-rope! r-start)
    (check-equal? (rope:interval-rope->list r-start)
                  '(((13 . 23) . sym)))
    (define r-end (rope:interval-rope-expand r 20 23 #:interior 'extend))
    (rope:check-interval-rope! r-end)
    (check-equal? (rope:interval-rope->list r-end)
                  '(((10 . 20) . sym)))

    ;; Exact boundary between two touching mapped intervals inserts gap without extending either
    (define touching (rope:interval-rope '(((10 . 20) . left) ((20 . 30) . right))))
    (define touching-exp (rope:interval-rope-expand touching 20 23 #:interior 'extend))
    (rope:check-interval-rope! touching-exp)
    (check-equal? (rope:interval-rope->list touching-exp)
                  '(((10 . 20) . left) ((23 . 33) . right)))

    ;; Insertion inside a gap or at a gap/interval boundary widens the gap without coalescing errors
    (define with-gap (rope:interval-rope '(((10 . 20) . left) ((30 . 40) . right))))
    (define gap-mid (rope:interval-rope-expand with-gap 25 28 #:interior 'extend))
    (rope:check-interval-rope! gap-mid)
    (check-equal? (rope:interval-rope->list gap-mid)
                  '(((10 . 20) . left) ((33 . 43) . right)))
    (define gap-start (rope:interval-rope-expand with-gap 20 23 #:interior 'extend))
    (rope:check-interval-rope! gap-start)
    (check-equal? (rope:interval-rope->list gap-start)
                  '(((10 . 20) . left) ((33 . 43) . right)))
    (define gap-end (rope:interval-rope-expand with-gap 30 33 #:interior 'extend))
    (rope:check-interval-rope! gap-end)
    (check-equal? (rope:interval-rope->list gap-end)
                  '(((10 . 20) . left) ((33 . 43) . right)))

    ;; Split expansion inside a gap widens the gap cleanly
    (define gap-split (rope:interval-rope-expand with-gap 25 28 #:interior 'split))
    (rope:check-interval-rope! gap-split)
    (check-equal? (rope:interval-rope->list gap-split)
                  '(((10 . 20) . left) ((33 . 43) . right))))

  (test-case "boundary gap trimming and canonical equality"
    (define base
      (rope:interval-rope '(((0 . 10) . left) ((20 . 30) . mid) ((40 . 50) . right))))
    (define trimmed-left (rope:interval-rope-remove base 0 10))
    (rope:check-interval-rope! trimmed-left)
    (check-equal? (rope:interval-rope-origin trimmed-left) 20)
    (check-equal? (rope:interval-rope-span trimmed-left) 30)

    (define trimmed-both (rope:interval-rope-remove trimmed-left 40 50))
    (rope:check-interval-rope! trimmed-both)
    (check-equal? (rope:interval-rope-origin trimmed-both) 20)
    (check-equal? (rope:interval-rope-span trimmed-both) 10)

    (define direct (rope:interval-rope '(((20 . 30) . mid))))
    (check-equal? trimmed-both direct)

    ;; Contraction exposing boundary gap also trims origin/span
    (define contracted-left (rope:interval-rope-contract base 0 10))
    (rope:check-interval-rope! contracted-left)
    (check-equal? (rope:interval-rope-origin contracted-left) 10)
    (check-equal? (rope:interval-rope-span contracted-left) 30))

  (test-case "procedure validation on empty rope and constructor edge cases"
    (check-exn exn:fail:contract?
               (lambda () (rope:interval-rope-for-each rope:empty-interval-rope 'not-proc)))
    (check-exn exn:fail:contract?
               (lambda () (rope:interval-rope-update rope:empty-interval-rope 0 5 'not-proc 0)))
    (check-exn exn:fail:contract?
               (lambda () (rope:interval-rope 'not-a-list)))
    (check-exn #rx"interval-rope:"
               (lambda () (rope:interval-rope '(bad))))
    (check-exn #rx"interval-rope:"
               (lambda () (rope:interval-rope '((10 . 20)))))
    (check-exn #rx"interval-rope:"
               (lambda () (rope:interval-rope '(((20 . 30) . b) ((0 . 10) . a) ((15 . 15) . bad)))))

    ;; Out-of-order and overlapping constructor entries use set semantics
    (define entries '(((20 . 30) . b) ((0 . 10) . a) ((5 . 25) . c) ((40 . 50) . d)))
    (define r-mixed (rope:interval-rope entries))
    (define ref (native:make-interval-map))
    (for ([entry (in-list entries)])
      (native:interval-map-set! ref (caar entry) (cdar entry) (cdr entry)))
    (check-rope-equivalence r-mixed ref))

  (test-case "interval-rope-from-sorted bulk constructor and validation"
    (define r-empty (rope:interval-rope-from-sorted '()))
    (check-eq? r-empty rope:empty-interval-rope)
    (rope:check-interval-rope! r-empty)

    ;; Sorted disjoint entries with both touching boundaries and gaps
    (define sorted-entries
      '(((0 . 5) . a) ((5 . 10) . b) ((20 . 30) . c) ((30 . 35) . d) ((50 . 60) . e)))
    (define r-sorted (rope:interval-rope-from-sorted sorted-entries))
    (define ref-sorted (native:make-interval-map))
    (for ([entry (in-list sorted-entries)])
      (native:interval-map-set! ref-sorted (caar entry) (cdar entry) (cdr entry)))
    (check-rope-equivalence r-sorted ref-sorted)

    ;; Rejects non-list, malformed entries, invalid bounds, out-of-order, and overlapping entries
    (check-exn #rx"interval-rope-from-sorted:"
               (lambda () (rope:interval-rope-from-sorted 'not-a-list)))
    (check-exn #rx"interval-rope-from-sorted:"
               (lambda () (rope:interval-rope-from-sorted '(bad))))
    (check-exn #rx"interval-rope-from-sorted:"
               (lambda () (rope:interval-rope-from-sorted '((10 . 20)))))
    (check-exn #rx"interval-rope-from-sorted:"
               (lambda () (rope:interval-rope-from-sorted '(((0 . 5) . a) ((10 . 10) . bad)))))
    (check-exn #rx"interval-rope-from-sorted:"
               (lambda () (rope:interval-rope-from-sorted '(((10 . 20) . a) ((0 . 5) . b)))))
    (check-exn #rx"interval-rope-from-sorted:"
               (lambda () (rope:interval-rope-from-sorted '(((0 . 10) . a) ((5 . 15) . b)))))))
