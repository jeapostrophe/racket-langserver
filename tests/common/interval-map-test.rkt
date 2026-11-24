#lang racket/base

(require rackunit
         "../../common/interval-map.rkt")

(module+ test
  (test-case "make-interval-map, set!, ref, and bounds"
    (define im (make-interval-map))
    (check-true (interval-map? im))
    (check-true (interval-map-empty? im))
    (check-equal? (interval-map-count im) 0)

    (interval-map-set! im 10 20 'hello)
    (check-false (interval-map-empty? im))
    (check-equal? (interval-map-count im) 1)

    (check-equal? (interval-map-ref im 15) 'hello)
    (check-equal? (interval-map-ref im 5 #f) #f)
    (check-equal? (interval-map-ref im 25 #f) #f)
    (check-exn exn:fail? (lambda () (interval-map-ref im 5)))

    (define-values (s e v) (interval-map-ref/bounds im 15 #f))
    (check-equal? s 10)
    (check-equal? e 20)
    (check-equal? v 'hello)

    (define-values (missing-s missing-e missing-v) (interval-map-ref/bounds im 5 'fallback))
    (check-equal? missing-v 'fallback)
    (check-exn exn:fail? (lambda () (interval-map-ref/bounds im 5))))

  (test-case "interval-map-remove!"
    (define im (make-interval-map))
    (interval-map-set! im 0 30 'val)
    (interval-map-remove! im 10 20)
    (check-equal? (interval-map->list im)
                  (list (cons (cons 0 10) 'val)
                        (cons (cons 20 30) 'val)))
    (interval-map-remove! im 0 30)
    (check-true (interval-map-empty? im))
    (check-equal? (interval-map-count im) 0))

  (test-case "interval-map->list and in-interval-map sequence"
    (define im (make-interval-map))
    (interval-map-set! im 10 15 'second)
    (interval-map-set! im 0 5 'first)
    (check-equal? (interval-map->list im)
                  (list (cons (cons 0 5) 'first)
                        (cons (cons 10 15) 'second)))
    (check-equal?
      (for/list ([(start end val) (in-interval-map im)])
        (list start end val))
      '((0 5 first) (10 15 second))))

  (test-case "interval-map sequence retains its captured contents"
    (define im (make-interval-map))
    (interval-map-set! im -5 0 'old)
    (define sequence (in-interval-map im))
    (interval-map-set! im 5 10 'new)
    (define (collect seq)
      (for/list ([(start end value) seq])
        (list start end value)))
    (check-equal? (collect sequence) '((-5 0 old)))
    (check-equal? (collect sequence) '((-5 0 old)))
    (check-equal? (collect (in-interval-map im)) '((-5 0 old) (5 10 new))))

  (test-case "overlap-values"
    (define im (make-interval-map))
    (interval-map-set! im 0 10 'a)
    (interval-map-set! im 15 25 'b)
    (interval-map-set! im 30 40 'c)

    (check-equal? (interval-map-overlap-values im 5 20) (list 'a 'b))
    (check-equal? (interval-map-overlap-values im 12 14) '())
    (check-equal? (interval-map-overlap-values im 18 35) (list 'b 'c))
    ;; Cursor/point query where start == end
    (check-equal? (interval-map-overlap-values im 20 20) (list 'b)))

  (test-case "expand! and contract!"
    (define im (make-interval-map))
    (interval-map-set! im 10 20 'val)
    ;; Default split expansion
    (interval-map-expand! im 15 18)
    (check-equal? (interval-map->list im)
                  (list (cons (cons 10 15) 'val)
                        (cons (cons 18 23) 'val)))
    ;; Contraction coalesces back
    (interval-map-contract! im 15 18)
    (check-equal? (interval-map->list im)
                  (list (cons (cons 10 20) 'val)))
    ;; Interior extend expansion
    (interval-map-expand! im 15 18 #:interior 'extend)
    (check-equal? (interval-map->list im)
                  (list (cons (cons 10 23) 'val))))

  (test-case "update*!"
    (define im (make-interval-map))
    (interval-map-update*! im 20 30 (lambda (old) (string-append old "!")) "init")
    (check-equal? (interval-map-ref im 25) "init!")
    (interval-map-update*! im 20 30 (lambda (old) (string-append old "?")) "fallback")
    (check-equal? (interval-map-ref im 25) "init!?"))

  (test-case "snapshot round-trip and custom codec"
    (define im (make-interval-map))
    (interval-map-set! im 0 10 '(1 2))
    (interval-map-set! im 20 30 '(3 4))

    (define snap (interval-map->snapshot im (lambda (v) (reverse v))))
    (define restored (snapshot->interval-map snap (lambda (v) (reverse v))))

    (check-true (interval-map? restored))
    (check-equal? (interval-map-ref restored 5) '(1 2))
    (check-equal? (interval-map-ref restored 25) '(3 4))
    (check-equal? (interval-map->list restored) (interval-map->list im)))

  (test-case "contract violations on invalid inputs"
    (check-exn exn:fail:contract? (lambda () (interval-map-count 'not-a-map)))
    (check-exn exn:fail:contract? (lambda () (interval-map-ref 'not-a-map 10)))
    (check-exn exn:fail:contract? (lambda () (interval-map-set! 'not-a-map 0 10 'val)))
    (check-exn exn:fail:contract? (lambda () (in-interval-map 'not-a-map)))
    (check-exn exn:fail:contract? (lambda () (interval-map->snapshot 'not-a-map)))
    (check-exn exn:fail:contract? (lambda () (interval-map->snapshot (make-interval-map) 'not-a-proc)))
    (check-exn exn:fail:contract? (lambda () (snapshot->interval-map 'not-a-list)))
    (check-exn exn:fail:contract? (lambda () (snapshot->interval-map (list "bad" #()))))
    (check-exn exn:fail:contract? (lambda () (snapshot->interval-map (list #() #()) 'not-a-proc)))))
