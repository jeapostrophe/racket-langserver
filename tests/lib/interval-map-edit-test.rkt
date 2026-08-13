#lang racket/base

(require rackunit
         racket/dict
         data/interval-map
         "../../doclib/service/interval-map-edit.rkt")

(module+ test
  (test-case
    "split expansion follows interval-map boundary rules"
    (define value (gensym 'value))
    (define at-start (make-interval-map))
    (interval-map-set! at-start 10 20 value)
    (interval-map-expand/policy! at-start 10 13 #:interior 'split)
    (check-equal? (dict-keys at-start) (list (cons 13 23)))

    (define in-middle (make-interval-map))
    (interval-map-set! in-middle 10 20 value)
    (interval-map-expand/policy! in-middle 15 18 #:interior 'split)
    (check-equal? (dict-keys in-middle) (list (cons 10 15) (cons 18 23)))

    (define at-end (make-interval-map))
    (interval-map-set! at-end 10 20 value)
    (interval-map-expand/policy! at-end 20 23 #:interior 'split)
    (check-equal? (dict-keys at-end) (list (cons 10 20))))

  (test-case
    "extend expansion keeps an interior insertion in one interval"
    (define im (make-interval-map))
    (interval-map-set! im 10 20 (gensym 'value))
    (interval-map-expand/policy! im 15 18 #:interior 'extend)
    (check-equal? (dict-keys im) (list (cons 10 23))))

  (test-case
    "extend expansion supports false interval values"
    (define im (make-interval-map))
    (interval-map-set! im 10 20 #f)
    (interval-map-expand/policy! im 15 18 #:interior 'extend)
    (check-equal? (dict-keys im) (list (cons 10 23))))

  (test-case
    "contraction coalesces an interval split around deleted text"
    (define im (make-interval-map))
    (define value (gensym 'value))
    (interval-map-set! im 10 20 value)
    (interval-map-expand/policy! im 15 18 #:interior 'split)
    (interval-map-contract! im 15 18)
    (check-equal? (dict-keys im) (list (cons 10 20)))))
