#lang racket/base

(require rackunit
         "../../common/interval-rope.rkt")

(module+ test
  (test-case
    "split expansion follows interval-map boundary rules"
    (define value (gensym 'value))
    (define at-start
      (interval-rope-expand (interval-rope-set empty-interval-rope 10 20 value) 10 13))
    (check-equal? (map car (interval-rope->list at-start)) (list (cons 13 23)))

    (define in-middle
      (interval-rope-expand (interval-rope-set empty-interval-rope 10 20 value) 15 18))
    (check-equal? (map car (interval-rope->list in-middle))
                  (list (cons 10 15) (cons 18 23)))

    (define at-end
      (interval-rope-expand (interval-rope-set empty-interval-rope 10 20 value) 20 23))
    (check-equal? (map car (interval-rope->list at-end)) (list (cons 10 20))))

  (test-case
    "extend expansion keeps an interior insertion in one interval"
    (define rope
      (interval-rope-expand/extend (interval-rope-set empty-interval-rope 10 20 (gensym 'value)) 15 18))
    (check-equal? (map car (interval-rope->list rope)) (list (cons 10 23))))

  (test-case
    "extend expansion supports false interval values"
    (define rope
      (interval-rope-expand/extend (interval-rope-set empty-interval-rope 10 20 #f) 15 18))
    (check-equal? (map car (interval-rope->list rope)) (list (cons 10 23))))

  (test-case
    "contraction coalesces an interval split around deleted text"
    (define value (gensym 'value))
    (define rope
      (interval-rope-contract
        (interval-rope-expand (interval-rope-set empty-interval-rope 10 20 value) 15 18)
        15 18))
    (check-equal? (map car (interval-rope->list rope)) (list (cons 10 20)))))
