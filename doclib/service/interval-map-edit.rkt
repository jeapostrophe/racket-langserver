#lang racket/base

(require data/interval-map)

(provide interval-map-expand/policy!)

;; #:interior 'split  — interior insert splits the interval (interval-map default)
;; #:interior 'extend — interior insert stays in the same interval
(define (interval-map-expand/policy! im start end #:interior interior)
  (unless (memq interior '(split extend))
    (raise-argument-error
      'interval-map-expand/policy!
      "(or/c 'split 'extend)"
      interior))
  (define missing (gensym 'missing))
  (define-values (range-start range-end value)
    (interval-map-ref/bounds im start missing))
  (interval-map-expand! im start end)
  (when (and (eq? interior 'extend)
             (not (eq? value missing))
             (< range-start start)
             (< start range-end))
    (interval-map-set! im range-start (+ range-end (- end start)) value)))
