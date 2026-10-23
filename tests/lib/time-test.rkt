#lang racket/base

(require rackunit
         "../../common/time.rkt")

(module+ test
  (test-case "past deadlines are ready"
    (check-not-false (sync/timeout 0 (deadline-evt (- (current-deadline-milliseconds) 1)))))

  (test-case "future deadlines wait and then become ready"
    (define alarm (deadline-evt (+ (current-deadline-milliseconds) 100)))
    (check-false (sync/timeout 0 alarm))
    (check-eq? (sync/timeout 5 alarm) alarm)))
