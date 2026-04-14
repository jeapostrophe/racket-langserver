#lang racket/base

(module+ test
  (require rackunit
           racket/contract
           "../../doclib/lazy-cache.rkt")

  (test-case
    "lazy cache memoizes until invalidated"
    (define cache (make-lazy-cache))
    (define build-count 0)

    (check-equal?
      (call-with-lazy-cache! cache
                             (lambda ()
                               (set! build-count (add1 build-count))
                               'first))
      'first)
    (check-equal? build-count 1)

    (check-equal?
      (call-with-lazy-cache! cache
                             (lambda ()
                               (set! build-count (add1 build-count))
                               'second))
      'first)
    (check-equal? build-count 1)

    (lazy-cache-invalidate! cache)

    (check-equal?
      (call-with-lazy-cache! cache
                             (lambda ()
                               (set! build-count (add1 build-count))
                               'second))
      'second)
    (check-equal? build-count 2))

  (test-case
    "lazy cache does not retain failed fills"
    (define cache (make-lazy-cache))
    (define build-count 0)

    (check-exn
      exn:fail?
      (lambda ()
        (call-with-lazy-cache! cache
                               (lambda ()
                                 (set! build-count (add1 build-count))
                                 (error 'lazy-cache-test "boom")))))
    (check-equal? build-count 1)

    (check-equal?
      (call-with-lazy-cache! cache
                             (lambda ()
                               (set! build-count (add1 build-count))
                               'ok))
      'ok)
    (check-equal? build-count 2))

  (test-case
    "lazy cache shares one fill across concurrent readers"
    (define cache (make-lazy-cache))
    (define build-count 0)
    (define fill-started (make-semaphore 0))
    (define release-fill (make-semaphore 0))
    (define results (make-channel))

    (define (spawn-reader thunk)
      (thread
        (lambda ()
          (channel-put results (thunk)))))

    (spawn-reader
      (lambda ()
        (call-with-lazy-cache! cache
                               (lambda ()
                                 (set! build-count (add1 build-count))
                                 (semaphore-post fill-started)
                                 (semaphore-wait release-fill)
                                 'cached))))
    (semaphore-wait fill-started)
    (spawn-reader
      (lambda ()
        (call-with-lazy-cache! cache (lambda () 'other))))

    (semaphore-post release-fill)

    (check-equal? (list (channel-get results) (channel-get results))
                  '(cached cached))
    (check-equal? build-count 1))

  (test-case
    "helper threads wait for the in-progress fill"
    (define cache (make-lazy-cache))
    (define helper-started (make-channel))
    (define helper-result (make-channel))

    (define outer-value
      (call-with-lazy-cache! cache
                             (lambda ()
                               (thread
                                 (lambda ()
                                   (channel-put helper-started 'started)
                                   (channel-put helper-result
                                                (call-with-lazy-cache! cache
                                                                       (lambda () 'nested)))))
                               (channel-get helper-started)
                               'outer)))

    (check-equal? outer-value 'outer)
    (check-equal? (channel-get helper-result) 'outer))

  (test-case
    "lazy cache contract rejects a current value that does not fit"
    (define cache (make-lazy-cache))

    (call-with-lazy-cache! cache
                           (lambda () 'bad))

    (define protected-cache
      (contract (lazy-cache-of number?) cache 'positive 'negative))

    (check-exn
      exn:fail:contract?
      (lambda ()
        (call-with-lazy-cache! protected-cache
                               (lambda () 7)))))

  (test-case
    "lazy cache rejects fills through a wrapped cache"
    (define cache
      (contract (lazy-cache-of number?)
                (make-lazy-cache)
                'positive
                'negative))

    (check-exn
      exn:fail:contract?
      (lambda ()
        (call-with-lazy-cache! cache
                               (lambda () 'bad))))

    (check-equal?
      (call-with-lazy-cache! cache
                             (lambda () 7))
      7))

  (test-case
    "lazy cache accepts higher-order contracts"
    (define cache
      (contract (lazy-cache-of (-> integer? integer?))
                (make-lazy-cache)
                'positive
                'negative))

    (define cached-procedure
      (call-with-lazy-cache! cache
                             (lambda ()
                               (lambda (n)
                                 (add1 n)))))

    (check-equal? (cached-procedure 4) 5)
    (check-exn
      exn:fail:contract?
      (lambda ()
        (cached-procedure 'bad)))))
