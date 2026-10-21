#lang racket/base

(module+ test
  (require rackunit
           "../../common/source.rkt"
           "../../lsp/analysis.rkt"
           "../../lsp/source-controller/closed-analysis-cache.rkt"
           "../../workspace/source-store.rkt")

  (define text "#lang racket/base\n(define value 1)\nvalue\n")

  (define (make-analysis name)
    (define source (path->source (build-path (current-directory) name)))
    (define snapshot (Source-Snapshot source (gensym 'revision) 'disk text))
    (define product (analyze-source snapshot))
    (check-true (Analysis-Product-succeeded? product))
    (define outcome (Analysis-Outcome 'succeeded (Analysis-Product-summary product)))
    (Available-Analysis snapshot product (wrap-evt always-evt (lambda (_) outcome))))

  (define a (make-analysis "retained-a.rkt"))
  (define b (make-analysis "retained-b.rkt"))
  (define c (make-analysis "retained-c.rkt"))
  (define a-snapshot (Available-Analysis-snapshot a))
  (define b-snapshot (Available-Analysis-snapshot b))
  (define c-snapshot (Available-Analysis-snapshot c))
  (define a-source (Source-Snapshot-source a-snapshot))
  (define b-source (Source-Snapshot-source b-snapshot))
  (define c-source (Source-Snapshot-source c-snapshot))

  (test-case "matching uses equal source IDs and text across revisions"
    (define cache (make-closed-analysis-cache 1))
    (store-closed-analysis! cache a)
    (define alias (uri->source (source-uri a-source)))
    (define snapshot (Source-Snapshot alias (gensym 'revision) 'buffer text))
    (check-equal? (source-id alias) (source-id a-source))
    (check-false (eq? (source-id alias) (source-id a-source)))
    (check-eq? (find-closed-analysis! cache snapshot) a))

  (test-case "matching lookup protects an older insertion from LRU eviction"
    (define cache (make-closed-analysis-cache 2))
    (store-closed-analysis! cache a)
    (store-closed-analysis! cache b)
    (check-eq? (find-closed-analysis! cache a-snapshot) a)
    (store-closed-analysis! cache c)
    (check-false (find-closed-analysis! cache b-snapshot))
    (check-eq? (take-closed-analysis! cache a-source) a)
    (check-eq? (take-closed-analysis! cache c-source) c))

  (test-case "misses and mismatched text do not refresh recency"
    (define cache (make-closed-analysis-cache 2))
    (store-closed-analysis! cache a)
    (store-closed-analysis! cache b)
    (check-false (find-closed-analysis! cache c-snapshot))
    (check-false (find-closed-analysis! cache
                                        (struct-copy Source-Snapshot a-snapshot [text "different"])))
    (store-closed-analysis! cache c)
    (check-false (find-closed-analysis! cache a-snapshot))
    (check-eq? (take-closed-analysis! cache b-source) b)
    (check-eq? (take-closed-analysis! cache c-source) c))

  (test-case "replacement occupies one slot and becomes most recently used"
    (define cache (make-closed-analysis-cache 2))
    (define replacement (struct-copy Available-Analysis a [completion-evt #f]))
    (store-closed-analysis! cache a)
    (store-closed-analysis! cache b)
    (store-closed-analysis! cache replacement)
    (check-eq? (find-closed-analysis! cache b-snapshot) b)
    ;; Replace again so the lookup above cannot supply the tested recency.
    (store-closed-analysis! cache replacement)
    (store-closed-analysis! cache c)
    (check-false (find-closed-analysis! cache b-snapshot))
    (check-eq? (take-closed-analysis! cache a-source) replacement)
    (check-eq? (take-closed-analysis! cache c-source) c))

  (test-case "take removes the entry without requiring matching text"
    (define cache (make-closed-analysis-cache 1))
    (store-closed-analysis! cache a)
    (check-false (find-closed-analysis! cache
                                        (struct-copy Source-Snapshot a-snapshot [text "different"])))
    (check-eq? (take-closed-analysis! cache a-source) a)
    (check-false (find-closed-analysis! cache a-snapshot))
    (check-false (take-closed-analysis! cache a-source)))

  (test-case "discard and clear release entries and allow reuse"
    (define cache (make-closed-analysis-cache 2))
    (store-closed-analysis! cache a)
    (store-closed-analysis! cache b)
    (discard-closed-analysis! cache a-source)
    (discard-closed-analysis! cache a-source)
    (check-false (find-closed-analysis! cache a-snapshot))
    (check-eq? (take-closed-analysis! cache b-source) b)
    (store-closed-analysis! cache a)
    (store-closed-analysis! cache b)
    (clear-closed-analysis! cache)
    (check-false (take-closed-analysis! cache a-source))
    (check-false (take-closed-analysis! cache b-source))
    (store-closed-analysis! cache c)
    (check-eq? (take-closed-analysis! cache c-source) c))

  (test-case "zero capacity retains nothing"
    (define cache (make-closed-analysis-cache 0))
    (for ([analysis (in-list (list a b c))])
      (store-closed-analysis! cache analysis)
      (check-false (find-closed-analysis! cache (Available-Analysis-snapshot analysis))))
    (check-false (take-closed-analysis! cache a-source)))

  (test-case "eviction preserves completion events held by existing waiters"
    (define cache (make-closed-analysis-cache 1))
    (store-closed-analysis! cache a)
    (define completion (Available-Analysis-completion-evt (find-closed-analysis! cache a-snapshot)))
    (store-closed-analysis! cache b)
    (check-false (find-closed-analysis! cache a-snapshot))
    (define outcome (sync/timeout 0 completion))
    (check-pred Analysis-Outcome? outcome)
    (check-eq? (Analysis-Outcome-state outcome) 'succeeded)))
