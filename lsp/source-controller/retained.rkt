#lang racket/base

;; Bounded cache of completed analysis for closed files.
(require racket/list
         "../../common/source.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt"
         "state.rkt")

(provide retained-analysis
         matching-retained
         remove-retained!
         retain!)

(define (retained-analysis controller source)
  (define cache (Source-Controller-retained controller))
  (define entry (assoc (source-id source) (Retained-Cache-entries cache)))
  (and entry (cdr entry)))

(define (matching-retained controller snapshot)
  (define available (retained-analysis controller (Source-Snapshot-source snapshot)))
  (and available
       (equal? (Source-Snapshot-text snapshot)
               (Source-Snapshot-text (Available-Analysis-snapshot available)))
       available))

;; Eviction drops the closed completed job too, so immutable facts cannot
;; bypass the cache bound. Existing waiters still own their broadcast outcome.
(define (remove-retained! controller source)
  (define job (hash-ref (controller-jobs controller) (cons (source-id source) 'analysis) #f))
  (when (and job (eq? 'terminal (Job-state job)) (not (source-buffer controller source)))
    (forget-job! controller job))
  (define cache (Source-Controller-retained controller))
  (set-Retained-Cache-entries!
    cache
    (filter-not (lambda (entry) (equal? (source-id source) (car entry)))
                (Retained-Cache-entries cache))))

(define (retain! controller source available)
  (remove-retained! controller source)
  (define cache (Source-Controller-retained controller))
  (define entries
    (append (Retained-Cache-entries cache) (list (cons (source-id source) available))))
  (set-Retained-Cache-entries! cache entries)
  (when (> (length entries) (Retained-Cache-limit cache))
    (define oldest (cdr (first entries)))
    (remove-retained! controller (Source-Snapshot-source (Available-Analysis-snapshot oldest)))))
