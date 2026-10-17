#lang racket/base

;; Buffer authority transitions: open, edit, close, and invalidation. Each one
;; replaces the source revision and stops work captured under the old one.
(require racket/class
         "../../common/source.rkt"
         "../../doclib/doc.rkt"
         "../../workspace/current.rkt"
         "../../workspace/state.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt"
         "../safedoc.rkt"
         "jobs.rkt"
         "reads.rkt"
         "retained.rkt"
         "state.rkt")

(provide source-controller-open!
         source-controller-edited!
         source-controller-close!
         source-controller-invalidate!)

(define (source-controller-open! controller source safe-doc)
  (with-write-safedoc safe-doc
    (lambda (sd)
      (set-SafeDoc-source-view! sd
                                (source-store-open! (Source-Controller-store controller)
                                                    source sd))))
  (invalidate-jobs! controller source)
  (invalidate-reads! controller source)
  (define snapshot
    (and (retained-analysis controller source) (source-controller-snapshot controller source)))
  (define available (and snapshot (matching-retained controller snapshot)))
  ;; Take ownership even on a mismatch: an older text must not remain eligible
  ;; after this buffer starts adjusting its trace.
  (remove-retained! controller source)
  (when available
    (define product (Available-Analysis-product available))
    (define restored-product
      (struct-copy Analysis-Product product
        [contribution (send (Analysis-Product-trace product) get-contribution
                            #:source (Source-Snapshot-source snapshot))]))
    (with-write-safedoc safe-doc
      (lambda (sd)
        (install-analysis! controller sd snapshot restored-product)))))

;; Called within the edit's existing write-lock section. This updates the
;; authoritative record and its SafeDoc projection before another reader enters.
(define (source-controller-edited! controller source sd)
  (set-SafeDoc-source-view! sd
                            (source-store-edit! (Source-Controller-store controller) source))
  (set-SafeDoc-analysis! sd #f)
  (set-SafeDoc-available! sd #f)
  (remove-retained! controller source)
  (invalidate-jobs! controller source))

;; Close drops buffer authority without a read. Files become unobserved;
;; virtual sources become closed and have no disk fallback.
(define (source-controller-close! controller source safe-doc)
  (define uri
    (with-write-safedoc safe-doc
      (lambda (sd)
        (define available (SafeDoc-available sd))
        (define closed-view (source-store-close! (Source-Controller-store controller) source))
        (when (and (eq? 'disk (Source-View-origin closed-view)) available
                   (eq? (Source-Snapshot-revision (Available-Analysis-snapshot available))
                        (Source-View-revision (SafeDoc-source-view sd))))
          (doc-detach-analysis! (SafeDoc-doc sd))
          (retain! controller source available))
        (set-SafeDoc-source-view! sd #f)
        (set-SafeDoc-analysis! sd #f)
        (set-SafeDoc-available! sd #f)
        (Doc-uri (SafeDoc-doc sd)))))
  (invalidate-jobs! controller source)
  (when (source-file-path source)
    (workspace-remove-path! current-workspace (source-file-path source)))
  (publish-diagnostics! controller uri '())
  (invalidate-reads! controller source)
  (pump-jobs! controller))

(define (source-controller-invalidate! controller source)
  (when (source-file-path source)
    (remove-retained! controller source)
    (define store (Source-Controller-store controller))
    (define buffer (source-store-buffer store source))
    (if buffer
        (with-write-safedoc buffer
          (lambda (sd)
            (set-SafeDoc-source-view! sd (source-store-invalidate! store source))
            (set-SafeDoc-analysis! sd #f)
            (set-SafeDoc-available! sd #f)))
        (source-store-invalidate! store source))
    (invalidate-jobs! controller source)
    (invalidate-reads! controller source)
    (when buffer (source-controller-analyze! controller source))
    (pump-jobs! controller)))
