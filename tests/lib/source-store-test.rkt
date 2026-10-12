#lang racket/base

;; Source-store transitions with no controller and no disk. These checks cover
;; observation, authority tickets, and which text the store retains.
(require "../../common/path-util.rkt"
         rackunit
         "../../workspace/source-store.rkt")

(define path (build-path (current-directory) "source.rkt"))

(define (changed? view)
  (and (sync/timeout 0 (Source-View-changed view)) #t))

(module+ test
  (test-case "source contracts reject invalid metadata and mutable snapshot text"
    (define revision (gensym 'revision))
    (check-exn exn:fail:contract?
               (lambda () (Source-View path revision 'unknown 'present never-evt)))
    (check-exn exn:fail:contract?
               (lambda () (Source-View path revision 'disk 'unknown never-evt)))
    (check-exn exn:fail:contract?
               (lambda () (Source-View path revision 'disk 'present #f)))
    (check-exn exn:fail:contract?
               (lambda () (Source-Snapshot (File-Source (path->uri path) path) revision 'disk (string-copy "text"))))
    (check-exn exn:fail:contract?
               (lambda () (Source-Snapshot "source.rkt" revision 'disk "text"))))

  (test-case "unchanged exact disk text preserves a revision and broadcasts changes"
    (define store (make-source-store))
    (define initial (source-store-view store path))
    (check-eq? 'unobserved (Source-View-state initial))
    (define first (source-store-observe! store initial 'present "one"))
    (check-true (changed? initial))
    (check-true (changed? initial) "invalidation is persistent for all waiters")
    (check-eq? first (source-store-observe! store first 'present (string-copy "one")))
    (check-false (changed? first))
    (define second (source-store-observe! store first 'present "two"))
    (check-not-eq? (Source-View-revision first) (Source-View-revision second))
    (check-true (changed? first))
    (define snapshot (source-store-disk-snapshot store path))
    (check-equal? "two" (Source-Snapshot-text snapshot))
    (check-true (immutable? (Source-Snapshot-text snapshot))))

  (test-case "missing, unavailable, and recreated sources stay distinct"
    (define store (make-source-store))
    (define missing (source-store-observe! store (source-store-view store path) 'missing))
    (check-eq? missing (source-store-observe! store missing 'missing))
    (check-false (source-store-disk-snapshot store path))
    (define unavailable (source-store-observe! store missing 'unavailable))
    (check-true (changed? missing))
    (check-eq? 'unavailable (Source-View-state unavailable))
    (define recreated (source-store-observe! store unavailable 'present "new"))
    (check-true (changed? unavailable))
    (check-eq? 'present (Source-View-state recreated)))

  (test-case "close discards unsaved authority and rejects old disk tickets"
    (define store (make-source-store))
    (define ticket (source-store-view store path))
    (define buffer (box 'editable-text))
    (define opened (source-store-open! store path buffer))
    (check-eq? buffer (source-store-buffer store path))
    (check-false (source-store-observe! store ticket 'present "old read"))
    (define edited (source-store-edit! store path))
    (check-true (changed? opened))
    (define closed (source-store-close! store path))
    (check-true (changed? edited))
    (check-eq? 'unobserved (Source-View-state closed))
    (check-false (source-store-buffer store path))
    (check-false (source-store-disk-snapshot store path))
    (check-false (source-store-observe! store ticket 'present "old read"))
    (define reopened (source-store-open! store path buffer))
    (check-not-eq? (Source-View-revision opened) (Source-View-revision reopened))
    (check-false (source-store-observe! store closed 'present "disk"))
    (check-eq? reopened (source-store-view store path)))

  (test-case "explicit invalidation prevents identical text from reviving a revision"
    (define store (make-source-store))
    (define old (source-store-observe! store (source-store-view store path) 'present "same"))
    (define invalidated (source-store-invalidate! store path))
    (check-true (changed? old))
    (check-false (source-store-observe! store old 'present "late"))
    (define fresh (source-store-observe! store invalidated 'present "same"))
    (check-not-eq? (Source-View-revision old) (Source-View-revision fresh))))
