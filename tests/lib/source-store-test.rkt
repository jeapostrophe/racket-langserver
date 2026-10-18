#lang racket/base

;; Source-store transitions with no controller and no disk. These checks cover
;; observation, authority tickets, and which text the store retains.
(require "../../common/source.rkt"
         "../../common/path-util.rkt"
         rackunit
         "../../workspace/source-store.rkt")

(define path (build-path (current-directory) "source.rkt"))

(define (changed? view)
  (and (sync/timeout 0 (Source-View-changed view)) #t))

(module+ test
  (test-case "source contracts reject invalid metadata and mutable snapshot text"
    (define revision (gensym 'revision))
    (check-exn exn:fail:contract?
               (lambda () (Source-View (path->source path) revision 'unknown 'text-available never-evt)))
    (check-exn exn:fail:contract?
               (lambda () (Source-View (path->source path) revision 'disk 'invalid-state never-evt)))
    (check-exn exn:fail:contract?
               (lambda () (Source-View (path->source path) revision 'disk 'text-available #f)))
    (check-exn exn:fail:contract?
               (lambda ()
                 (Source-Snapshot (path->source path)
                                  revision 'disk (string-copy "text"))))
    (check-exn exn:fail:contract?
               (lambda () (Source-Snapshot #f revision 'disk "text"))))

  (test-case "virtual sources exist only while their buffer is open"
    (define store (make-source-store))
    (define source (uri->source "untitled:store-lifecycle"))
    (define (check-absent)
      (check-false (source-store-view store source))
      (check-false (source-store-buffer store source))
      (check-false (source-store-disk-snapshot store source))
      (check-false (source-store-invalidate! store source))
      (check-exn #rx"source is not open" (lambda () (source-store-edit! store source))))
    (check-absent)
    (check-false (source-store-close! store source))
    (define buffer (box 'editable-text))
    (define opened (source-store-open! store source buffer))
    (check-eq? 'buffer (Source-View-origin opened))
    (check-eq? buffer (source-store-buffer store source))
    (check-eq? opened (source-store-invalidate! store source))
    (check-false (source-store-close! store source))
    (check-true (changed? opened))
    (check-true (changed? opened) "all waiters retain the close signal")
    (check-absent)
    (check-false (source-store-observe! store opened 'text-available "late"))
    (check-absent)
    (check-false (source-store-close! store source))
    (define reopened (source-store-open! store source buffer))
    (check-not-eq? (Source-View-revision opened) (Source-View-revision reopened))
    (check-false (changed? reopened))
    (check-false (source-store-observe! store opened 'text-available "late"))
    (check-eq? reopened (source-store-view store source)))

  (test-case "invalid observations leave the current revision and text intact"
    (define store (make-source-store))
    (define source (path->source path))
    (define current
      (source-store-observe! store (source-store-view store source) 'text-available "kept"))
    (for ([observation (in-list '((text-available #f)
                                  (text-available 42)
                                  (file-not-found "text")
                                  (unknown "text")
                                  (invalid-state #f)))])
      (check-exn exn:fail:contract?
                 (lambda () (apply source-store-observe! store current observation)))
      (check-eq? current (source-store-view store source))
      (check-false (changed? current))
      (check-equal? "kept" (Source-Snapshot-text (source-store-disk-snapshot store source)))))

  (test-case "unchanged exact disk text preserves a revision and broadcasts changes"
    (define store (make-source-store))
    (define initial (source-store-view store (path->source path)))
    (check-eq? 'unknown (Source-View-content-state initial))
    (define first (source-store-observe! store initial 'text-available "one"))
    (check-true (changed? initial))
    (check-true (changed? initial) "invalidation is persistent for all waiters")
    (check-eq? first (source-store-observe! store first 'text-available (string-copy "one")))
    (check-false (changed? first))
    (define second (source-store-observe! store first 'text-available "two"))
    (check-not-eq? (Source-View-revision first) (Source-View-revision second))
    (check-true (changed? first))
    (define snapshot (source-store-disk-snapshot store (path->source path)))
    (check-equal? "two" (Source-Snapshot-text snapshot))
    (check-true (immutable? (Source-Snapshot-text snapshot))))

  (test-case "failed reads replace not-found observations with unknown content"
    (define store (make-source-store))
    (define missing
      (source-store-observe! store (source-store-view store (path->source path)) 'file-not-found))
    (check-eq? missing (source-store-observe! store missing 'file-not-found))
    (check-false (source-store-disk-snapshot store (path->source path)))
    (define unknown (source-store-observe! store missing 'unknown))
    (check-true (changed? missing))
    (check-eq? 'unknown (Source-View-content-state unknown))
    (define failed-again (source-store-observe! store unknown 'unknown))
    (check-true (changed? unknown))
    (check-not-eq? unknown failed-again)
    (define recreated (source-store-observe! store failed-again 'text-available "new"))
    (check-true (changed? failed-again))
    (check-eq? 'text-available (Source-View-content-state recreated)))

  (test-case "close discards unsaved authority and rejects old disk tickets"
    (define store (make-source-store))
    (define ticket (source-store-view store (path->source path)))
    (define buffer (box 'editable-text))
    (define opened (source-store-open! store (path->source path) buffer))
    (check-eq? buffer (source-store-buffer store (path->source path)))
    (check-false (source-store-observe! store ticket 'text-available "old read"))
    (define edited (source-store-edit! store (path->source path)))
    (check-true (changed? opened))
    (define closed (source-store-close! store (path->source path)))
    (check-true (changed? edited))
    (check-eq? 'unknown (Source-View-content-state closed))
    (check-false (source-store-buffer store (path->source path)))
    (check-false (source-store-disk-snapshot store (path->source path)))
    (check-false (source-store-observe! store ticket 'text-available "old read"))
    (define reopened (source-store-open! store (path->source path) buffer))
    (check-not-eq? (Source-View-revision opened) (Source-View-revision reopened))
    (check-false (source-store-observe! store closed 'text-available "disk"))
    (check-eq? reopened (source-store-view store (path->source path))))

  (test-case "explicit invalidation prevents identical text from reviving a revision"
    (define store (make-source-store))
    (define old
      (source-store-observe! store (source-store-view store (path->source path)) 'text-available "same"))
    (define invalidated (source-store-invalidate! store (path->source path)))
    (check-true (changed? old))
    (check-false (source-store-observe! store old 'text-available "late"))
    (define fresh (source-store-observe! store invalidated 'text-available "same"))
    (check-not-eq? (Source-View-revision old) (Source-View-revision fresh))))
