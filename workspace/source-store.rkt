#lang racket/base

;; Per-source table of current knowledge, keyed by `source-id`. Buffer
;; contents stay with the caller; this store keeps only an opaque handle and
;; never reads files.
;;
;; There is no lock. Callers must serialize all access, including a lookup
;; that installs the default record.
;;
;; The first lookup for a file installs an unknown disk record. A virtual
;; source exists in this table only while its buffer is open; lookups of an
;; absent virtual source return #f without inserting a record.
;;
;; Replacing or removing a view posts its `changed` event permanently, so
;; waiters cannot miss an invalidation.
(require racket/contract
         racket/match
         "../common/source.rkt")

(provide (struct-out Source-View)
         (struct-out Source-Snapshot)
         Source-Snapshot-id
         Source-Snapshot-uri
         Source-View-id
         Source-Store?
         make-source-store
         source-store-view
         source-store-buffer
         source-store-open!
         source-store-edit!
         source-store-close!
         source-store-invalidate!
         source-store-observe!
         source-store-disk-snapshot)

;; Public handle for one source's current knowledge.
;;
;; source: Descriptor this table retains for the id. Open stores the caller's
;;   URI spelling; later edits reuse that descriptor rather than the lookup
;;   argument.
;; revision: Lifetime token, fresh whenever this view is replaced, even when
;;   text is unchanged (open, close, or explicit invalidation). It is stable
;;   within one view. Callers compare it with `eq?`.
;; origin: Open buffer or disk file without a buffer. Closing a file returns
;;   it to disk; closing a virtual source removes its view.
;; content-state: `text-available` means buffer or cached disk text is available.
;;   `unknown` means no trusted content observation remains: the file has not
;;   been read, its observation was invalidated, or a read failed.
;;   `file-not-found` means the last disk read established that the path was
;;   not found. Read errors are reported separately by the caller.
;; changed: Becomes ready when this view is superseded and stays ready, so a
;;   waiter cannot miss the invalidation.
;;
;; An open buffer always uses `text-available`. A disk file uses all three states.
(struct/contract Source-View
  ([source Source?]
   [revision symbol?]
   [origin (or/c 'buffer 'disk)]
   [content-state (or/c 'text-available 'unknown 'file-not-found)]
   [changed evt?])
  #:transparent)

;; Frozen text bound to one view revision. This module constructs snapshots
;; only for disk/`text-available` views; callers may build the same struct from
;; buffer text.
;;
;; source: Descriptor that names this text. A disk snapshot may use a
;;   path-derived address rather than the URI spelling stored on the view.
;; revision: The view revision from which this text was frozen.
;; origin: Buffer or disk. A closed virtual source has no snapshot.
(struct/contract Source-Snapshot
  ([source Source?]
   [revision symbol?]
   [origin (or/c 'buffer 'disk)]
   [text (and/c string? immutable?)])
  #:transparent)

;; One table entry. Transitions replace the whole record or remove it.
;;
;; signal: Posted once when this record is replaced or removed, making the
;;   old view's `changed` event ready permanently.
;; buffer: Opaque handle owned by the caller, or `#f` when no buffer is open.
;;   This store never reads it.
;; disk-text: Cached file text for a disk/`text-available` view, or `#f` when this
;;   entry stores no text.
;;
;; A buffer view always uses `text-available`: it holds a buffer handle and no
;; disk text. A disk/`text-available` view holds disk text and no buffer. The
;; other content states hold neither.
(struct/contract Source-Record
  ([view Source-View?]
   [signal semaphore?]
   [buffer any/c]
   [disk-text (or/c #f (and/c string? immutable?))]))

;; Opaque table of per-source records.
;;
;; table: Uses `equal?` so reconstructed source IDs find the same record.
;;   The field stays private so callers cannot bypass the transitions.
(struct/contract Source-Store
  ([table (hash/c Source-Id? Source-Record? #:immutable #f)]))

(define (Source-Snapshot-id snapshot)
  (source-id (Source-Snapshot-source snapshot)))

(define (Source-Snapshot-uri snapshot)
  (source-uri (Source-Snapshot-source snapshot)))

(define (Source-View-id view)
  (source-id (Source-View-source view)))

(define (make-source-store)
  (Source-Store (make-hash)))

;; The view's `changed` event starts unready; a later transition posts
;; `signal` when it replaces or removes this view.
(define (make-record source origin content-state buffer disk-text)
  (define signal (make-semaphore 0))
  (define view
    (Source-View source (gensym 'revision) origin content-state (semaphore-peek-evt signal)))
  (Source-Record view signal buffer disk-text))

;; Current record, or #f for an absent virtual source. Only files acquire a
;; default record on lookup because only files have a disk fallback.
(define (record-for store source)
  (if (source-file-path source)
      (hash-ref! (Source-Store-table store) (source-id source)
                 (lambda () (make-record source 'disk 'unknown #f #f)))
      (hash-ref (Source-Store-table store) (source-id source) #f)))

;; Return #f when a virtual source has no open buffer.
(define (source-store-view store source)
  (define record (record-for store source))
  (and record (Source-Record-view record)))

(define (source-store-buffer store source)
  (define record (record-for store source))
  (and record (Source-Record-buffer record)))

;; Install the complete successor record before posting the replaced view's
;; signal, so a waiter that wakes always observes the new record.
(define (replace-view! store source origin content-state buffer disk-text)
  (define previous (hash-ref (Source-Store-table store) (source-id source) #f))
  (define next (make-record source origin content-state buffer disk-text))
  (hash-set! (Source-Store-table store) (source-id source) next)
  (when previous (semaphore-post (Source-Record-signal previous)))
  (Source-Record-view next))

;; Open a buffer for this source. Store this `source` as the descriptor so
;; later edits keep its URI spelling, and drop any cached disk text.
(define (source-store-open! store source buffer)
  (replace-view! store source 'buffer 'text-available buffer #f))

;; Issue a new buffer revision for an open source and drop cached disk text.
;; The stored descriptor stays in place; `source` is only the lookup key.
(define (source-store-edit! store source)
  (define record (record-for store source))
  (unless (and record (Source-Record-buffer record))
    (error 'source-store-edit! "source is not open: ~a" source))
  (replace-view! store (Source-View-source (Source-Record-view record))
                 'buffer 'text-available (Source-Record-buffer record) #f))

;; Drop the buffer handle and cached disk text. A file returns to unknown
;; disk; a virtual source is removed and returns #f. Remove before signaling
;; so a waiter that wakes cannot observe the closed buffer as current.
(define (source-store-close! store source)
  (cond
    [(source-file-path source) (replace-view! store source 'disk 'unknown #f #f)]
    [else
     (define previous (record-for store source))
     (hash-remove! (Source-Store-table store) (source-id source))
     (when previous
       (semaphore-post (Source-Record-signal previous)))
     #f]))

;; Drop cached disk text without reading.
;;
;; A virtual source is unchanged. A file with an open buffer keeps its buffer
;; and gets a new revision. A file without one returns to unknown disk.
(define (source-store-invalidate! store source)
  (define record (record-for store source))
  (cond
    [(not (source-file-path source)) (and record (Source-Record-view record))]
    [(Source-Record-buffer record) (source-store-edit! store source)]
    [else (source-store-close! store source)]))

;; Install a completed disk observation only while `started-view`, the view
;; captured when the read started, is still the current disk view. Any other
;; view returns `#f`, so a superseded revision cannot be revived.
;;
;; `text-available` requires string text; `file-not-found` and `unknown` require
;; no text. Use `unknown` for a failed read, dropping any previous disk text.
;; Every such failure issues a fresh revision, including repeated failures;
;; error details belong to the caller's read result rather than this store.
(define (source-store-observe! store started-view content-state [text #f])
  (unless (case content-state
            [(text-available) (string? text)]
            [(file-not-found unknown) (not text)]
            [else #f])
    (raise-arguments-error 'source-store-observe!
                           (string-append "expected text-available with string text, "
                                          "or file-not-found/unknown without text")
                           "content-state" content-state "text" text))
  (define record (record-for store (Source-View-source started-view)))
  (define current (and record (Source-Record-view record)))
  (match current
    [(struct* Source-View ([origin 'disk] [content-state current-content-state]))
     #:when (eq? started-view current)
     ;; Equal successful observations preserve the current revision.
     (if (and (eq? content-state current-content-state)
              (not (eq? content-state 'unknown))
              (equal? text (Source-Record-disk-text record)))
         current
         (replace-view! store (Source-View-source current)
                        'disk content-state #f (and text (string->immutable-string text))))]
    [_ #f]))

;; Snapshot of cached disk text when the current view is disk and `text-available`;
;; otherwise `#f`.
(define (source-store-disk-snapshot store source)
  (define record (record-for store source))
  (match (and record (Source-Record-view record))
    [(struct* Source-View ([source disk-source] [revision revision]
                           [origin 'disk] [content-state 'text-available]))
     ;; Rebuild the source descriptor from the file path so URI aliases share
     ;; one snapshot address. An earlier buffer open may have stored a
     ;; different spelling of the same path.
     (Source-Snapshot (path->source (source-file-path disk-source))
                      revision 'disk
                      (Source-Record-disk-text record))]
    [_ #f]))
