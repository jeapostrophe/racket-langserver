#lang racket/base

;; The session consumer owns this store. Views are immutable lifetime handles;
;; their change events broadcast invalidation without consuming the signal.
;; Buffer text stays in SafeDoc. This module retains only observed disk text
;; and does not read files. Close leaves files unobserved and virtual sources
;; closed. A virtual record can never acquire disk text.
(require racket/contract
         racket/match
         "../common/path-util.rkt")

(provide (struct-out Source-View)
         (struct-out Source-Snapshot)
         Source-Snapshot-source-id
         make-source-store
         source-store-view
         source-store-buffer
         source-store-open!
         source-store-edit!
         source-store-close!
         source-store-invalidate!
         source-store-observe!
         source-store-disk-snapshot)

(struct/contract Source-View
  ([source-id source-id?]
   [revision symbol?]
   [origin (or/c 'buffer 'disk 'closed)]
   [state (or/c 'present 'unobserved 'missing 'unavailable)]
   [changed evt?])
  #:transparent)

(struct/contract Source-Snapshot
  ([source source?]
   [revision symbol?]
   [origin (or/c 'buffer 'disk)]
   [text (and/c string? immutable?)])
  #:transparent)

(struct/contract Source-Record
  ([view Source-View?]
   [signal semaphore?]
   ;; The adapter owns the buffer type; the store retains an opaque handle.
   [buffer any/c]
   [disk-text (or/c #f (and/c string? immutable?))])
  #:mutable)

(define (Source-Snapshot-source-id snapshot)
  (source-id (Source-Snapshot-source snapshot)))

(define (make-source-store)
  (make-hash))

(define (fresh-signal-view source-id origin state)
  (define signal (make-semaphore 0))
  (values signal
          (Source-View source-id (gensym 'revision) origin state (semaphore-peek-evt signal))))

(define (reset-origin-state source-id)
  (if (Virtual-Id? source-id)
      (values 'closed 'missing)
      (values 'disk 'unobserved)))

(define (new-record source-id)
  (define-values (origin state) (reset-origin-state source-id))
  (define-values (signal view) (fresh-signal-view source-id origin state))
  (Source-Record view signal #f #f))

(define (record-for store source-id)
  (hash-ref! store source-id (lambda () (new-record source-id))))

(define (source-store-view store source-id)
  (Source-Record-view (record-for store source-id)))

(define (source-store-buffer store source-id)
  (Source-Record-buffer (record-for store source-id)))

(define (replace-view! record origin state buffer text)
  (semaphore-post (Source-Record-signal record))
  (define-values (signal view)
    (fresh-signal-view (Source-View-source-id (Source-Record-view record)) origin state))
  (set-Source-Record-view! record view)
  (set-Source-Record-signal! record signal)
  (set-Source-Record-buffer! record buffer)
  (set-Source-Record-disk-text! record text)
  view)

(define (source-store-open! store source-id buffer)
  (replace-view! (record-for store source-id) 'buffer 'present buffer #f))

(define (source-store-edit! store source-id)
  (define record (record-for store source-id))
  (unless (Source-Record-buffer record)
    (error 'source-store-edit! "source is not open: ~a" source-id))
  (replace-view! record 'buffer 'present (Source-Record-buffer record) #f))

(define (source-store-close! store source-id)
  (define-values (origin state) (reset-origin-state source-id))
  (replace-view! (record-for store source-id) origin state #f #f))

;; An open file keeps its buffer and gets a new revision. A closed file
;; returns to an unobserved disk view. Invalidation does not read either one.
(define (source-store-invalidate! store source-id)
  (define record (record-for store source-id))
  (cond
    [(Virtual-Id? source-id) (Source-Record-view record)]
    [(Source-Record-buffer record) (source-store-edit! store source-id)]
    [else (source-store-close! store source-id)]))

;; A read ticket is the view captured when that read starts. Authority changes
;; replace it, so even identical text cannot revive a closed buffer incarnation.
(define (source-store-observe! store ticket state [text #f])
  (define record (record-for store (Source-View-source-id ticket)))
  (define current (Source-Record-view record))
  (match current
    [(struct* Source-View ([origin 'disk] [state current-state]))
     #:when (eq? ticket current)
     ;; Exact text reuses a disk revision. A read error is not observed text,
     ;; so unavailable always replaces the view, including a repeated failure.
     (if (and (eq? state current-state)
              (not (eq? state 'unavailable))
              (equal? text (Source-Record-disk-text record)))
         current
         (replace-view! record 'disk state #f (and text (string->immutable-string text))))]
    [_ #f]))

(define (source-store-disk-snapshot store source-id)
  (define record (record-for store source-id))
  (match (Source-Record-view record)
    [(struct* Source-View ([revision revision] [origin 'disk] [state 'present]))
     (Source-Snapshot (File-Source (path->uri source-id) source-id)
                      revision 'disk
                      (Source-Record-disk-text record))]
    [_ #f]))
