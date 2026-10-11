#lang racket/base

;; The session consumer owns this store. Views are immutable lifetime handles;
;; their change events broadcast invalidation without consuming the signal.
;; Buffer text stays in SafeDoc. This module retains only observed disk text
;; and does not read files. Close leaves a disk view unobserved.
(require racket/contract
         racket/match)

(provide (struct-out Source-View)
         (struct-out Source-Snapshot)
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
  ([source-id path?]
   [revision symbol?]
   [origin (or/c 'buffer 'disk)]
   [state (or/c 'present 'unobserved 'missing 'unavailable)]
   [changed evt?])
  #:transparent)

(struct/contract Source-Snapshot
  ([source-id path?]
   [revision symbol?]
   [origin (or/c 'buffer 'disk)]
   [text (and/c string? immutable?)])
  #:transparent)

(struct/contract File-Record
  ([view Source-View?]
   [signal semaphore?]
   ;; The adapter owns the buffer type; the store retains an opaque handle.
   [buffer any/c]
   [disk-text (or/c #f (and/c string? immutable?))])
  #:mutable)

(define (make-source-store)
  (make-hash))

(define (fresh-signal-view source-id origin state)
  (define signal (make-semaphore 0))
  (values signal
          (Source-View source-id (gensym 'revision) origin state (semaphore-peek-evt signal))))

(define (new-record source-id)
  (define-values (signal view) (fresh-signal-view source-id 'disk 'unobserved))
  (File-Record view signal #f #f))

(define (record-for store source-id)
  (hash-ref! store source-id (lambda () (new-record source-id))))

(define (source-store-view store source-id)
  (File-Record-view (record-for store source-id)))

(define (source-store-buffer store source-id)
  (File-Record-buffer (record-for store source-id)))

(define (replace-view! record origin state buffer text)
  (semaphore-post (File-Record-signal record))
  (define-values (signal view)
    (fresh-signal-view (Source-View-source-id (File-Record-view record)) origin state))
  (set-File-Record-view! record view)
  (set-File-Record-signal! record signal)
  (set-File-Record-buffer! record buffer)
  (set-File-Record-disk-text! record text)
  view)

(define (source-store-open! store source-id buffer)
  (replace-view! (record-for store source-id) 'buffer 'present buffer #f))

(define (source-store-edit! store source-id)
  (define record (record-for store source-id))
  (unless (File-Record-buffer record)
    (error 'source-store-edit! "source is not open: ~a" source-id))
  (replace-view! record 'buffer 'present (File-Record-buffer record) #f))

(define (source-store-close! store source-id)
  (replace-view! (record-for store source-id) 'disk 'unobserved #f #f))

;; An open file keeps its buffer and gets a new revision. A closed file
;; returns to an unobserved disk view. Invalidation does not read either one.
(define (source-store-invalidate! store source-id)
  (define record (record-for store source-id))
  (if (File-Record-buffer record)
      (source-store-edit! store source-id)
      (source-store-close! store source-id)))

;; A read ticket is the view captured when that read starts. Authority changes
;; replace it, so even identical text cannot revive a closed buffer incarnation.
(define (source-store-observe! store ticket state [text #f])
  (define record (record-for store (Source-View-source-id ticket)))
  (define current (File-Record-view record))
  (match current
    [(struct* Source-View ([origin 'disk] [state current-state]))
     #:when (eq? ticket current)
     ;; Exact text reuses a disk revision. A read error is not observed text,
     ;; so unavailable always replaces the view, including a repeated failure.
     (if (and (eq? state current-state)
              (not (eq? state 'unavailable))
              (equal? text (File-Record-disk-text record)))
         current
         (replace-view! record 'disk state #f (and text (string->immutable-string text))))]
    [_ #f]))

(define (source-store-disk-snapshot store source-id)
  (define record (record-for store source-id))
  (match (File-Record-view record)
    [(struct* Source-View ([revision revision] [origin 'disk] [state 'present]))
     (Source-Snapshot source-id revision 'disk (File-Record-disk-text record))]
    [_ #f]))
