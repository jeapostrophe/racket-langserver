#lang racket/base

;; The session consumer owns this store. Views are immutable lifetime handles;
;; their change events broadcast invalidation without consuming the signal.
;; Buffer text stays in SafeDoc. This module retains only observed disk text
;; and does not read files. Close leaves files unobserved and virtual sources
;; closed. A virtual record can never acquire disk text.
(require racket/contract
         racket/match
         "../common/source.rkt")

(provide (struct-out Source-View)
         (struct-out Source-Snapshot)
         Source-Snapshot-id
         Source-Snapshot-uri
         Source-View-id
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
  ([source Source?]
   [revision symbol?]
   [origin (or/c 'buffer 'disk 'closed)]
   [state (or/c 'present 'unobserved 'missing 'unavailable)]
   [changed evt?])
  #:transparent)

(struct/contract Source-Snapshot
  ([source Source?]
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

(define (Source-Snapshot-id snapshot)
  (source-id (Source-Snapshot-source snapshot)))

(define (Source-Snapshot-uri snapshot)
  (source-uri (Source-Snapshot-source snapshot)))

(define (Source-View-id view)
  (source-id (Source-View-source view)))

(define (make-source-store)
  (make-hash))

(define (fresh-signal-view source origin state)
  (define signal (make-semaphore 0))
  (values signal
          (Source-View source (gensym 'revision) origin state (semaphore-peek-evt signal))))

(define (reset-origin-state source)
  (if (source-file-path source)
      (values 'disk 'unobserved)
      (values 'closed 'missing)))

(define (new-record source)
  (define-values (origin state) (reset-origin-state source))
  (define-values (signal view) (fresh-signal-view source origin state))
  (Source-Record view signal #f #f))

(define (record-for store source)
  (hash-ref! store (source-id source) (lambda () (new-record source))))

(define (source-store-view store source)
  (Source-Record-view (record-for store source)))

(define (source-store-buffer store source)
  (Source-Record-buffer (record-for store source)))

(define (replace-view! record source origin state buffer text)
  (semaphore-post (Source-Record-signal record))
  (define-values (signal view)
    (fresh-signal-view source origin state))
  (set-Source-Record-view! record view)
  (set-Source-Record-signal! record signal)
  (set-Source-Record-buffer! record buffer)
  (set-Source-Record-disk-text! record text)
  view)

(define (source-store-open! store source buffer)
  (replace-view! (record-for store source) source 'buffer 'present buffer #f))

(define (source-store-edit! store source)
  (define record (record-for store source))
  (unless (Source-Record-buffer record)
    (error 'source-store-edit! "source is not open: ~a" source))
  (replace-view! record (Source-View-source (Source-Record-view record))
                 'buffer 'present (Source-Record-buffer record) #f))

(define (source-store-close! store source)
  (define-values (origin state) (reset-origin-state source))
  (replace-view! (record-for store source) source origin state #f #f))

;; An open file keeps its buffer and gets a new revision. A closed file
;; returns to an unobserved disk view. Invalidation does not read either one.
(define (source-store-invalidate! store source)
  (define record (record-for store source))
  (cond
    [(not (source-file-path source)) (Source-Record-view record)]
    [(Source-Record-buffer record) (source-store-edit! store source)]
    [else (source-store-close! store source)]))

;; A read ticket is the view captured when that read starts. Authority changes
;; replace it, so even identical text cannot revive a closed buffer incarnation.
(define (source-store-observe! store ticket state [text #f])
  (define record (record-for store (Source-View-source ticket)))
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
         (replace-view! record (Source-View-source current)
                        'disk state #f (and text (string->immutable-string text))))]
    [_ #f]))

(define (source-store-disk-snapshot store source)
  (define record (record-for store source))
  (match (Source-Record-view record)
    [(struct* Source-View ([source disk-source] [revision revision]
                           [origin 'disk] [state 'present]))
     ;; Disk observations use a path-derived address, independent of the URI
     ;; spelling of a previously open buffer.
     (Source-Snapshot (path->source (source-file-path disk-source))
                      revision 'disk
                      (Source-Record-disk-text record))]
    [_ #f]))
