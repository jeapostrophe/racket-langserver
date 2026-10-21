#lang racket/base

;; Ordered disk observations, owned by one consumer. Commands accepted during
;; an active read share its successor; only the consumer decides what an
;; observation means for source authority and the response to its waiters.
(require racket/async-channel
         racket/contract
         racket/file
         "../../common/source.rkt"
         (only-in "../../workspace/source-store.rkt" Source-View? Source-View-source)
         "completion-event.rkt"
         "supervised-worker.rkt")

(provide Disk-Reader?
         Disk-Read-Completed?
         (struct-out Disk-Missing)
         (struct-out Disk-Read-Failure)
         (contract-out
           [make-disk-reader
            (->* (async-channel?)
                 (#:read (-> path? any/c) #:timeout (and/c real? (>=/c 0)))
                 Disk-Reader?)]
           [read-source-text (-> path? (or/c string? Disk-Missing?))]
           [request-disk-read! (-> Disk-Reader? Source-View? Completion-Event?)]
           [finish-disk-read!
            (-> Disk-Reader? Disk-Read-Completed?
                (-> Source-View? any/c (values any/c Source-View?)) void?)]
           [cancel-disk-read! (-> Disk-Reader? Source? any/c void?)]
           [stop-disk-reader! (-> Disk-Reader? any/c void?)]))

(struct/contract Disk-Missing () #:transparent)

(struct/contract Disk-Read-Failure ([reason any/c]) #:transparent)

;; All records below are private. Attempt identity rejects messages left in
;; the channel after cancellation, including when a new lane has the same id.
(struct/contract Read-Attempt
  ([started-view Source-View?]
   [completion Completion-Event?]
   [worker (or/c Supervised-Worker? #f)])
  #:mutable)

(struct/contract Read-Lane
  ([active Read-Attempt?]
   [pending (or/c Completion-Event? #f)])
  #:mutable)

(struct/contract Disk-Reader
  ([events async-channel?]
   [read (-> path? any/c)]
   [timeout (and/c real? (>=/c 0))]
   [lanes (hash/c Source-Id? Read-Lane? #:immutable #f)]
   [stopped? boolean?]
   [stop-response any/c])
  #:mutable)

(struct/contract Disk-Read-Completed
  ([source Source?]
   [attempt Read-Attempt?]
   [result (or/c string? Disk-Missing? Disk-Read-Failure?)]))

(define (make-disk-reader events #:read [read read-source-text] #:timeout [timeout 3])
  (Disk-Reader events read timeout (make-hash) #f #f))

;; POSIX ENOENT/ENOTDIR; Windows FILE_NOT_FOUND/PATH_NOT_FOUND.
(define (missing-file-errno? errno)
  (member errno '((2 . posix) (20 . posix) (2 . windows) (3 . windows))))

(define (read-source-text path)
  (with-handlers ([exn:fail:filesystem:errno?
                   (lambda (e)
                     (if (missing-file-errno? (exn:fail:filesystem:errno-errno e))
                         (Disk-Missing)
                         (raise e)))])
    (file->string path)))

(define (start-read! reader started-view completion)
  (define source (Source-View-source started-view))
  (define attempt (Read-Attempt started-view completion #f))
  (define worker
    (start-supervised-worker!
      (Disk-Reader-events reader) (Disk-Reader-timeout reader)
      (lambda ()
        (define result ((Disk-Reader-read reader) (source-file-path source)))
        (cond
          [(string? result) (string->immutable-string result)]
          [(or (Disk-Missing? result) (Disk-Read-Failure? result)) result]
          [else (Disk-Read-Failure result)]))
      #:fail Disk-Read-Failure
      #:exited 'reader-exited
      #:timed-out 'read-timeout
      #:done (lambda (result) (Disk-Read-Completed source attempt result))))
  (set-Read-Attempt-worker! attempt worker)
  attempt)

(define (request-disk-read! reader started-view)
  (cond
    [(Disk-Reader-stopped? reader)
     (define completion (make-completion-event))
     (resolve-completion! completion (Disk-Reader-stop-response reader))
     completion]
    [else
     (define lanes (Disk-Reader-lanes reader))
     (define key (source-id (Source-View-source started-view)))
     (define lane (hash-ref lanes key #f))
     (cond
       [lane
        (unless (Read-Lane-pending lane)
          (set-Read-Lane-pending! lane (make-completion-event)))
        (Read-Lane-pending lane)]
       [else
        (define completion (make-completion-event))
        (hash-set! lanes key (Read-Lane (start-read! reader started-view completion) #f))
        completion])]))

(define (finish-disk-read! reader message accept)
  (define key (source-id (Disk-Read-Completed-source message)))
  (define lanes (Disk-Reader-lanes reader))
  (define lane (hash-ref lanes key #f))
  (define attempt (Disk-Read-Completed-attempt message))
  (when (and lane (eq? attempt (Read-Lane-active lane)))
    ;; Acceptance must finish before any response is visible. The successor
    ;; captures the accepted view, never the view of a command queued earlier.
    (define-values (response current-view)
      (accept (Read-Attempt-started-view attempt) (Disk-Read-Completed-result message)))
    (resolve-completion! (Read-Attempt-completion attempt) response)
    (define pending (Read-Lane-pending lane))
    (cond
      [pending
       (set-Read-Lane-pending! lane #f)
       (set-Read-Lane-active! lane (start-read! reader current-view pending))]
      [else (hash-remove! lanes key)]))
  (void))

(define (cancel-lane! lane response)
  (define active (Read-Lane-active lane))
  (stop-supervised-worker! (Read-Attempt-worker active))
  (resolve-completion! (Read-Attempt-completion active) response)
  (when (Read-Lane-pending lane)
    (resolve-completion! (Read-Lane-pending lane) response)))

(define (cancel-disk-read! reader source response)
  (define lanes (Disk-Reader-lanes reader))
  (define key (source-id source))
  (define lane (hash-ref lanes key #f))
  (when lane
    (hash-remove! lanes key)
    (cancel-lane! lane response))
  (void))

(define (stop-disk-reader! reader response)
  (unless (Disk-Reader-stopped? reader)
    (set-Disk-Reader-stopped?! reader #t)
    (set-Disk-Reader-stop-response! reader response)
    (define lanes (Disk-Reader-lanes reader))
    (define abandoned (hash-values lanes))
    (hash-clear! lanes)
    (for ([lane (in-list abandoned)])
      (cancel-lane! lane response)))
  (void))
