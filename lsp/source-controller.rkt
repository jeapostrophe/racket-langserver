#lang racket/base

;; Per-session owner of source records and ordered disk reads. The session
;; consumer applies every mutation and table read. Other threads submit
;; commands and wait on reply or change events; a read runs only on its
;; reader thread.
;; Source ids are exact filepaths from the URI boundary. Callers must preserve
;; that path through the URI round trip rather than add an alias map.
(require racket/async-channel
         racket/contract
         racket/file
         racket/match
         racket/port
         "../doclib/doc.rkt"
         "../workspace/source-store.rkt"
         "safedoc.rkt")

(provide make-source-controller
         Source-Controller?
         current-source-controller
         source-controller-events
         source-controller-call-evt
         source-controller-refresh-evt
         source-controller-handle!
         source-controller-view
         source-controller-snapshot
         source-controller-open!
         source-controller-edited!
         source-controller-close!
         source-controller-invalidate!
         source-controller-shutdown!
         (struct-out Source-Result)
         (struct-out Source-Failure)
         (struct-out Disk-Missing))

(struct/contract Source-Failure
  ([reason any/c])
  #:transparent)

(struct/contract Source-Result
  ([view Source-View?]
   [snapshot (or/c #f Source-Snapshot?)]
   [error (or/c #f Source-Failure?)])
  #:transparent)

(struct/contract Disk-Missing () #:transparent)

(struct/contract Completion
  ([result (and/c box? (not/c immutable?))]
   [ready semaphore?])
  #:transparent)

(struct/contract Read-Attempt
  ([ticket Source-View?]
   [batch Completion?]
   [custodian custodian?])
  #:transparent)

(struct/contract Read-Lane
  ([active Read-Attempt?]
   [next (or/c #f Completion?)])
  #:mutable)

(struct/contract Source-Controller
  ([store (and/c hash? (not/c immutable?))]
   [events async-channel?]
   [lanes (and/c hash? (not/c immutable?))]
   [read (-> path? any/c)]
   [timeout (and/c real? (>=/c 0))]
   [shutdown semaphore?])
  #:transparent)

(struct/contract Owner-Call
  ([proc (-> Source-Controller? any/c)]
   [reply Completion?])
  #:transparent)

(struct/contract Refresh
  ([source-id path?]
   [reply Completion?])
  #:transparent)

(struct/contract Read-Done
  ([source-id path?]
   [attempt Read-Attempt?]
   [result any/c])
  #:transparent)

(define (missing-file-errno? errno)
  (member errno '((2 . posix) (20 . posix) (2 . windows) (3 . windows))))

(define (read-source-text path)
  (with-handlers ([exn:fail:filesystem:errno?
                   (lambda (e)
                     (if (missing-file-errno? (exn:fail:filesystem:errno-errno e))
                         (Disk-Missing)
                         (raise e)))])
    (file->string path)))

(define (make-source-controller #:read [read read-source-text] #:read-timeout [timeout 90])
  (Source-Controller (make-source-store) (make-async-channel) (make-hash)
                     read timeout (make-semaphore 0)))

(define current-source-controller (make-parameter (make-source-controller)))

(define (source-controller-events controller)
  (Source-Controller-events controller))

(define (shutdown-evt controller)
  (semaphore-peek-evt (Source-Controller-shutdown controller)))

(define (stopped? controller)
  (and (sync/timeout 0 (shutdown-evt controller)) #t))

(define (make-completion)
  (Completion (box #f) (make-semaphore 0)))

(define (completion-evt completion)
  (wrap-evt (semaphore-peek-evt (Completion-ready completion))
            (lambda (_) (unbox (Completion-result completion)))))

(define (complete! completion result)
  (set-box! (Completion-result completion) result)
  (semaphore-post (Completion-ready completion)))

(define (submit controller make-command)
  (define reply (make-completion))
  (async-channel-put (Source-Controller-events controller) (make-command reply))
  (choice-evt (completion-evt reply)
              (wrap-evt (shutdown-evt controller)
                        (lambda (_) (Source-Failure 'shutdown)))))

(define (source-controller-call-evt controller proc)
  (submit controller (lambda (reply) (Owner-Call proc reply))))

;; Each refresh is a barrier: it cannot join a read that started before the
;; owner accepted the command. Pending callers share the next ordered read.
(define (source-controller-refresh-evt controller source-id)
  (replace-evt (submit controller (lambda (reply) (Refresh source-id reply)))
               (lambda (result)
                 (if (evt? result)
                     result
                     (wrap-evt always-evt (lambda (_) result))))))

(define (source-controller-view controller source-id)
  (source-store-view (Source-Controller-store controller) source-id))

(define (source-controller-snapshot controller source-id)
  (define store (Source-Controller-store controller))
  (define buffer (source-store-buffer store source-id))
  (if buffer
      (with-read-safedoc buffer
        (lambda (sd)
          (define view (SafeDoc-source-view sd))
          (Source-Snapshot source-id (Source-View-revision view) 'buffer
                           (string->immutable-string (doc-get-text (SafeDoc-doc sd))))))
      (source-store-disk-snapshot store source-id)))

(define (current-result controller source-id [error #f])
  (Source-Result (source-controller-view controller source-id)
                 (source-controller-snapshot controller source-id) error))

(define (complete-lane! lane result)
  (complete! (Read-Attempt-batch (Read-Lane-active lane)) result)
  (when (Read-Lane-next lane)
    (complete! (Read-Lane-next lane) result)))

;; Open, close, and invalidate abandon the in-flight read. Answer both the
;; active batch and the queued batch with the current view, which may be
;; unobserved. The abandoned read must not stay pending or be installed.
(define (invalidate-reads! controller source-id)
  (define lanes (Source-Controller-lanes controller))
  (define lane (hash-ref lanes source-id #f))
  (when lane
    (define active (Read-Lane-active lane))
    (custodian-shutdown-all (Read-Attempt-custodian active))
    (define result (current-result controller source-id))
    (complete-lane! lane result)
    (hash-remove! lanes source-id)))

(define (source-controller-open! controller source-id safe-doc)
  (with-write-safedoc safe-doc
    (lambda (sd)
      (set-SafeDoc-source-view! sd
                                (source-store-open! (Source-Controller-store controller)
                                                    source-id sd))))
  (invalidate-reads! controller source-id))

;; Called within the edit's existing write-lock section. This updates the
;; authoritative record and its SafeDoc projection before another reader enters.
(define (source-controller-edited! controller source-id sd)
  (set-SafeDoc-source-view! sd
                            (source-store-edit! (Source-Controller-store controller) source-id)))

;; Drop buffer authority immediately and leave disk unobserved. Close does
;; not start a read; the next refresh acquires saved text.
(define (source-controller-close! controller source-id safe-doc)
  (with-write-safedoc safe-doc
    (lambda (sd)
      (source-store-close! (Source-Controller-store controller) source-id)
      (set-SafeDoc-source-view! sd #f)))
  (invalidate-reads! controller source-id))

(define (source-controller-invalidate! controller source-id)
  (define store (Source-Controller-store controller))
  (define buffer (source-store-buffer store source-id))
  (if buffer
      (with-write-safedoc buffer
        (lambda (sd)
          (set-SafeDoc-source-view! sd (source-store-invalidate! store source-id))))
      (source-store-invalidate! store source-id))
  (invalidate-reads! controller source-id))

(define (start-read! controller source-id batch)
  (define deadline (+ (current-inexact-monotonic-milliseconds)
                      (* 1000 (Source-Controller-timeout controller))))
  (define custodian (make-custodian))
  (define attempt
    (Read-Attempt (source-controller-view controller source-id) batch custodian))
  (define result-ch (make-async-channel))
  ;; Reader threads inherit this port. An injected read must not write the
  ;; LSP stdout, even when the consumer's own redirection changes.
  (parameterize ([current-custodian custodian]
                 [current-output-port (open-output-nowhere)])
    (define worker
      (thread
        (lambda ()
          (define result
            (with-handlers ([(lambda (_) #t) Source-Failure])
              (define value ((Source-Controller-read controller) source-id))
              (if (string? value) (string->immutable-string value) value)))
          (async-channel-put result-ch result))))
    (thread
      (lambda ()
        (define outcome
          (sync result-ch
                (wrap-evt (thread-dead-evt worker)
                          (lambda (_)
                            (or (sync/timeout 0 result-ch) (Source-Failure 'reader-exited))))
                (wrap-evt (alarm-evt deadline #t)
                          (lambda (_) (Source-Failure 'read-timeout)))))
        (async-channel-put (Source-Controller-events controller)
                           (Read-Done source-id attempt outcome))
        ;; Clean up even while the consumer is busy; a timed-out read must not
        ;; keep running until its completion is accepted.
        (custodian-shutdown-all custodian))))
  attempt)

(define (refresh! controller source-id reply)
  (define view (source-controller-view controller source-id))
  (cond
    ;; An open buffer is the source text. Do not read disk over it.
    [(eq? 'buffer (Source-View-origin view))
     (complete! reply (current-result controller source-id))]
    [else
     (define lanes (Source-Controller-lanes controller))
     (define lane (hash-ref lanes source-id #f))
     (define batch
       (cond
         [(not lane)
          (define created (make-completion))
          (hash-set! lanes source-id
                     (Read-Lane (start-read! controller source-id created) #f))
          created]
         [else
          (unless (Read-Lane-next lane)
            (set-Read-Lane-next! lane (make-completion)))
          (Read-Lane-next lane)]))
     (complete! reply (completion-evt batch))]))

;; Apply a completion only while its attempt is still the lane's active read.
;; Invalidation already removed the lane and answered its waiters.
(define (finish-read! controller source-id attempt result)
  (define lanes (Source-Controller-lanes controller))
  (define lane (hash-ref lanes source-id #f))
  (when (and lane (eq? attempt (Read-Lane-active lane)))
    (custodian-shutdown-all (Read-Attempt-custodian attempt))
    (define-values (state text error)
      (match result
        [(? string? text) (values 'present text #f)]
        [(Disk-Missing) (values 'missing #f #f)]
        [(? Source-Failure? failure) (values 'unavailable #f failure)]
        [_ (values 'unavailable #f (Source-Failure result))]))
    (source-store-observe! (Source-Controller-store controller)
                           (Read-Attempt-ticket attempt) state text)
    (complete! (Read-Attempt-batch attempt) (current-result controller source-id error))
    (define next (Read-Lane-next lane))
    (cond
      [next
       (set-Read-Lane-next! lane #f)
       (set-Read-Lane-active! lane (start-read! controller source-id next))]
      [else
       (hash-remove! lanes source-id)])))

(define (source-controller-handle! controller event)
  (match event
    [(Owner-Call proc reply)
     (complete! reply
                (if (stopped? controller)
                    (Source-Failure 'shutdown)
                    (with-handlers ([(lambda (_) #t) Source-Failure])
                      (proc controller))))]
    [(Refresh source-id reply)
     (if (stopped? controller)
         (complete! reply (Source-Failure 'shutdown))
         (refresh! controller source-id reply))]
    [(Read-Done source-id attempt result)
     (finish-read! controller source-id attempt result)]))

(define (source-controller-shutdown! controller)
  (unless (stopped? controller)
    (semaphore-post (Source-Controller-shutdown controller))
    (for ([lane (in-hash-values (Source-Controller-lanes controller))])
      (define active (Read-Lane-active lane))
      (custodian-shutdown-all (Read-Attempt-custodian active))
      (complete-lane! lane (Source-Failure 'shutdown)))
    (hash-clear! (Source-Controller-lanes controller))))
