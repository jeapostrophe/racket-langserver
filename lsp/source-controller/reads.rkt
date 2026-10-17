#lang racket/base

;; Ordered disk reads. Each source has at most one active read; refreshes that
;; arrive while it runs share the next one.
(require racket/contract
         racket/file
         racket/match
         "../../common/source.rkt"
         "../../workspace/source-store.rkt"
         "completion.rkt"
         "jobs.rkt"
         "state.rkt"
         "worker.rkt")

(provide (struct-out Read-Done)
         read-source-text
         refresh!
         finish-read!
         abandon-lane!
         invalidate-reads!)

(struct/contract Read-Attempt
  ([ticket Source-View?]
   [batch Completion?]
   [custodian custodian?])
  #:transparent)

;; `next` is the batch shared by refreshes that arrived after `active` started.
(struct/contract Read-Lane
  ([active Read-Attempt?]
   [next (or/c #f Completion?)])
  #:mutable)

(struct/contract Read-Done
  ([source Source?]
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

(define (refresh! controller source reply)
  (define origin (Source-View-origin (source-controller-view controller source)))
  (complete! reply
             (case origin
               [(buffer) (current-result controller source)]
               [(closed) (current-result controller source (Source-Failure 'closed))]
               [(disk) (completion-evt (next-read-batch! controller source))])))

(define (next-read-batch! controller source)
  (define lanes (controller-lanes controller))
  (match (hash-ref lanes (source-id source) #f)
    [#f
     (define batch (make-completion))
     (hash-set! lanes (source-id source) (Read-Lane (start-read! controller source batch) #f))
     batch]
    [lane
     (unless (Read-Lane-next lane)
       (set-Read-Lane-next! lane (make-completion)))
     (Read-Lane-next lane)]))

(define (start-read! controller source batch)
  (define custodian (make-custodian))
  (define attempt
    (Read-Attempt (source-controller-view controller source) batch custodian))
  (define reads (Source-Controller-reads controller))
  (start-worker! (Source-Controller-events controller) custodian (Disk-Reads-timeout reads)
                 (lambda ()
                   (define value ((Disk-Reads-read reads) (source-file-path source)))
                   (if (string? value) (string->immutable-string value) value))
                 #:fail Source-Failure
                 #:exited 'reader-exited
                 #:timed-out 'read-timeout
                 #:done (lambda (result) (Read-Done source attempt result)))
  attempt)

;; Apply a completion only while its attempt is still the lane's active read.
;; Invalidation already removed the lane and answered its waiters.
(define (finish-read! controller source attempt result)
  (define lanes (controller-lanes controller))
  (define lane (hash-ref lanes (source-id source) #f))
  (when (and lane (eq? attempt (Read-Lane-active lane)))
    (define-values (state text error)
      (match result
        [(? string? text) (values 'present text #f)]
        [(Disk-Missing) (values 'missing #f #f)]
        [(? Source-Failure? failure) (values 'unavailable #f failure)]
        [_ (values 'unavailable #f (Source-Failure result))]))
    (define ticket (Read-Attempt-ticket attempt))
    (source-store-observe! (Source-Controller-store controller) ticket state text)
    ;; A new disk view stops jobs captured under the view it replaced.
    ;; An unchanged reread keeps the ticket and its jobs.
    (unless (eq? ticket (source-controller-view controller source))
      (invalidate-jobs! controller source))
    (complete! (Read-Attempt-batch attempt) (current-result controller source error))
    (match (Read-Lane-next lane)
      [#f (hash-remove! lanes (source-id source))]
      [next
       (set-Read-Lane-next! lane #f)
       (set-Read-Lane-active! lane (start-read! controller source next))])))

(define (abandon-lane! lane result)
  (define active (Read-Lane-active lane))
  (custodian-shutdown-all (Read-Attempt-custodian active))
  (complete! (Read-Attempt-batch active) result)
  (when (Read-Lane-next lane)
    (complete! (Read-Lane-next lane) result)))

;; Open, close, and invalidate abandon the in-flight read. Answer both the
;; active batch and the queued batch with the current view, which may be
;; unobserved. The abandoned read must not stay pending or be installed.
(define (invalidate-reads! controller source)
  (define lanes (controller-lanes controller))
  (define lane (hash-ref lanes (source-id source) #f))
  (when lane
    (abandon-lane! lane (current-result controller source))
    (hash-remove! lanes (source-id source))))
