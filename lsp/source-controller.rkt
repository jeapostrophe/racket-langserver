#lang racket/base

;; Per-session owner of source records, ordered disk reads, shared analysis
;; jobs, bounded execution and result acceptance. The session consumer applies
;; every mutation and table read. Other threads submit commands and wait on
;; reply or change events; disk reads and analysis run on private workers.
;; Descriptors retain source addresses; tables key on their stable IDs.
(require racket/async-channel
         racket/contract
         racket/match
         "../common/source.rkt"
         "../doclib/external/resyntax.rkt"
         "../workspace/source-store.rkt"
         "analysis.rkt"
         "resyntax-place.rkt"
         "source-controller/completion.rkt"
         "source-controller/jobs.rkt"
         "source-controller/lifecycle.rkt"
         "source-controller/reads.rkt"
         "source-controller/state.rkt")

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
         source-controller-analyze!
         source-controller-query-analysis!
         source-controller-acquire-analysis!
         source-controller-set-notifier!
         source-controller-shutdown-evt
         source-controller-shutdown!
         (struct-out Source-Result)
         (struct-out Source-Failure)
         (struct-out Disk-Missing))

(define (make-source-controller #:read [read read-source-text]
                                #:read-timeout [read-timeout 90]
                                #:analyze [analyze analyze-source]
                                #:analysis-slots [slots 2]
                                #:analysis-timeout [analysis-timeout 90]
                                #:retained-limit [retained-limit 32]
                                #:resyntax [resyntax (and (resyntax-available?)
                                                          run-resyntax/in-place)])
  (Source-Controller (make-source-store) (make-async-channel) (make-semaphore 0)
                     (lambda (_method _params) (void))
                     (Disk-Reads read read-timeout (make-hash))
                     (Runner analyze resyntax analysis-timeout slots (make-hash) '())
                     (Retained-Cache retained-limit '())))

(define current-source-controller (make-parameter (make-source-controller)))

(define (source-controller-set-notifier! controller notify)
  (set-Source-Controller-notify! controller notify))

(define (source-controller-events controller)
  (Source-Controller-events controller))

(struct/contract Owner-Call
  ([proc (-> Source-Controller? any/c)]
   [reply Completion?])
  #:transparent)

(struct/contract Refresh
  ([source Source?]
   [reply Completion?])
  #:transparent)

(define (submit controller make-command)
  (define reply (make-completion))
  (async-channel-put (Source-Controller-events controller) (make-command reply))
  (choice-evt (completion-evt reply)
              (wrap-evt (source-controller-shutdown-evt controller)
                        (lambda (_) (Source-Failure 'shutdown)))))

(define (source-controller-call-evt controller proc)
  (submit controller (lambda (reply) (Owner-Call proc reply))))

;; Each refresh is a barrier: it cannot join a read that started before the
;; owner accepted the command. Pending callers share the next ordered read.
(define (source-controller-refresh-evt controller source)
  (replace-evt (submit controller (lambda (reply) (Refresh source reply)))
               (lambda (result) (if (evt? result) result (ready-evt result)))))

(define (source-controller-handle! controller event)
  (match event
    [(Owner-Call proc reply)
     (complete! reply
                (if (stopped? controller)
                    (Source-Failure 'shutdown)
                    (with-handlers ([(lambda (_) #t) Source-Failure])
                      (proc controller))))]
    [(Refresh source reply)
     (if (stopped? controller)
         (complete! reply (Source-Failure 'shutdown))
         (refresh! controller source reply))]
    [(Read-Done source attempt result)
     (finish-read! controller source attempt result)]
    [(Job-Done job result)
     (finish-job! controller job result)])
  (pump-jobs! controller))

(define (source-controller-shutdown! controller)
  (unless (stopped? controller)
    (semaphore-post (Source-Controller-shutdown controller))
    (define jobs (controller-jobs controller))
    (for ([job (in-list (hash-values jobs))])
      (settle-job! controller job (Analysis-Outcome 'stopped 'shutdown)))
    (hash-clear! jobs)
    (set-Runner-queue! (Source-Controller-runner controller) '())
    (define lanes (controller-lanes controller))
    (for ([lane (in-hash-values lanes)])
      (abandon-lane! lane (Source-Failure 'shutdown)))
    (hash-clear! lanes)
    (set-Retained-Cache-entries! (Source-Controller-retained controller) '())))

(module+ test-support
  ;; Replay a captured runner message to exercise terminal-attempt rejection.
  (provide Job-Done Job-Done-job))
