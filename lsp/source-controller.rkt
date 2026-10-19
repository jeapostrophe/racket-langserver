#lang racket/base

;; Public command interface for the source controller. Request threads submit
;; named commands and wait; the consumer dispatches them to the private source
;; owner. No source state or subsystem handle is exposed here.
(require racket/async-channel
         racket/contract
         racket/match
         "../common/source.rkt"
         "../workspace/source-store.rkt"
         "source-controller/completion-event.rkt"
         "source-controller/source-owner.rkt")

(provide make-source-controller
         Source-Controller?
         current-source-controller
         source-controller-events
         source-controller-observe-evt
         source-controller-analysis-evt
         source-controller-current-view-evt
         source-controller-shutdown-evt
         (struct-out Analysis-Acquisition)
         (struct-out Source-Result)
         (struct-out Source-Failure)
         (struct-out Disk-Missing))

;; These operations run only on the session consumer. Request threads use the
;; named commands above.
(module+ owner
  (provide source-controller-handle!
           source-controller-open!
           source-controller-edit!
           source-controller-close!
           source-controller-invalidate!
           source-controller-set-notifier!
           source-controller-shutdown!
           source-controller-view
           source-controller-snapshot
           source-controller-ensure-analysis!
           source-controller-refresh-analysis!))

;; Commands on `events`, beside worker completions. Request threads never
;; construct these records; the functions below submit them.
;;
;; reply: The submitter's one-shot wait, completed by the consumer in
;;   `answer!`. Shared by every command below.

;; Current source knowledge. The consumer may complete `reply` with a
;; `Source-Result`, or with a further read completion that the observe
;; wrapper waits through.
(struct/contract Observe
  ([source Source?]
   [reply Completion-Event?])
  #:transparent)

;; Start or join analysis for `snapshot`. Completes `reply` with
;; `Analysis-Acquisition`.
(struct/contract Acquire-Analysis
  ([snapshot Source-Snapshot?]
   [reply Completion-Event?])
  #:transparent)

;; Whether `view` is still this source's current view. Completes `reply` with
;; a boolean. Does not read disk.
(struct/contract Validate-View
  ([view Source-View?]
   [reply Completion-Event?])
  #:transparent)

;; Puts the command on the consumer queue immediately. The returned event is
;; the reply, or `Source-Failure` `'shutdown` if the controller stops first.
(define (submit controller make-command)
  (define reply (make-completion-event))
  (async-channel-put (source-controller-events controller) (make-command reply))
  (choice-evt reply
              (wrap-evt (source-controller-shutdown-evt controller)
                        (lambda (_) (Source-Failure 'shutdown)))))

;; Current source knowledge. An open buffer replies with a `Source-Result`. A
;; virtual source with no open buffer replies with a `Source-Result` whose
;; `error` is `closed`. A disk file replies with the completion for its read:
;; this command cannot join a read that started before the consumer accepted
;; it, so overlapping observers share the next read. `replace-evt` waits
;; through that inner event.
(define (source-controller-observe-evt controller source)
  (replace-evt (submit controller (lambda (reply) (Observe source reply)))
               (lambda (result) (if (Completion-Event? result) result (ready-evt result)))))

;; Start or join analysis for `snapshot`. The reply is `Analysis-Acquisition`:
;; optional reusable facts plus the fresh attempt's completion event.
(define (source-controller-analysis-evt controller snapshot)
  (submit controller (lambda (reply) (Acquire-Analysis snapshot reply))))

;; Whether `view` is still this source's current view. Does not read disk.
(define (source-controller-current-view-evt controller view)
  (submit controller (lambda (reply) (Validate-View view reply))))

;; Complete `reply` with `proc`'s value. Shutdown and uncaught exceptions both
;; become `Source-Failure`, so request threads never see a raised owner error.
(define (answer! controller reply proc)
  (resolve-completion! reply
                       (if (source-controller-stopped? controller)
                           (Source-Failure 'shutdown)
                           (with-handlers ([(lambda (_) #t) Source-Failure])
                             (proc)))))

;; One dispatch finishes its source transition before queued analysis starts.
;; Worker completions use the same owner and acceptance ordering as commands.
(define (source-controller-handle! controller event)
  (match event
    [(Observe source reply)
     (answer! controller reply (lambda () (source-controller-observe! controller source)))]
    [(Acquire-Analysis snapshot reply)
     (answer! controller reply (lambda () (source-controller-acquire-analysis! controller snapshot)))]
    [(Validate-View view reply)
     (answer! controller reply (lambda () (source-controller-current-view? controller view)))]
    [_ (source-controller-handle-completion! controller event)])
  (source-controller-pump! controller))
