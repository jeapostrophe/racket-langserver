#lang racket/base

;; A single owner resolves the event; any number of waiters receive its first value.
(provide Completion-Event? make-completion-event resolve-completion! ready-evt evt-ready?)

(struct Completion-Event (result ready)
  #:property prop:evt
  (lambda (completion)
    (wrap-evt (semaphore-peek-evt (Completion-Event-ready completion))
              (lambda (_) (unbox (Completion-Event-result completion))))))

(define (make-completion-event)
  (Completion-Event (box #f) (make-semaphore 0)))

(define (resolve-completion! completion result)
  (unless (evt-ready? (semaphore-peek-evt (Completion-Event-ready completion)))
    (set-box! (Completion-Event-result completion) result)
    (semaphore-post (Completion-Event-ready completion))))

(define (ready-evt value)
  (wrap-evt always-evt (lambda (_) value)))

;; Readiness does not depend on whether the event produces #f.
(define (evt-ready? event)
  (sync/timeout 0 (wrap-evt event (lambda (_) #t))))
