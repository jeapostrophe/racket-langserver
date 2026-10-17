#lang racket/base

;; One-shot broadcast results: every waiter observes the first completion.
(require racket/contract)

(provide Completion?
         make-completion
         completion-evt
         complete!
         ready-evt)

(struct/contract Completion
  ([result (and/c box? (not/c immutable?))]
   [ready semaphore?])
  #:transparent)

(define (make-completion)
  (Completion (box #f) (make-semaphore 0)))

(define (completion-evt completion)
  (wrap-evt (semaphore-peek-evt (Completion-ready completion))
            (lambda (_) (unbox (Completion-result completion)))))

(define (complete! completion result)
  ;; The first result wins; replacing it could disagree with earlier waiters.
  (unless (sync/timeout 0 (semaphore-peek-evt (Completion-ready completion)))
    (set-box! (Completion-result completion) result)
    (semaphore-post (Completion-ready completion))))

(define (ready-evt value)
  (wrap-evt always-evt (lambda (_) value)))
