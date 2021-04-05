#lang racket/base

(require racket/match
         framework
         racket/class
         "check-syntax.rkt"
         "interfaces.rkt")

(define (do-check-and-stuff! data)
  (match-define (list src text doc) data)
  (define new-text (new racket:text%))
  (send new-text insert text)
  (define new-trace (check-syntax src new-text (doc-trace doc)))
  (set-doc-trace! doc new-trace))

(define (waiter in-ch ready-ch out-ch)
  (define (run doc ready?)
    (define (send-doc! doc)
      (channel-put out-ch doc)
      (run #f #f))
    (sync (handle-evt in-ch
            (lambda (doc)
              (cond
                [ready? (send-doc! doc)]
                [else   (run doc #f)])))
          (handle-evt ready-ch
            (lambda (ignore)
              (cond
                [doc  (send-doc! doc)]
                [else (run #f #t)])))))
  (run #f #t))

(define (check in-ch ready-ch)
  (define (run)    
    (define doc (sync in-ch))
    (do-check-and-stuff! doc)
    (channel-put ready-ch #t)
    (run))
  (run))


(define in-ch       (make-channel))
(define ready-ch    (make-channel))
(define wait->check (make-channel))

(define waiter-th (thread (lambda () (waiter in-ch ready-ch wait->check))))
(define server-th (thread (lambda () (check wait->check ready-ch))))

(define (try-queue-check src doc)
    (when (and (thread-running? waiter-th) (thread-running? server-th))
        (channel-put in-ch (list src (send (doc-text doc) get-text) doc))))

(provide try-queue-check)