#lang racket/base
(require json
         mzlib/cml
         racket/exn
         racket/function
         racket/list
         racket/match
         "append-message.rkt"
         "error-codes.rkt"
         "methods.rkt"
         "msg-io.rkt"
         "responses.rkt")

;; https://www.cs.utah.edu/plt/publications/pldi04-ff.pdf

(struct Q (in-ch out-ch mgr-t))

(define (queue)
  (define in-ch (channel))
  (define out-ch (channel))
  (define (serve msgs)
    (cond [(empty? msgs)
           (serve (list (sync (channel-recv-evt in-ch))))]
          [else
           (sync (choice-evt
                  (wrap-evt
                   (channel-recv-evt in-ch)
                   (λ (m)
                     (serve (append-message msgs m))))
                  (wrap-evt
                   (channel-send-evt out-ch (first msgs))
                   (thunk*
                    (serve (rest msgs))))))]))
  (define mgr-t (spawn (λ () (serve empty))))
  (Q in-ch out-ch mgr-t))

(define (queue-send-evt q v)
  (guard-evt
   (λ ()
     (thread-resume (Q-mgr-t q) (current-thread))
     (channel-send-evt (Q-in-ch q) v))))

(define (queue-recv-evt q)
  (guard-evt
   (λ ()
     (thread-resume (Q-mgr-t q) (current-thread))
     (channel-recv-evt (Q-out-ch q)))))

(define (report-error exn)
  (eprintf "\nCaught exn:\n~a\n" (exn->string exn)))

(define (main-loop)
  (define q (queue))
  (define (consume)
    (define msg (sync (queue-recv-evt q)))
    (match msg
      ['parse-json-error
       (define err "Invalid JSON was received by the server.")
       (display-message/flush (error-response (json-null) PARSE-ERROR err))]
      [_
       (with-handlers ([exn:fail? report-error])
         (process-message msg))])
    (consume))
  (spawn consume)
  (for ([msg (in-port read-message)])
    (sync (queue-send-evt q msg)))
  (eprintf "Unexpected EOF\n")
  (exit 1))

(module+ main
  (main-loop))
