#lang racket/base
(require json
         mzlib/cml
         racket/exn
         racket/function
         racket/list
         racket/match
         racket/class
         racket/async-channel
         "debug.rkt"
         "error-codes.rkt"
         "methods.rkt"
         "msg-io.rkt"
         "server.rkt"
         "responses.rkt")

;; https://www.cs.utah.edu/plt/publications/pldi04-ff.pdf

;; (struct/c Q channel? channel? thread?)
(struct Q (in-ch out-ch mgr-t))

(define (queue)
  (define in-ch (channel))
  (define out-ch (channel))
  (define (serve ready-req-evts)
    (cond [(empty? ready-req-evts)
           (serve (list (sync (channel-recv-evt in-ch))))]
          [else
           (sync (choice-evt
                  (wrap-evt (channel-recv-evt in-ch)
                            (λ (m)
                              (serve (append ready-req-evts (list m)))))
                  (wrap-evt (channel-send-evt out-ch (first ready-req-evts))
                            (thunk*
                             (serve (rest ready-req-evts))))))]))
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

;; We spawn some threads:
;; * current-thread - read request message from a specified input-port
;;                    or current-input-port and put it into queue `q`.
;; * mgr-t          - defined in queue, forward message between current and consume threads.
;; * consume        - read message from queue `q` and really process it.
;; * out-t          - defined in `msg-io.rkt`, put the response message
;;                    to a specified output-port or current-output-port.
(define (main-loop)
  (define resp-ch (make-async-channel))
  (set-current-server! (new server%
                            [response-channel resp-ch]
                            [request-channel resp-ch]
                            [notification-channel resp-ch]))

  (define q (queue))
  (define (consume)
    (define msg (sync (queue-recv-evt q)))
    (match msg
      ['parse-json-error
       (define err "Invalid JSON was received by the server.")
       (display-message/flush (error-response (json-null) PARSE-ERROR err))]
      [_
       (maybe-debug-log msg)
       (with-handlers ([exn:fail? report-error])
         (send current-server process-message msg))])
    (consume))
  (define (write-resp)
    (display-message/flush (async-channel-get resp-ch))
    (write-resp))

  (spawn consume)
  (spawn write-resp)
  (for ([msg (in-port read-message)])
    (sync (queue-send-evt q msg)))
  (eprintf "Unexpected EOF\n")
  (exit 1))

(module+ main
  (main-loop))
