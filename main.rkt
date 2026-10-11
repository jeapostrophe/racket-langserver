#lang racket/base

;; Stdio language server. One consumer thread dispatches client messages and
;; source-controller events for the session.
(require json
         mzlib/cml
         racket/exn
         racket/function
         racket/list
         racket/match
         racket/port
         racket/class
         racket/async-channel
         "common/interfaces.rkt"
         "common/debug.rkt"
         "lsp/methods.rkt"
         "lsp/source-controller.rkt"
         "lsp/msg-io.rkt"
         "lsp/responses.rkt")

(provide start-consumer)

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
  (log-racket-langserver-error "Caught exn:\n~a" (exn->string exn)))

;; The same pump is used by stdio and integration tests. Alternating preference
;; when both queues are ready prevents either producer from starving the other;
;; receiving only the head of the client queue preserves protocol message order.
(define (start-consumer server client-evt)
  (define controller (send server get-source-controller))
  (define internal-evt
    (wrap-evt (source-controller-events controller) (lambda (event) (cons 'internal event))))
  (define message-evt (wrap-evt client-evt (lambda (msg) (cons 'client msg))))
  (thread
    (lambda ()
      ;; Language readers, lexers, and formatting hooks can print. Protocol
      ;; messages are queued; write-resp retains the real port and displays them.
      (parameterize ([current-output-port (open-output-nowhere)])
        (let loop ([prefer-client? #t])
          (define event
            (or (sync/timeout 0 (if prefer-client? message-evt internal-evt))
                (sync message-evt internal-evt)))
          (with-handlers ([exn:fail? report-error])
            (match event
              [(cons 'internal source-event) (send server process-source-event source-event)]
              [(cons 'client 'parse-json-error)
               (send server send-response
                     (error-response (json-null) ErrorCode-ParseError
                                     "Invalid JSON was received by the server."))]
              [(cons 'client msg)
               (maybe-debug-log msg)
               (send server process-message msg)]))
          (loop (eq? 'internal (car event))))))))

;; Threads started for stdio:
;; * current-thread reads messages from the input port into queue `q`.
;; * mgr-t, inside queue, forwards those messages to the consumer.
;; * start-consumer dispatches client messages and internal source events.
;; * write-resp displays queued responses on the output port.
(define (main-loop)
  (define resp-ch (make-async-channel))
  (define server (new server%
                   [response-channel resp-ch]
                   [request-channel resp-ch]
                   [notification-channel resp-ch]))

  (define q (queue))
  (define (write-resp)
    (display-message/flush (async-channel-get resp-ch))
    (write-resp))

  (start-consumer server (queue-recv-evt q))
  (spawn write-resp)
  (for ([msg (in-port read-message)])
    (sync (queue-send-evt q msg)))
  (log-racket-langserver-error "Unexpected EOF")
  (exit 1))

(module+ main
  (main-loop))
