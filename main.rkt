#lang racket/base

;; Process layout. The main thread reads LSP frames from stdin onto a client
;; channel. `start-consumer` is the only thread that dispatches on `server%`.
;; A dedicated thread writes outbound JSON with `display-message/flush`.

(require json
         racket/exn
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

(define (report-error exn)
  (log-racket-langserver-error "Caught exn:\n~a" (exn->string exn)))

;; (-> (is-a?/c server%) evt? thread?)
;; Exported so `tests/client.rkt` and `tests/lsp/source-controller-test.rkt`
;; run this dispatcher instead of a forked copy.
(define (start-consumer server client-evt)
  (define controller (send server get-source-controller))
  (define internal-evt
    (wrap-evt (source-controller-events controller)
              (λ (event) (cons 'internal event))))
  (define message-evt
    (wrap-evt client-evt (λ (msg) (cons 'client msg))))
  (thread
    (λ ()
      ;; Analysis and language hooks may write to `current-output-port`. Stdout
      ;; must stay the LSP writer (`display-message/flush` on another thread).
      (parameterize ([current-output-port (open-output-nowhere)])
        ;; Chosen over a fixed client-first or internal-first order, and over
        ;; `sync` alone: alternate zero-timeout polls so a burst on one queue
        ;; cannot starve the other across consecutive dispatches.
        (let loop ([prefer-client? #t])
          (define event
            (or (sync/timeout 0 (if prefer-client? message-evt internal-evt))
                (sync message-evt internal-evt)))
          (with-handlers ([exn:fail? report-error])
            (match event
              [(cons 'internal source-event)
               (send server process-source-event source-event)]
              [(cons 'client 'parse-json-error)
               (send server send-response
                     (error-response (json-null) ErrorCode-ParseError
                                     "Invalid JSON was received by the server."))]
              [(cons 'client msg)
               (maybe-debug-log msg)
               (send server process-message msg)]))
          (loop (eq? (car event) 'internal)))))))

;; Responses, requests, and notifications share one channel so a single writer
;; owns stdout. stdin EOF exits 1; a clean shutdown is the LSP `exit`
;; notification.
(define (main-loop)
  (define resp-ch (make-async-channel))
  (define server
    (new server%
      [response-channel resp-ch]
      [request-channel resp-ch]
      [notification-channel resp-ch]))
  (define client-ch (make-async-channel))
  (define out (current-output-port))

  (start-consumer server client-ch)
  (thread
    (λ ()
      (let loop ()
        (display-message/flush (async-channel-get resp-ch) out)
        (loop))))

  (for ([msg (in-port read-message)])
    (async-channel-put client-ch msg))
  (log-racket-langserver-error "Unexpected EOF")
  (exit 1))

(module+ main
  (main-loop))
