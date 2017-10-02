#lang racket/base
(require json
         racket/function
         racket/logging
         racket/match
         "error-codes.rkt"
         "methods.rkt"
         "msg-io.rkt"
         "responses.rkt")

(define (report-error msg exn)
  (log-warning "caught exn: ~a\n\tmsg = ~v" (exn-message exn) msg))

(define report-error* (curry report-error))

(define (main-loop)
  (define msg (read-message))
  (log-info "====================")
  (log-info "msg = ~v" msg)
  (define response
    (with-handlers ([exn:fail? (report-error* msg)])
      (match msg
        ['parse-json-error
         (error-response (json-null) PARSE-ERROR
                         "Invalid JSON was received by the server.")]
        ['parse-eof-error
         (log-fatal "The server received unexpected EOF. Shutting down...")
         (exit 1)]
        [_
         (process-message msg)])))
  (log-info "resp = ~v" response)
  (unless (void? response)
    (display-message/flush response))
  (main-loop))

(module+ main
  (with-logging-to-port (current-error-port) main-loop 'debug))
