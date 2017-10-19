#lang racket/base
(require json
         racket/exn
         racket/logging
         racket/match
         "error-codes.rkt"
         "methods.rkt"
         "msg-io.rkt"
         "responses.rkt")

(define (report-error exn)
  (log-warning "\nCaught exn:\n~a" (exn->string exn)))

(define (main-loop)
  (define msg (read-message))
  (log-info "msg = ~v" msg)
  (with-handlers ([exn:fail? report-error])
    (match msg
      ['parse-json-error
       (define err "Invalid JSON was received by the server.")
       (display-message/flush (error-response (json-null) PARSE-ERROR err))]
      ['parse-eof-error
       (log-fatal "The server received unexpected EOF. Shutting down...")
       (exit 1)]
      [_
       (process-message msg)]))
  (log-info "====================")
  (main-loop))

(module+ main
  (with-logging-to-port (current-error-port) main-loop 'debug))
