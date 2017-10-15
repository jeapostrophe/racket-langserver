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
  (log-info "msg = ~v" msg)
  (with-handlers ([exn:fail? (report-error* msg)])
    (match msg
      ['parse-json-error
       (display-message/flush
        (error-response (json-null) PARSE-ERROR
                        "Invalid JSON was received by the server."))]
      ['parse-eof-error
       (log-fatal "The server received unexpected EOF. Shutting down...")
       (exit 1)]
      [_
       (process-message msg)]))
  (log-info "====================")
  (main-loop))

(module+ main
  (with-logging-to-port (current-error-port) main-loop 'debug))
