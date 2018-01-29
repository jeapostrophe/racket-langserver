#lang racket/base
(require json
         racket/exn
         racket/match
         "error-codes.rkt"
         "methods.rkt"
         "msg-io.rkt"
         "responses.rkt")

(define (report-error exn)
  (eprintf "\nCaught exn:\n~a\n" (exn->string exn)))

(define (main-loop)
  (define msg (read-message))
  (when (verbose-io?)
    (eprintf "msg = ~v\n" msg))
  (with-handlers ([exn:fail? report-error])
    (match msg
      ['parse-json-error
       (define err "Invalid JSON was received by the server.")
       (display-message/flush (error-response (json-null) PARSE-ERROR err))]
      [(? eof-object?)
       (eprintf "The server received unexpected EOF. Shutting down...\n")
       (exit 1)]
      [_
       (process-message msg)]))
  (when (verbose-io?)
    (eprintf "====================\n"))
  (main-loop))

(module+ main
  (main-loop))