#lang racket/base
(require json
         racket/contract/base
         racket/function
         racket/logging
         racket/match
         racket/port
         "error-codes.rkt"
         "methods.rkt"
         "responses.rkt")

(define (read-message [in (current-input-port)])
  (match (read-line in 'return-linefeed)
    ["" (with-handlers ([exn:fail:read? (λ (exn) 'parse-json-error)])
          (read-json in))]
    [(? eof-object?) 'parse-eof-error]
    [_ (read-message in)]))

(define (display-message msg [out (current-output-port)])
  (define null-port (open-output-nowhere))
  (write-json msg null-port)
  (define content-length (file-position null-port))
  (fprintf out "Content-Length: ~a\r\n\r\n" content-length)
  (write-json msg out))

(define (report-error msg exn)
  (log-warning "caught exn: ~a\n\tmsg = ~v" (exn-message exn) msg))

(define report-error* (curry report-error))

(define (main-loop)
  (define msg (read-message))
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
    (display-message response))
  (main-loop))

(module+ main
  (with-logging-to-port (current-error-port) main-loop 'debug))
 
(provide
 (contract-out
  [read-message
   (input-port? . -> . (or/c jsexpr? 'parse-json-error 'parse-eof-error))]
  [display-message
   (jsexpr? output-port? . -> . void?)]
  [main-loop
   (-> void?)]))
