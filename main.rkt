#lang racket/base
(require json
         racket/contract/base
         racket/match
         "error-codes.rkt"
         "log.rkt"
         "methods.rkt"
         "responses.rkt")

(define (read-message [in (current-input-port)])
  (match (read-line in 'return-linefeed)
    ["" (with-handlers ([exn:fail:read? (λ (_) 'parse-json-error)])
          (read-json in))]
    [(? eof-object?) 'parse-eof-error]
    [_ (read-message in)]))

(define (message->string msg)
  ;; TODO: are bytes implicitly UTF-8?
  (define content (jsexpr->bytes msg))
  (define content-length (add1 (bytes-length content))) ; +1 for null byte.
  (format "Content-Length: ~a\r\n\r\n~a" content-length content))

(define (main-loop)
  (define msg (read-message))
  (define response 
    (match msg
      ['parse-json-error
       (error-response (json-null)
                       PARSE-ERROR
                       "Invalid JSON was received by the server.")]
      ['parse-eof-error
       (log! "The server received unexpected EOF. Shutting down...")
       (exit 1)]
      [(? jsexpr?)
       (process-message msg)]))
  (unless (void? response)
    (display (message->string response)))
  (main-loop))

(provide
 (contract-out
  [read-message
   (input-port? . -> . (or/c jsexpr? 'parse-json-error 'parse-eof-error))]
  [message->string
   (jsexpr? . -> . string?)]
  [main-loop
   (-> void?)]))
