#lang racket/base
(require json
         racket/contract/base
         racket/match
         (only-in racket/port open-output-nowhere))

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

(define (display-message/flush msg [out (current-output-port)])
  (display-message msg out)
  (flush-output out))

(provide
 (contract-out
  [read-message (->* ()
                     (input-port?)
                     (or/c jsexpr? 'parse-json-error 'parse-eof-error))]
  [display-message (->* (jsexpr?)
                        (output-port?)
                        void?)]
  [display-message/flush (->* (jsexpr?)
                              (output-port?)
                              void?)]))