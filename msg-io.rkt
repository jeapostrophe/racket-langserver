#lang racket/base
(require json
         racket/contract/base
         racket/match
         racket/string
         (only-in racket/port open-output-nowhere))

(define verbose-io? (make-parameter #f))

(define (read-message [in (current-input-port)])
  (match (read-line in 'return-linefeed)
    ["" (with-handlers ([exn:fail:read? (λ (exn) 'parse-json-error)])
          (string->jsexpr (string-replace (jsexpr->string (read-json in)) "\\r\\n" "\\n")))]
    [(? eof-object?) eof]
    [_ (read-message in)]))

(define (display-message msg [out (current-output-port)])
  (when (verbose-io?)
    (eprintf "\nresp = ~v\n" msg))
  (define null-port (open-output-nowhere))
  (write-json msg null-port)
  (define content-length (file-position null-port))
  (fprintf out "Content-Length: ~a\r\n\r\n" content-length)
  (write-json msg out))

(define (display-message/flush msg [out (current-output-port)])
  (channel-put in-ch msg))

(define (read-loop in-ch [out (current-output-port)])
  (define msg (channel-get in-ch))
  (display-message msg out)
  (flush-output out)
  (read-loop in-ch out))

(define in-ch (make-channel))
(define message-th (thread (lambda () (read-loop in-ch))))

(provide
 verbose-io?
 (contract-out
  [read-message (->* ()
                     (input-port?)
                     (or/c jsexpr? eof-object? 'parse-json-error))]
  [display-message (->* (jsexpr?)
                        (output-port?)
                        void?)]
  [display-message/flush (->* (jsexpr?)
                              (output-port?)
                              void?)]))