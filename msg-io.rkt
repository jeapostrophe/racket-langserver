#lang racket/base
(require json
         racket/contract/base
         racket/async-channel
         racket/match
         racket/string)

;; (->* () (input-port?) (or/c jsexpr? eof-object? 'parse-json-error))
(define (read-message [in (current-input-port)])
  (match (read-line in 'return-linefeed)
    ["" (with-handlers ([exn:fail:read? (λ (exn) 'parse-json-error)])
          (string->jsexpr (string-replace (jsexpr->string (read-json in)) "\\r\\n" "\\n")))]
    [(? eof-object?) eof]
    [_ (read-message in)]))

;; (->* (jsexpr?) (output-port?) void?)
(define (display-message msg [out (current-output-port)])
  (define json-bstr (jsexpr->bytes msg))
  (define content-length (bytes-length json-bstr))

  (fprintf out "Content-Length: ~a\r\n\r\n" content-length)
  (write-bytes json-bstr out))

;; (->* (jsexpr?) (output-port?) void?)
(define (display-message/flush msg [out (current-output-port)])
  (async-channel-put out-ch (list out msg)))

(define (read-loop out-ch)
  (match-define (list out msg) (async-channel-get out-ch))
  (display-message msg out)
  (flush-output out)
  (read-loop out-ch))

(define out-ch (make-async-channel))
(define out-t (thread (lambda () (read-loop out-ch))))

(provide
 (contract-out
  [read-message (->* ()
                     (input-port?)
                     (or/c jsexpr? eof-object? 'parse-json-error))]
  [display-message/flush (->* (jsexpr?)
                              (output-port?)
                              void?)]))
