#lang racket/base

;; LSP JSON-RPC frames: Content-Length headers, a blank line, then a JSON body.

(require json
         racket/contract/base
         racket/match
         racket/string)

;; Skip each header line until the blank separator, then parse one JSON value.
;; Chosen over Content-Length: `read-json` finds the value boundary. Malformed
;; JSON becomes 'parse-json-error. Encoded CRLF in JSON strings is re-read as LF.
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

;; Flush so the peer sees a complete frame before this returns. Callers must
;; serialize writes to each port; frames must not interleave.
(define (display-message/flush msg [out (current-output-port)])
  (display-message msg out)
  (flush-output out))

(provide
  (contract-out
    [read-message (->* ()
                       (input-port?)
                       (or/c jsexpr? eof-object? 'parse-json-error))]
    [display-message/flush (->* (jsexpr?)
                                (output-port?)
                                void?)]))

