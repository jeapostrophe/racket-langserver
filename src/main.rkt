#lang racket/base
(require json
         racket/match
         "error-codes.rkt"
         "methods.rkt"
         "responses.rkt")

(define (read-message [in (current-input-port)])
  ;; TODO: Do we want to complain? What happens if the input port is cut off
  ;; without a shutdown message being sent first?
  (when (eof-object? (peek-byte in))
    (exit 1))
  ;; TODO: Should we be stricter about making sure headers are well-formed?
  (let loop ()
    (match (read-line in 'return-linefeed)
      ["" (with-handlers ([exn:fail:read? (λ (exn) 'parse-json-error)])
            (read-json in))]
      [(? eof-object?) 'parse-eof-error]
      [_ (loop)])))

(define (read-message_ [in (current-input-port)])
  (match (read-line in 'return-linefeed)
    ["" (with-handlers ([exn:fail:read? (λ (exn) 'parse-json-error)])
          (read-json in))]
    [(? eof-object?) (raise-eof-error)]
    [_ (read-message in)]))

(define (message->string msg)
  ;; TODO: are bytes implicitly UTF-8?
  (define content (jsexpr->bytes msg))
  (define content-length (add1 (bytes-length content))) ; +1 for null byte.
  (format "Content-Length: ~a\r\n\r\n~a" content-length content))

(define (raise-eof-error)
  (raise (exn:fail:read:eof
          "The server expected JSON, received EOF."
          (current-continuation-marks)
          null)))

;(module+ main_
;  (let loop()
;    (with-handlers 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-json-error)
  (error-response (json-null)
                  PARSE-ERROR
                  "Invalid JSON was received by the server."))

(define (parse-eof-error)
  (error-response (json-null)
                  PARSE-ERROR
                  "The server expected JSON; received EOF."))

#;
(module+ main
  (let loop ()
    (define msg (read-message))
    (define response
      (match msg
        ['parse-json-error
         (parse-json-error)]
        ['parse-eof-error
         (parse-eof-error)]
        [(? jsexpr?)
         (process-message msg)]))
    (unless (void? response)
      (display (message->string response)))
    (loop)))
    