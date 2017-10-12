#lang racket/base
(require (for-syntax racket/base)
         json
         racket/contract/base
         racket/match
         (only-in racket/port open-output-nowhere)
         syntax/parse/define)

(define log-messages? (make-parameter #t))

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

(define-syntax (display-message* stx)
  (syntax-parse stx
    [(_ msg (~optional out #:defaults ([out #'(current-output-port)])))
     (syntax/loc stx
       (begin
         (when (log-messages?)
           (log-info "resp = ~v" msg))
         (display-message msg out)))]))

(define-syntax (display-message/flush* stx)
  (syntax-parse stx
    [(_ msg (~optional out #:defaults ([out #'(current-output-port)])))
     (syntax/loc stx
       (begin
         (when (log-messages?)
           (log-info "resp = ~v" msg))
         (display-message/flush msg out)))]))

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