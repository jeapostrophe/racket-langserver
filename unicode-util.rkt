#lang racket/base
(require (only-in racket/match
                  match-define-values)
         racket/contract/base)

(define 8->16 (bytes-open-converter "platform-UTF-8" "platform-UTF-16"))
(define 16->8 (bytes-open-converter "platform-UTF-16" "platform-UTF-8"))

(define (conversion-error proc-name output result)
  (error proc-name "conversion failed with output ~v and result ~a" output result))

(define (utf-8->utf-16 str)
  (match-define-values (bytes-16 _ result)
                       (bytes-convert 8->16 (string->bytes/utf-8 str)))
  (unless (eq? 'complete result)
    (conversion-error 'utf-8->utf-16 bytes-16 result))
  bytes-16)

(define (utf-16->utf-8 bytes-16)
  (match-define-values (bytes-8 _ result)
                       (bytes-convert 16->8 bytes-16))
  (unless (eq? 'complete result)
    (conversion-error 'utf-16->utf-8 bytes-8 result))
  (bytes->string/utf-8 bytes-8))

(provide
 (contract-out
  [utf-8->utf-16 (string? . -> . bytes?)]
  [utf-16->utf-8 (bytes? . -> . string?)]))