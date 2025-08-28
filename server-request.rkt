#lang racket/base
(provide response-handlers send-request)
(require "msg-io.rkt")

(define response-handlers (make-hash)) ; Each request sent by server will record its response handler here

;; Send a request from server to client and register handler of response.
(define (send-request id method params handler)
  (hash-set! response-handlers id handler)
  (display-message/flush
    (hasheq 'id id
            'method method
            'params params)))
