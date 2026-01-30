#lang racket

;; This module provides a simple incomplete lsp client for test.

(provide with-racket-lsp
         client-send
         client-wait-response
         client-wait-notification
         make-request
         make-expected-response
         make-notification)

(require racket/os
         racket/async-channel
         racket/match
         json
         "../server.rkt"
         "../methods.rkt")

(define id 0)
(define (genid)
  (set! id (add1 id))
  id)

(define request-channel (make-parameter #f))
(define response-channel (make-parameter #f))
(define notification-channel (make-parameter #f))

(define/contract (with-racket-lsp proc)
  (-> (-> any/c any/c) void?)
  (parameterize ([response-channel (make-async-channel)]
                 [request-channel (make-async-channel)]
                 [notification-channel (make-async-channel)])
    (set-current-server! (new server%
                              [response-channel (response-channel)]
                              [request-channel (request-channel)]
                              [notification-channel (notification-channel)]))
    (define lsp current-server)

    (define (handle-request)
      ;; Server request - handle immediately and send response
      (match (async-channel-get (request-channel))
        [(hash-table ['id (? (or/c number? string?) id)]
                     ['method (? string? method)])
         (define result
           (match method
             ["workspace/configuration" (list (json-null))]
             [_ (json-null)]))

         (define response
           (hasheq 'jsonrpc "2.0"
                   'id id
                   'result result))

         (client-send lsp response)]
        [_ (error "Not a request from server")])
      (handle-request))
    (thread handle-request)

    (define init-req
      (make-request lsp "initialize"
                    (hasheq 'processId (getpid)
                            'capabilities (hasheq))))
    (client-send lsp init-req)
    (client-wait-response init-req)

    (proc lsp)

    (define shutdown-req
      (make-request lsp "shutdown" #f))
    (client-send lsp shutdown-req)
    (client-wait-response shutdown-req)

    (define exit-notf (make-notification "exit" #f))
    (client-send lsp exit-notf)

    (void)))

(define/contract (client-send lsp req)
  (-> any/c jsexpr? void?)

  (send lsp process-message req))

(define/contract (client-wait-response req)
  (-> any/c jsexpr?)

  (define msg (async-channel-get (response-channel)))

  (match msg
    [(hash-table ['id (? (or/c number? string?) id)]
                 ['result _result])
     (cond
       [(equal? (hash-ref req 'id) id) msg]
       ; not the response of this request, wait next
       [else (async-channel-put (response-channel) msg)])]
    [_ (error "Not a response from server")]))

(define (client-wait-notification lsp)
  (async-channel-get (notification-channel)))

(define/contract (make-request lsp method params)
  (-> any/c string? jsexpr? jsexpr?)

  (define req
    (hasheq 'jsonrpc "2.0"
            'id (genid)
            'method method))
  (cond [(not params) req]
        [else (hash-set req 'params params)]))

(define/contract (make-expected-response request result)
  (-> jsexpr? jsexpr? jsexpr?)

  (define res
    (hasheq 'jsonrpc "2.0"
            'id (hash-ref request 'id)))
  (cond [(not result) res]
        [else (hash-set res 'result result)]))

(define/contract (make-notification method params)
  (-> string? jsexpr? jsexpr?)

  (define req
    (hasheq 'jsonrpc "2.0"
            'method method))
  (cond [(not params) req]
        [else (hash-set req 'params params)]))
