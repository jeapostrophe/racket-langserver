#lang racket

;; This module provides a simple incomplete lsp client for test.

(provide with-racket-lsp
         client-send
         client-wait-response
         make-request
         make-expected-response
         make-notification
         handle-server-request)

(require racket/os
         racket/async-channel
         racket/match
         json
         data/queue
         "../server.rkt"
         "../methods.rkt"
         "../msg-io.rkt")

(define id 0)
(define (Lsp-genid lsp)
  (set! id (add1 id))
  id)

(define server-output-ch (make-parameter #f))
(define response-channel (make-parameter #f))

(define/contract (process-message-from-server lsp msg)
  (-> any/c jsexpr? void?)

  (match msg
    ;; Server request - handle immediately and send response
    [(hash-table ['id (? (or/c number? string?) id)]
                 ['method (? string? method)])
     (handle-server-request lsp msg)]
    ;; Server notification - queue diagnostic notifications, ignore others
    [(hash-table ['method (? string? method)])
     (when (equal? method "textDocument/publishDiagnostics")
       (async-channel-put (response-channel) msg))]
    ;; Response - put it to another channel for client-wait-response
    [else
     (async-channel-put (response-channel) msg)]))

(define/contract (with-racket-lsp proc)
  (-> (-> any/c any/c) void?)
  (define ch (make-async-channel))

  (parameterize ([server-output-ch ch]
                 [response-channel (make-async-channel)])
    (set-current-server! (new server% [output-channel ch]))
    (define lsp current-server)

    (define (handle-output)
      (define msg (async-channel-get (server-output-ch)))
      (cond
        [(jsexpr? msg) (process-message-from-server lsp msg)]
        [(void? msg) (void)]
        [else (printf "handle-output ~a~n" msg)])
      (handle-output))
    (thread handle-output)

    (define init-req
      (make-request lsp "initialize"
                    (hasheq 'processId (getpid)
                            'capabilities (hasheq))))
    (client-send lsp init-req)
    (client-wait-response lsp)

    (proc lsp)

    (define shutdown-req
      (make-request lsp "shutdown" #f))
    (client-send lsp shutdown-req)
    (client-wait-response lsp)

    (define exit-notf (make-notification "exit" #f))
    (client-send lsp exit-notf)
    (client-should-no-response lsp)

    (void)))

(define/contract (client-send lsp req)
  (-> any/c jsexpr? void?)

  (send lsp process-message req))

(define/contract (client-wait-response lsp)
  (-> any/c jsexpr?)

  (define js (async-channel-get (response-channel)))
  (make-immutable-hasheq (hash->list js)))

(define/contract (client-should-no-response lsp)
  (-> any/c eof-object?)

  (async-channel-get (response-channel))
  )

(define/contract (make-request lsp method params)
  (-> any/c string? jsexpr? jsexpr?)

  (define req
    (hasheq 'jsonrpc "2.0"
            'id (Lsp-genid lsp)
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

;; Currently this is just a stub procedure that always reply null
(define/contract (handle-server-request lsp request)
  (-> any/c jsexpr? void?)
  (define id (hash-ref request 'id))
  (define method (hash-ref request 'method))

  (define result
    (match method
      ["workspace/configuration" (list (json-null))]
      [_ (json-null)]))

  (define response
    (hasheq 'jsonrpc "2.0"
            'id id
            'result result))

  (client-send lsp response))
