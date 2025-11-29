#lang racket

;; This module provides a simple incomplete lsp client for test.

(provide with-racket-lsp
         Lsp?
         client-send
         client-wait-response
         make-request
         make-expected-response
         make-notification
         handle-server-request)

(require racket/os
         json
         data/queue
         "../msg-io.rkt")

(struct Lsp
  (stdout stdin stderr response-queue [id #:mutable #:auto])
  #:auto-value -1)

(define (Lsp-genid lsp)
  (set-Lsp-id! lsp (add1 (Lsp-id lsp)))
  (Lsp-id lsp))

(define/contract (process-incoming-message lsp msg)
  (-> Lsp? jsexpr? void?)

  (cond
    ;; Server request - handle immediately and send response
    [(and (hash-has-key? msg 'method) (hash-has-key? msg 'id))
     (handle-server-request lsp msg)]
    ;; Server notification - queue diagnostic notifications, ignore others
    [(hash-has-key? msg 'method)
     (define method (hash-ref msg 'method))
     (when (equal? method "textDocument/publishDiagnostics")
       (enqueue! (Lsp-response-queue lsp) msg))]
    ;; Response - queue it for client-wait-response
    [else
     (enqueue! (Lsp-response-queue lsp) msg)]))

(define ((forward-errors in))
  (for ([str (in-port read-line in)])
    (displayln (format "LSP ERROR: ~a" str) (current-error-port))))

(define/contract (with-racket-lsp path proc)
  (-> string? (-> Lsp? any/c) void?)

  (define racket-path (find-executable-path "racket"))
  (define-values (sp stdout stdin stderr)
    (subprocess #f #f #f racket-path path))
  (define _err-thd (thread (forward-errors stderr)))
  (define lsp (Lsp stdout stdin stderr (make-queue)))

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

  (subprocess-wait sp))

(define/contract (client-send lsp req)
  (-> Lsp? jsexpr? void?)

  (display-message/flush req (Lsp-stdin lsp)))

(define/contract (client-wait-response lsp)
  (-> Lsp? jsexpr?)

  ;; First, check if there's already a response in the queue
  (define queued-response
    (if (queue-empty? (Lsp-response-queue lsp))
        #f
        (dequeue! (Lsp-response-queue lsp))))

  (if queued-response
      queued-response
      ;; No queued response, so read and process messages until we get one
      (let loop ()
        (define msg (read-message (Lsp-stdout lsp)))
        (process-incoming-message lsp msg)
        ;; Check if processing the message added a response to the queue
        (if (queue-empty? (Lsp-response-queue lsp))
            (loop)
            (dequeue! (Lsp-response-queue lsp))))))

(define/contract (client-should-no-response lsp)
  (-> Lsp? eof-object?)
  
  (read-message (Lsp-stdout lsp)))

(define/contract (make-request lsp method params)
  (-> Lsp? string? jsexpr? jsexpr?)

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
  (-> Lsp? jsexpr? void?)
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
