#lang racket

;; This module provides a simple incomplete lsp client for test.

(provide with-racket-lsp
         Lsp?
         client-send
         client-wait-response
         make-request
         make-response
         make-notification)

(require racket/os
         json
         "../msg-io.rkt")

(struct Lsp
  (stdout stdin stderr [id #:mutable #:auto])
  #:auto-value -1)

(define (Lsp-genid lsp)
  (set-Lsp-id! lsp (add1 (Lsp-id lsp)))
  (Lsp-id lsp))

(define ((forward-errors in))
  (for ([str (in-port read-line in)])
    (displayln (format "LSP ERROR: ~a" str) (current-error-port))))

(define/contract (with-racket-lsp path proc)
  (-> string? (-> Lsp? any/c) void?)

  (define racket-path (find-executable-path "racket"))
  (define-values (sp stdout stdin stderr)
    (subprocess #f #f #f racket-path path))
  (define _err-thd (thread (forward-errors stderr)))
  (define lsp (Lsp stdout stdin stderr))

  (define init-req
    (make-request lsp "initialize"
                  (hasheq 'processId (getpid)
                          'capabilities (hasheq))))
  (client-send lsp init-req)
  (client-wait-response lsp)

  (proc (Lsp stdout stdin stderr))

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

  (read-message (Lsp-stdout lsp)))

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

(define/contract (make-response request result)
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
