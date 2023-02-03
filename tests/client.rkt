#lang racket/base

;; This module provides a simple incomplete lsp client for test.

(provide with-racket-lsp
         client-send
         client-wait-response
         make-request
         make-notification)

(require racket/os
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

;; (-> string? (-> Lsp? any/c) void?)
(define (with-racket-lsp path thunk)
  (define racket-path (find-executable-path "racket"))
  (define-values (sp stdout stdin stderr)
    (subprocess #f #f #f racket-path path))
  (define err-thd (thread (forward-errors stderr)))
  (define lsp (Lsp stdout stdin stderr))

  (define init-req
    (make-request lsp "initialize"
                  (hasheq 'processId (getpid)
                          'capabilities (hasheq))))
  (client-send lsp init-req)
  (client-wait-response lsp)

  (thunk (Lsp stdout stdin stderr))

  (define shutdown-req
    (make-request lsp "shutdown" #f))
  (client-send lsp shutdown-req)
  (client-wait-response lsp)

  (define exit-notf (make-notification "exit" #f))
  (client-send lsp exit-notf)
  (client-wait-response lsp)

  (subprocess-wait sp))

(define (client-send lsp req)
  (display-message/flush req (Lsp-stdin lsp)))

(define (client-wait-response lsp)
  (read-message (Lsp-stdout lsp)))

(define (make-request lsp method params)
  (define req
    (hasheq 'jsonrpc "2.0"
            'id (Lsp-genid lsp)
            'method method))
  (cond [(not params) req]
        [else (hash-set req 'params params)]))

(define (make-notification method params)
  (define req
    (hasheq 'jsonrpc "2.0"
            'method method))
  (cond [(not params) req]
        [else (hash-set req 'params params)]))