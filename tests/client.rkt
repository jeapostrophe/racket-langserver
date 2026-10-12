#lang racket

;; This module provides a simple incomplete lsp client for test.

(provide with-racket-lsp
         client-send
         client-wait-response
         client-wait-notification
         client-has-diagnostic?
         make-request
         make-expected-response
         make-notification)

(require racket/os
         racket/async-channel
         racket/match
         racket/runtime-path
         json
         "../common/json-util.rkt"
         "../lsp/methods.rkt"
         "../main.rkt")

(define id 0)
(define (genid)
  (set! id (add1 id))
  id)

(define request-channel (make-parameter #f))
(define response-channel (make-parameter #f))
(define notification-channel (make-parameter #f))
(define client-channel (make-parameter #f))
(define pending-responses (make-parameter #f))

(define-runtime-path rootFolder "./")

(define/contract (with-racket-lsp proc #:capabilities [capabilities (hasheq)])
  (->* ((-> any/c any/c)) (#:capabilities jsexpr?) void?)
  (parameterize ([response-channel (make-async-channel)]
                 [request-channel (make-async-channel)]
                 [notification-channel (make-async-channel)]
                 [client-channel (make-async-channel)]
                 [pending-responses (make-hash)])
    (define custodian (make-custodian))
    (define exited (make-async-channel))
    (define lsp
      (parameterize ([current-custodian custodian]
                     [exit-handler (lambda (code) (async-channel-put exited code))])
        (define server (new server%
                         [response-channel (response-channel)]
                         [request-channel (request-channel)]
                         [notification-channel (notification-channel)]))
        (start-consumer server (client-channel))
        server))

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
    (parameterize ([current-custodian custodian]) (thread handle-request))

    (define init-req
      (make-request lsp "initialize"
                    (hasheq 'processId (getpid)
                            'capabilities capabilities
                            'rootPath (path->string (normalize-path rootFolder))
                            'rootUri (string-append "file://" (path->string (normalize-path rootFolder))))))
    (client-send lsp init-req)
    (client-wait-response init-req)

    (dynamic-wind
      void
      (lambda ()
        (proc lsp)

        (define shutdown-req
          (make-request lsp "shutdown" #f))
        (client-send lsp shutdown-req)
        (client-wait-response shutdown-req)

        ;; The server exits the process on this notification, and the tests share
        ;; one process, so take the code it exits with instead of the exit.
        (define exit-notf (make-notification "exit" #f))
        (client-send lsp exit-notf)
        (define exit-code (sync/timeout 5 exited))
        (unless (eqv? exit-code 0)
          (error 'with-racket-lsp "server exited with ~v after shutdown" exit-code)))
      (lambda () (custodian-shutdown-all custodian)))

    (void)))

(define/contract (client-send lsp req)
  (-> any/c jsexpr? void?)

  (async-channel-put (client-channel) req))

(define/contract (client-wait-response req)
  (-> any/c jsexpr?)
  (define id (hash-ref req 'id))
  (define pending (pending-responses))
  (define result
    (or (hash-ref pending id #f)
        (let loop ()
          (define msg (async-channel-get (response-channel)))
          (match msg
            [(or (hash-table ['id (? (or/c number? string?) received-id)] ['result _])
                 (hash-table ['id (? (or/c number? string?) received-id)] ['error _]))
             (cond
               [(equal? received-id id) msg]
               [else (hash-set! pending received-id msg) (loop)])]
            [_ (error "Not a response from server")]))))
  (hash-remove! pending id)
  result)

(define (client-wait-notification lsp)
  (async-channel-get (notification-channel)))

(define/contract (client-has-diagnostic? notification expected-diagnostic)
  (-> jsexpr? hash? boolean?)

  (for/or ([diagnostic (in-list (jsexpr-ref notification '(params diagnostics)))])
    (equal? diagnostic expected-diagnostic)))

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


(module+ test
  (require rackunit)
  (test-case "response waits preserve replies that arrive in reverse request order"
    (parameterize ([response-channel (make-async-channel)]
                   [pending-responses (make-hash)])
      (define first (hasheq 'id 1 'result '()))
      (define second (hasheq 'id 2 'error (hasheq 'code -32803 'message "failed")))
      (async-channel-put (response-channel) second)
      (async-channel-put (response-channel) first)
      (check-equal? (client-wait-response (hasheq 'id 1)) first)
      (check-equal? (client-wait-response (hasheq 'id 2)) second)
      (check-true (hash-empty? (pending-responses))))))
