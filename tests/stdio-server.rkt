#lang racket/base

;; Helpers for spawning the real stdio server process and exchanging strict
;; Content-Length framed JSON-RPC messages.
(provide (struct-out Stdio-Client)
         call-with-stdio-client
         stdio-send!
         stdio-receive
         stdio-response
         stdio-result
         stdio-initialize!
         stdio-shutdown-and-exit!)

(require json
         racket/async-channel
         racket/port
         racket/runtime-path
         "../common/time.rkt"
         "../lsp/msg-io.rkt")

(define-runtime-path main-path "../main.rkt")

(define (read-frame in)
  (define header (read-line in 'return-linefeed))
  (cond
    [(eof-object? header) eof]
    [else
     (define found (regexp-match #rx"^Content-Length: ([0-9]+)$" header))
     (unless found
       (error 'read-frame "unexpected protocol header: ~s" header))
     (unless (equal? (read-line in 'return-linefeed) "")
       (error 'read-frame "missing header separator"))
     (define size (string->number (cadr found)))
     (define body (read-bytes size in))
     (unless (and (bytes? body) (= (bytes-length body) size))
       (error 'read-frame "incomplete protocol body"))
     (bytes->jsexpr body)]))

;; The callback owns normal shutdown and reader joins. Cleanup also handles a
;; failed assertion; supplied stderr ports remain owned by the caller.
(define (call-with-server-process proc
                                  #:arguments [arguments '()]
                                  #:environment [environment (current-environment-variables)]
                                  #:stderr [error-port #f])
  (define readers (make-custodian))
  (define-values (server stdout stdin stderr)
    (parameterize ([current-environment-variables environment])
      (apply subprocess #f #f error-port (find-executable-path "racket")
             (append arguments (list "-t" (path->string main-path))))))
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([current-custodian readers])
        (proc server stdout stdin stderr)))
    (lambda ()
      (subprocess-kill server #t)
      (subprocess-wait server)
      (custodian-shutdown-all readers)
      (close-output-port stdin)
      (close-input-port stdout)
      (when stderr
        (close-input-port stderr)))))

(struct Stdio-Client
  (process stdin messages pending frame-reader error-reader stderr-output)
  #:transparent)

(define (call-with-stdio-client proc
                                #:arguments [arguments '()]
                                #:environment [environment (current-environment-variables)]
                                #:stderr [error-port #f]
                                #:on-message [on-message void])
  (call-with-server-process
    #:arguments arguments
    #:environment environment
    #:stderr error-port
    (lambda (server stdout stdin stderr)
      (define messages (make-async-channel))
      (define stderr-output (open-output-string))
      (define error-reader
        (and stderr
             (thread (lambda () (copy-port stderr stderr-output)))))
      (define frame-reader
        (thread
          (lambda ()
            (with-handlers ([exn? (lambda (e) (async-channel-put messages e))])
              (let loop ()
                (define msg (read-frame stdout))
                (cond
                  [(eof-object? msg)
                   (async-channel-put messages eof)]
                  [else
                   (on-message msg)
                   (async-channel-put messages msg)
                   (loop)]))))))
      (proc (Stdio-Client server stdin messages (make-hash) frame-reader error-reader stderr-output)))))

(define (stdio-send! client method [params #f] [id #f])
  (define base
    (hasheq 'jsonrpc "2.0"
            'method method))
  (define with-params
    (if params (hash-set base 'params params) base))
  (display-message/flush (if id (hash-set with-params 'id id) with-params)
                         (Stdio-Client-stdin client)))

(define (stdio-receive client [timeout 30])
  (define msg (sync/timeout timeout (Stdio-Client-messages client)))
  (unless msg
    (error 'stdio-receive "server message timed out after ~a seconds" timeout))
  (when (exn? msg)
    (raise msg))
  msg)

(define (stdio-response client id [timeout 20])
  (define pending (Stdio-Client-pending client))
  (define deadline (deadline-after timeout))
  (define result
    (or (hash-ref pending id #f)
        (let loop ()
          (define remaining
            (max 0 (/ (- deadline (current-deadline-milliseconds)) 1000)))
          (when (zero? remaining)
            (error 'stdio-response "response deadline expired for ~a" id))
          (define msg (stdio-receive client remaining))
          (when (eof-object? msg)
            (error 'stdio-response "unexpected EOF waiting for ~a" id))
          (define received (hash-ref msg 'id #f))
          (cond
            [(equal? received id) msg]
            [else
             (when received
               (hash-set! pending received msg))
             (loop)]))))
  (hash-remove! pending id)
  (unless (equal? (hash-ref result 'jsonrpc #f) "2.0")
    (error 'stdio-response "expected jsonrpc 2.0 in response: ~s" result))
  result)

(define (stdio-result client id [timeout 20])
  (define resp (stdio-response client id timeout))
  (when (hash-has-key? resp 'error)
    (error 'stdio-result "unexpected error response: ~s" resp))
  (hash-ref resp 'result))

(define (stdio-initialize! client [id 1] #:capabilities [capabilities (hasheq)])
  (stdio-send! client
               "initialize"
               (hasheq 'processId (json-null)
                       'capabilities capabilities
                       'rootUri (json-null)
                       'rootPath (json-null))
               id)
  (stdio-result client id))

(define (stdio-shutdown-and-exit! client [id 999])
  (stdio-send! client "shutdown" (hasheq) id)
  (define resp (stdio-result client id))
  (stdio-send! client "exit" (hasheq))
  (define exited (sync/timeout 20 (Stdio-Client-process client)))
  (unless exited
    (error 'stdio-shutdown-and-exit! "server process did not exit"))
  (define reader-done (sync/timeout 5 (Stdio-Client-frame-reader client)))
  (unless reader-done
    (error 'stdio-shutdown-and-exit! "frame reader did not reach EOF"))
  (when (Stdio-Client-error-reader client)
    (sync/timeout 5 (Stdio-Client-error-reader client)))
  (let drain ()
    (define msg (sync/timeout 0 (Stdio-Client-messages client)))
    (when (exn? msg)
      (raise msg))
    (when msg
      (drain)))
  (values resp (subprocess-status (Stdio-Client-process client))))
