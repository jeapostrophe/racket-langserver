#lang racket/base

(require json
         rackunit
         racket/async-channel
         racket/file
         racket/match
         "../../lsp/msg-io.rkt"
         "../stdio-server.rkt")


(module+ test
  (for ([setting (in-list
                   (list (list "default error logging" #f '() #t #f)
                         (list "debug logging through PLTSTDERR"
                               "error debug@racket-langserver" '() #t #t)
                         (list "debug logging through -W" #f
                               '("-W" "error debug@racket-langserver") #t #t)
                         (list "disabled logging" "none" '() #f #f)))])
    (match-define (list name levels arguments error? debug?) setting)
    (test-case
      name
      (define log-path (make-temporary-file "server-log~a"))
      (display-to-file "previous session\n" log-path #:exists 'truncate)
      (define log-port (open-output-file log-path #:exists 'append))
      (define environment (environment-variables-copy (current-environment-variables)))
      (environment-variables-set! environment #"PLTSTDERR"
                                  (and levels (string->bytes/utf-8 levels)))
      (environment-variables-set! environment #"PLTSTDOUT" #"none")
      (environment-variables-set! environment #"PLTSYSLOG" #"none")
      (dynamic-wind
        void
        (lambda ()
          (call-with-server-process
            (lambda (server stdout stdin _stderr)
              (define messages (make-async-channel))
              (thread
                (lambda ()
                  (with-handlers ([exn? (lambda (e) (async-channel-put messages e))])
                    (let loop ()
                      (define message (read-frame stdout))
                      (async-channel-put messages message)
                      (unless (eof-object? message) (loop))))))
              (define (receive)
                (define message (sync/timeout 30 messages))
                (unless message (fail "server response timed out"))
                (when (exn? message) (raise message))
                message)
              (define (send! method params [id #f])
                (define message (hasheq 'jsonrpc "2.0" 'method method 'params params))
                (display-message/flush (if id (hash-set message 'id id) message) stdin))
              ;; An empty path exercises report-request-error before initialization.
              (send! "initialize" (hasheq 'processId (json-null) 'capabilities (hasheq)
                                          'rootUri (json-null) 'rootPath "") 3)
              (define failed (receive))
              (check-equal? (hash-ref failed 'id) 3)
              (check-equal? (hash-ref (hash-ref failed 'error) 'code) -32603)
              (send! "initialize" (hasheq 'processId (json-null) 'capabilities (hasheq)
                                          'rootUri (json-null) 'rootPath (json-null)) 1)
              (define initialized (receive))
              (check-equal? (hash-ref initialized 'id) 1)
              (check-true (hash? (hash-ref initialized 'result)))
              ;; Missing textDocument exercises report-error in the dispatch loop.
              (send! "textDocument/didClose" (hasheq))
              (send! "logging-test/unknown-method" (hasheq) 4)
              (define unknown (receive))
              (check-equal? (hash-ref unknown 'id) 4)
              (check-equal? (hash-ref (hash-ref unknown 'error) 'code) -32601)
              (send! "logging-test/debug-marker" (hasheq))
              (send! "shutdown" (hasheq) 2)
              (check-equal? (receive) (hasheq 'jsonrpc "2.0" 'id 2 'result (json-null)))
              (send! "exit" (hasheq))
              (check-not-false (sync/timeout 20 server))
              (check-equal? (subprocess-status server) 0)
              (check-true (eof-object? (receive)))
              (define log (file->string log-path))
              (check-regexp-match #rx"^previous session\n" log)
              (check-equal? (regexp-match? #rx"racket-langserver: Caught exn:" log) error?)
              (check-equal? (regexp-match? #rx"racket-langserver: Caught exn in request" log) error?)
              (check-equal? (regexp-match? #rx"racket-langserver: invalid request for method" log) error?)
              (check-equal? (regexp-match? #rx"logging-test/debug-marker" log) debug?)
              (when error? (check-regexp-match #rx"path string is empty" log))
              (unless (or error? debug?) (check-equal? log "previous session\n")))
            #:arguments arguments
            #:environment environment
            #:stderr log-port))
        (lambda ()
          (close-output-port log-port)
          (delete-file log-path))))))
