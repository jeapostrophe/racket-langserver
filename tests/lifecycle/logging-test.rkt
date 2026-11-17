#lang racket/base

(module+ test
  (require rackunit
           racket/file
           racket/match
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../json-dsl.rkt"
           "../stdio-server.rkt")

  (for ([setting (in-list
                   (list (list "default error logging" #f '() #t #f)
                         (list "debug logging through PLTSTDERR"
                               "error debug@racket-langserver" '() #t #t)
                         (list "debug logging through -W" #f
                               '("-W" "error debug@racket-langserver") #t #t)
                         (list "disabled logging" "none" '() #f #f)))])
    (match-define (list name levels arguments error? debug?) setting)
    (test-case name
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
          (call-with-stdio-client
            #:arguments arguments
            #:environment environment
            #:stderr log-port
            (lambda (client)
              ;; An empty path exercises report-request-error before initialization.
              (stdio-send! client "initialize"
                           (json {#:processId null
                                  #:capabilities {}
                                  #:rootUri null
                                  #:rootPath ""})
                           3)
              (define failed (stdio-receive client))
              (check-equal? (hash-ref failed 'id) 3)
              (check-equal? (hash-ref (hash-ref failed 'error) 'code)
                            (->jsexpr ErrorCode-InternalError))

              (define initialized (stdio-initialize! client 1))
              (check-true (hash? initialized))

              ;; Missing textDocument exercises report-error in the dispatch loop.
              (stdio-send! client "textDocument/didClose" (json {}))
              (stdio-send! client "logging-test/unknown-method" (json {}) 4)
              (define unknown (stdio-receive client))
              (check-equal? (hash-ref unknown 'id) 4)
              (check-equal? (hash-ref (hash-ref unknown 'error) 'code)
                            (->jsexpr ErrorCode-MethodNotFound))

              (stdio-send! client "logging-test/debug-marker" (json {}))
              (define-values (shutdown-res exit-code) (stdio-shutdown-and-exit! client 2))
              (check-equal? shutdown-res (json null))
              (check-equal? exit-code 0)))

          (define log (file->string log-path))
          (check-regexp-match #rx"^previous session\n" log)
          (check-equal? (regexp-match? #rx"racket-langserver: Caught exn:" log) error?)
          (check-equal? (regexp-match? #rx"racket-langserver: Caught exn in request" log) error?)
          (check-equal? (regexp-match? #rx"racket-langserver: invalid request for method" log) error?)
          (check-equal? (regexp-match? #rx"logging-test/debug-marker" log) debug?)
          (when error?
            (check-regexp-match #rx"path string is empty" log))
          (unless (or error? debug?)
            (check-equal? log "previous session\n")))
        (lambda ()
          (close-output-port log-port)
          (delete-file log-path))))))
