#lang racket/base

(require json
         rackunit
         racket/async-channel
         racket/file
         racket/port
         "../../common/path-util.rkt"
         "../../lsp/msg-io.rkt"
         "../stdio-server.rkt")


;; Exercise module initialization, reader discovery, reading, lexing and
;; formatting through the public reader API. Each callback still does its job.
(define noisy-reader
#<<READER
#lang s-exp syntax/module-reader
racket/base
#:wrapper1 (lambda (read)
             (displayln "reader output")
             (displayln "reader stderr" (current-error-port))
             (read))
#:info noisy-info
(require syntax-color/racket-lexer)
(displayln "module output")
(define (noisy-info key default default-filter)
  (displayln "reader information output")
  (case key
    [(color-lexer)
     (lambda (in) (displayln "lexer output") (racket-lexer in))]
    [(drracket:indentation)
     (lambda _ (displayln "indentation output") 2)]
    [else (default-filter key default)]))
READER
  )

(module+ test
  (test-case
    "language callbacks cannot corrupt protocol stdout"
    (define directory (make-temporary-file "reader-protocol~a" 'directory))
    (define reader-path (build-path directory "reader.rkt"))
    (display-to-file noisy-reader reader-path)
    (define uri (path->uri (build-path directory "source.rkt")))
    (define text
      (format "#lang reader (file ~s)\n(define (id x)\nx)\n(id 1)\n" (path->string reader-path)))
    (dynamic-wind
      void
      (lambda ()
        (call-with-server-process
          (lambda (server stdout stdin stderr)
            (define messages (make-async-channel))
            (define errors (open-output-string))
            (define frame-failure #f)
            (define error-reader
              (thread (lambda () (copy-port stderr errors))))
            (define frame-reader
              (thread
                (lambda ()
                  (with-handlers ([exn? (lambda (e)
                                          (set! frame-failure e)
                                          (async-channel-put messages e))])
                    (let loop ()
                      (define message (read-frame stdout))
                      (unless (eof-object? message)
                        (async-channel-put messages message)
                        (loop)))))))
            (define diagnostics 0)
            (define (send! method params [id #f])
              (define message (hasheq 'jsonrpc "2.0" 'method method 'params params))
              (display-message/flush (if id (hash-set message 'id id) message) stdin))
            (define (response id)
              (define deadline (+ (current-inexact-milliseconds) 30000))
              (let loop ()
                (define message
                  (sync/timeout (max 0 (/ (- deadline (current-inexact-milliseconds)) 1000)) messages))
                (unless message (fail "server response timed out"))
                (when (exn? message) (raise message))
                (when (equal? (hash-ref message 'method #f) "textDocument/publishDiagnostics")
                  (check-equal? (hash-ref (hash-ref message 'params) 'uri) uri)
                  (set! diagnostics (add1 diagnostics)))
                (if (equal? (hash-ref message 'id #f) id)
                    (hash-ref message 'result)
                    (loop))))
            (send! "initialize" (hasheq 'processId (json-null) 'capabilities (hasheq)
                                        'rootUri (json-null) 'rootPath (json-null)) 1)
            (check-true (hash? (response 1)))
            (send! "workspace/didChangeConfiguration"
                   (hasheq 'settings
                           (hasheq 'racket-langserver
                                   (hasheq 'resyntax (hasheq 'enable #f)
                                           'formatting (hasheq 'documentFormatter "drracket")))))
            (send! "textDocument/didOpen"
                   (hasheq 'textDocument (hasheq 'uri uri 'languageId "racket" 'version 1 'text text)))
            (send! "textDocument/semanticTokens/full" (hasheq 'textDocument (hasheq 'uri uri)) 2)
            (check-not-equal? (hash-ref (response 2) 'data) '())
            (check-equal? diagnostics 1)
            (send! "textDocument/formatting"
                   (hasheq 'textDocument (hasheq 'uri uri)
                           'options (hasheq 'tabSize 2 'insertSpaces #t)) 3)
            (check-true (pair? (response 3)))
            (send! "textDocument/didChange"
                   (hasheq 'textDocument (hasheq 'uri uri 'version 2)
                           'contentChanges (list (hasheq 'text (string-append text "(id 2)\n")))))
            (send! "textDocument/semanticTokens/full" (hasheq 'textDocument (hasheq 'uri uri)) 4)
            (check-not-equal? (hash-ref (response 4) 'data) '())
            (check-equal? diagnostics 2)
            (send! "shutdown" (hasheq) 5)
            (check-equal? (response 5) (json-null))
            (send! "exit" (hasheq))
            (check-not-false (sync/timeout 20 server))
            (check-equal? (subprocess-status server) 0)
            (check-eq? (sync/timeout 5 frame-reader) frame-reader)
            (when frame-failure (raise frame-failure))
            (check-eq? (sync/timeout 5 error-reader) error-reader)
            (check-regexp-match #rx"reader stderr" (get-output-string errors)))))
      (lambda () (delete-directory/files directory)))))
