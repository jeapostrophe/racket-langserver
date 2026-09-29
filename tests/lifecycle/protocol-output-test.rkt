#lang racket/base

(require json
         rackunit
         racket/async-channel
         racket/file
         racket/match
         racket/port
         racket/runtime-path
         "../../common/path-util.rkt"
         "../../lsp/msg-io.rkt")

(define-runtime-path main-path "../../main.rkt")

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

;; The production reader tolerates stray lines; a protocol regression must not.
(define (read-frame in)
  (define header (read-line in 'return-linefeed))
  (define found (and (string? header) (regexp-match #rx"^Content-Length: ([0-9]+)$" header)))
  (unless found (error 'read-frame "unexpected protocol header: ~s" header))
  (unless (equal? (read-line in 'return-linefeed) "")
    (error 'read-frame "missing header separator"))
  (define length (string->number (cadr found)))
  (define body (read-bytes length in))
  (unless (and (bytes? body) (= (bytes-length body) length))
    (error 'read-frame "incomplete protocol body"))
  (bytes->jsexpr body))

(module+ test
  (test-case
    "language callbacks cannot corrupt protocol stdout"
    (define directory (make-temporary-file "reader-protocol~a" 'directory))
    (define reader-path (build-path directory "reader.rkt"))
    (display-to-file noisy-reader reader-path)
    (define uri (path->uri (build-path directory "source.rkt")))
    (define text
      (format "#lang reader (file ~s)\n(define (id x)\nx)\n(id 1)\n" (path->string reader-path)))
    (define-values (server stdout stdin stderr)
      (subprocess #f #f #f (find-executable-path "racket") "-t" (path->string main-path)))
    (define readers (make-custodian))
    (define messages (make-async-channel))
    (define errors (open-output-string))
    (define error-reader
      (parameterize ([current-custodian readers])
        (thread (lambda () (copy-port stderr errors)))))
    (parameterize ([current-custodian readers])
      (thread
        (lambda ()
          (with-handlers ([exn? (lambda (e) (async-channel-put messages e))])
            (let loop ()
              (async-channel-put messages (read-frame stdout))
              (loop))))))
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
    (dynamic-wind
      void
      (lambda ()
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
        (check-eq? (sync/timeout 5 error-reader) error-reader)
        (check-regexp-match #rx"reader stderr" (get-output-string errors)))
      (lambda ()
        (subprocess-kill server #t)
        (subprocess-wait server)
        (custodian-shutdown-all readers)
        (close-output-port stdin)
        (close-input-port stdout)
        (close-input-port stderr)
        (delete-directory/files directory)))))
