#lang racket/base

;; End-to-end evidence for the prototype: real stdio frames and gated macro
;; expansion. File markers establish ordering, not elapsed sleeps.
(require json rackunit racket/async-channel racket/file racket/port
         racket/path
         racket/runtime-path racket/string
         "../../common/path-util.rkt"
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../common/time.rkt"
         "../../lsp/msg-io.rkt"
         "../stdio-server.rkt")

(define-runtime-path scratch-path "../../.agent-scratch")
(define scratch (simplify-path scratch-path #f))

(define (target-text root value)
  (string-append
    "#lang racket/base\n(provide shared)\n;; Target comment\n"
    (format "(define shared ~a)\n" value)
    "(require (for-syntax racket/base))\n"
    "(define-syntax (probe stx)\n"
    "  (displayln \"expansion stdout must be isolated\")\n"
    (format "  (call-with-output-file ~s #:exists 'append\n"
            (path->string (build-path root "expansions")))
    "    (lambda (out) (displayln 'expanded out)))\n"
    (format "  (when (file-exists? ~s)\n" (path->string (build-path root "hold")))
    (format "    (call-with-output-file ~s #:exists 'truncate void)\n"
            (path->string (build-path root "entered")))
    "    (let wait ()\n"
    (format "      (unless (file-exists? ~s) (sleep 0.01) (wait))))\n"
            (path->string (build-path root "release")))
    "  #'(void))\n(probe)\n"))

(define (write-text path text) (display-to-file text path #:exists 'truncate))
(define (count root) (length (file->lines (build-path root "expansions"))))

(define (wait-entered root)
  (define marker (build-path root "entered"))
  (define deadline (+ (current-deadline-milliseconds) 20000))
  (let loop ()
    (unless (file-exists? marker)
      (when (>= (current-deadline-milliseconds) deadline)
        (error 'wait-entered "target did not reach expansion gate"))
      (sleep 0.01)
      (loop))))

(module+ test
  (test-case "real transport shares target facts, refreshes saves, and cancels held imports"
    (make-directory* scratch)
    (define root (make-temporary-file "background-protocol~a" 'directory scratch))
    (dynamic-wind
      void
      (lambda ()
        (define a (build-path root "a.rkt"))
        (define b (build-path root "b.rkt"))
        (define c (build-path root "c.rkt"))
        (define a-uri (path->uri a))
        (define b-uri (path->uri b))
        (define c-uri (path->uri c))
        (define a-text "#lang racket/base\n(require \"b.rkt\")\n(define local 1)\n(list shared local)\n")
        (write-text a a-text)
        (write-text b (target-text root 1))
        (call-with-server-process
          (lambda (server stdout stdin stderr)
            (define messages (make-async-channel))
            (define pending (make-hash))
            (define diagnostics (make-hash))
            (define frame-failure #f)
            (thread (lambda () (copy-port stderr (open-output-nowhere))))
            (define frame-reader
              (thread
                (lambda ()
                  (with-handlers ([exn? (lambda (e)
                                          (set! frame-failure e)
                                          (async-channel-put messages e))])
                    (let loop ()
                      (define msg (read-frame stdout))
                      (unless (eof-object? msg)
                        (when (equal? (hash-ref msg 'method #f) "textDocument/publishDiagnostics")
                          (hash-set! diagnostics (hash-ref (hash-ref msg 'params) 'uri) #t))
                        (async-channel-put messages msg)
                        (loop)))))))
            (define (send! method params [id #f])
              (define msg (hasheq 'jsonrpc "2.0" 'method method 'params params))
              (display-message/flush (if id (hash-set msg 'id id) msg) stdin))
            (define (response id)
              (define deadline (+ (current-deadline-milliseconds) 20000))
              (define result
                (or (hash-ref pending id #f)
                    (let loop ()
                      (when (>= (current-deadline-milliseconds) deadline)
                        (error 'response "response deadline expired for ~a" id))
                      (define msg
                        (sync/timeout (max 0 (/ (- deadline (current-deadline-milliseconds)) 1000))
                                      messages))
                      (unless msg (error 'response "no response for ~a" id))
                      (when (exn? msg) (raise msg))
                      (define received (hash-ref msg 'id #f))
                      (cond [(equal? received id) msg]
                            [else (when received (hash-set! pending received msg)) (loop)]))))
              (hash-remove! pending id)
              result)
            (define (result id) (hash-ref (response id) 'result))
            (define (open! uri text version)
              (send! "textDocument/didOpen"
                     (hasheq 'textDocument (hasheq 'uri uri 'languageId "racket"
                                                   'version version 'text text))))
            (define (query method id [uri a-uri] [pos (Pos 3 7)])
              (send! method (hasheq 'textDocument (hasheq 'uri uri) 'position (->jsexpr pos)) id))
            (define (check-definition id)
              (check-equal? (result id)
                            (->jsexpr (Location b-uri (Range (Pos 3 8) (Pos 3 14))))))
            (define (check-hover id value)
              (define hover (result id))
              (check-equal? (hash-ref hover 'range) (->jsexpr (Range (Pos 3 6) (Pos 3 12))))
              (check-true (string-contains? (hash-ref hover 'contents)
                                            (format ";; Target comment\n(define shared ~a)" value))))
            (send! "initialize" (hasheq 'processId (json-null) 'capabilities (hasheq)
                                        'rootUri (json-null) 'rootPath (json-null)) 1)
            (result 1)
            (send! "workspace/didChangeConfiguration"
                   (hasheq 'settings (hasheq 'racket-langserver
                                             (hasheq 'resyntax (hasheq 'enable #f)))))
            (open! a-uri a-text 0)
            (send! "textDocument/semanticTokens/full" (hasheq 'textDocument (hasheq 'uri a-uri)) 2)
            (result 2)
            (define dependency-count (count root))
            (write-text (build-path root "hold") "")
            (query "textDocument/definition" 3)
            (wait-entered root)
            ;; The consumer processes both requests before release. Owner-side
            ;; tests establish atomic joining; this barrier cannot observe whether
            ;; the second deferred thread has joined yet.
            (query "textDocument/hover" 4)
            (send! "textDocument/hover" (hasheq) 90)
            (check-true (hash-has-key? (response 90) 'error))
            (check-equal? (- (count root) dependency-count) 1)
            (write-text (build-path root "release") "")
            (check-definition 3)
            (check-hover 4 1)
            (check-equal? (- (count root) dependency-count) 1)
            (query "textDocument/hover" 5)
            (check-hover 5 1)
            (query "textDocument/definition" 6)
            (check-definition 6)
            (check-equal? (- (count root) dependency-count) 1)
            ;; A save during analysis leaves this request on its acquired
            ;; snapshot. The next query observes the new disk text.
            (delete-file (build-path root "entered"))
            (delete-file (build-path root "release"))
            (write-text b (target-text root 2))
            (query "textDocument/hover" 7)
            (wait-entered root)
            (write-text b (target-text root 3))
            (write-text (build-path root "release") "")
            (check-hover 7 2)
            (check-equal? (- (count root) dependency-count) 2)
            (query "textDocument/hover" 70)
            (check-hover 70 3)
            (check-equal? (- (count root) dependency-count) 3)
            ;; B stays held while an independent C completes Check Syntax and local
            ;; queries. An edit to A cancels its imported request before release.
            (delete-file (build-path root "entered"))
            (delete-file (build-path root "release"))
            (write-text b (target-text root 4))
            (query "textDocument/definition" 8)
            (wait-entered root)
            (open! c-uri "#lang racket/base\n(define independent 1)\nindependent\n" 0)
            (send! "textDocument/semanticTokens/full" (hasheq 'textDocument (hasheq 'uri c-uri)) 9)
            (check-true (pair? (hash-ref (result 9) 'data)))
            (query "textDocument/definition" 10 c-uri (Pos 2 1))
            (check-equal? (result 10)
                          (->jsexpr (Location c-uri (Range (Pos 1 8) (Pos 1 19)))))
            (send! "textDocument/didChange"
                   (hasheq 'textDocument (hasheq 'uri a-uri 'version 1)
                           'contentChanges (list (hasheq 'range (->jsexpr (Range (Pos 2 14) (Pos 2 15)))
                                                         'text "2"))))
            (check-equal? (hash-ref (hash-ref (response 8) 'error) 'code)
                          (->jsexpr ErrorCode-ContentModified))
            (query "textDocument/definition" 11 a-uri (Pos 3 14))
            (check-equal? (result 11)
                          (->jsexpr (Location a-uri (Range (Pos 2 8) (Pos 2 13)))))
            (check-false (file-exists? (build-path root "release")))
            (write-text (build-path root "release") "")
            (send! "textDocument/semanticTokens/full" (hasheq 'textDocument (hasheq 'uri a-uri)) 12)
            (result 12)
            (send! "shutdown" (hasheq) 13)
            (check-equal? (result 13) (json-null))
            (send! "exit" (hasheq))
            (check-not-false (sync/timeout 20 server))
            (check-equal? (subprocess-status server) 0)
            ;; EOF after process exit bounds the entire stream, including messages
            ;; queued after the last response. The reader is the only log writer.
            (check-eq? (sync/timeout 5 frame-reader) frame-reader)
            (check-false frame-failure)
            (check-false (hash-ref diagnostics b-uri #f))
            (printf "Transport: unchanged target +1 expansion; each changed snapshot +1; cancellation and independent queries precede gate release.\n"))))
      (lambda () (delete-directory/files root)))))
