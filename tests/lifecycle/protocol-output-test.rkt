#lang racket/base

;; Exercise module initialization, reader discovery, reading, lexing, and
;; formatting through the public reader API to ensure noisy language callbacks
;; cannot corrupt protocol stdout.
(module+ test
  (require rackunit
           racket/file
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../../common/path-util.rkt"
           "../json-dsl.rkt"
           "../stdio-server.rkt")

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

  (test-case "language callbacks cannot corrupt protocol stdout"
    (define directory (make-temporary-file "reader-protocol~a" 'directory))
    (define reader-path (build-path directory "reader.rkt"))
    (display-to-file noisy-reader reader-path)
    (define uri (path->uri (build-path directory "source.rkt")))
    (define text
      (format "#lang reader (file ~s)\n(define (id x)\nx)\n(id 1)\n"
              (path->string reader-path)))
    (dynamic-wind
      void
      (lambda ()
        (define diagnostics 0)
        (call-with-stdio-client
          #:on-message
          (lambda (message)
            (when (equal? (hash-ref message 'method #f) "textDocument/publishDiagnostics")
              (check-equal? (hash-ref (hash-ref message 'params) 'uri) uri)
              (set! diagnostics (add1 diagnostics))))
          (lambda (client)
            (check-true (hash? (stdio-initialize! client 1)))
            (stdio-send! client "workspace/didChangeConfiguration"
                         (json {#:settings
                                {#:racket-langserver
                                 {#:resyntax {#:enable #f}
                                  #:formatting {#:documentFormatter "drracket"}}}}))
            (stdio-send! client "textDocument/didOpen"
                         (json {#:textDocument (->jsexpr (DocItem uri "racket" 1 text))}))
            (stdio-send! client "textDocument/semanticTokens/full"
                         (json {#:textDocument {#:uri uri}}) 2)
            (check-not-equal? (hash-ref (stdio-result client 2) 'data) '())
            (check-equal? diagnostics 1)

            (stdio-send! client "textDocument/formatting"
                         (json {#:textDocument {#:uri uri}
                                #:options {#:tabSize 2 #:insertSpaces #t}})
                         3)
            (check-true (pair? (stdio-result client 3)))

            (stdio-send! client "textDocument/didChange"
                         (json {#:textDocument (->jsexpr (DocIdentifier 2 uri))
                                #:contentChanges [(->jsexpr
                                                    (ContentChangeEvent-Full
                                                      (string-append text "(id 2)\n")))]}))
            (stdio-send! client "textDocument/semanticTokens/full"
                         (json {#:textDocument {#:uri uri}}) 4)
            (check-not-equal? (hash-ref (stdio-result client 4) 'data) '())
            (check-equal? diagnostics 2)

            (define-values (shutdown-res exit-code) (stdio-shutdown-and-exit! client 5))
            (check-equal? shutdown-res (json null))
            (check-equal? exit-code 0)
            (check-regexp-match #rx"reader stderr"
                                (get-output-string (Stdio-Client-stderr-output client))))))
      (lambda () (delete-directory/files directory)))))
