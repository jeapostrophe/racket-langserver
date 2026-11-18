#lang racket/base

;; End-to-end evidence over real stdio frames and gated macro expansion.
;; File markers synchronize ordering across the server subprocess via 10 ms polling.
(module+ test
  (require rackunit
           racket/file
           racket/runtime-path
           racket/string
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../../common/path-util.rkt"
           "../../common/time.rkt"
           "../json-dsl.rkt"
           "../stdio-server.rkt")

  (define-runtime-path hover-path "../../lsp/compose/hover.rkt")

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

  (define (write-text path text)
    (display-to-file text path #:exists 'truncate))

  (define (expansion-count root)
    (length (file->lines (build-path root "expansions"))))

  (define (wait-entered root)
    (define marker (build-path root "entered"))
    (define deadline (+ (current-deadline-milliseconds) 20000))
    (let loop ()
      (unless (file-exists? marker)
        (when (>= (current-deadline-milliseconds) deadline)
          (error 'wait-entered "target did not reach expansion gate"))
        (sleep 0.01)
        (loop))))

  (test-case "real transport shares target facts, refreshes saves, and cancels held imports"
    (define root (make-temporary-file "background-protocol~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (define a (build-path root "a.rkt"))
        (define b (build-path root "b.rkt"))
        (define c (build-path root "c.rkt"))
        (define a-uri (path->uri a))
        (define b-uri (path->uri b))
        (define c-uri (path->uri c))
        (define a-text
          "#lang racket/base\n(require \"b.rkt\")\n(define local 1)\n(list shared local)\n")
        (write-text a a-text)
        (write-text b (target-text root 1))

        (define diagnostics (make-hash))
        (call-with-stdio-client
          #:arguments
          (list "-e" (format "((dynamic-require '(file ~s) 'current-hover-target-timeout) 20)"
                             (path->string (simplify-path hover-path #f))))
          #:on-message
          (lambda (msg)
            (when (equal? (hash-ref msg 'method #f) "textDocument/publishDiagnostics")
              (hash-set! diagnostics (hash-ref (hash-ref msg 'params) 'uri) #t)))
          (lambda (client)
            (define (open! uri text version)
              (stdio-send! client "textDocument/didOpen"
                           (json {#:textDocument (->jsexpr (DocItem uri "racket" version text))})))
            (define (query method id [uri a-uri] [pos (Pos 3 7)])
              (stdio-send! client method
                           (json {#:textDocument {#:uri uri} #:position (->jsexpr pos)})
                           id))
            (define (check-definition id)
              (check-equal? (stdio-result client id)
                            (->jsexpr (Location b-uri (Range (Pos 3 8) (Pos 3 14))))))
            (define (check-hover id value)
              (define hover (stdio-result client id))
              (check-equal? (hash-ref hover 'range)
                            (->jsexpr (Range (Pos 3 6) (Pos 3 12))))
              (check-true
                (string-contains? (hash-ref hover 'contents)
                                  (format ";; Target comment\n(define shared ~a)" value))
                (format "Unexpected hover contents: ~s" (hash-ref hover 'contents))))

            (stdio-initialize! client 1)
            (stdio-send! client "workspace/didChangeConfiguration"
                         (json {#:settings {#:racket-langserver {#:resyntax {#:enable #f}}}}))
            (open! a-uri a-text 0)
            (stdio-send! client "textDocument/semanticTokens/full"
                         (json {#:textDocument {#:uri a-uri}}) 2)
            (stdio-result client 2)

            (define dependency-count (expansion-count root))
            (write-text (build-path root "hold") "")
            (query "textDocument/definition" 3)
            (wait-entered root)
            (query "textDocument/hover" 4)
            (stdio-send! client "textDocument/hover" (json {}) 90)
            (check-true (hash-has-key? (stdio-response client 90) 'error))
            (check-equal? (- (expansion-count root) dependency-count) 1)

            (write-text (build-path root "release") "")
            (check-definition 3)
            (check-hover 4 1)
            (check-equal? (- (expansion-count root) dependency-count) 1)

            (query "textDocument/hover" 5)
            (check-hover 5 1)
            (query "textDocument/definition" 6)
            (check-definition 6)
            (check-equal? (- (expansion-count root) dependency-count) 1)

            ;; A save during analysis leaves this request on its acquired snapshot.
            ;; The next query observes the new disk text.
            (delete-file (build-path root "entered"))
            (delete-file (build-path root "release"))
            (write-text b (target-text root 2))
            (query "textDocument/hover" 7)
            (wait-entered root)
            (write-text b (target-text root 3))
            (write-text (build-path root "release") "")
            (check-hover 7 2)
            (check-equal? (- (expansion-count root) dependency-count) 2)
            (query "textDocument/hover" 70)
            (check-hover 70 3)
            (check-equal? (- (expansion-count root) dependency-count) 3)

            ;; B stays held while an independent C completes Check Syntax and local
            ;; queries. An edit to A cancels its imported request before release.
            (delete-file (build-path root "entered"))
            (delete-file (build-path root "release"))
            (write-text b (target-text root 4))
            (query "textDocument/definition" 8)
            (wait-entered root)
            (open! c-uri "#lang racket/base\n(define independent 1)\nindependent\n" 0)
            (stdio-send! client "textDocument/semanticTokens/full"
                         (json {#:textDocument {#:uri c-uri}}) 9)
            (check-true (pair? (hash-ref (stdio-result client 9) 'data)))
            (query "textDocument/definition" 10 c-uri (Pos 2 1))
            (check-equal? (stdio-result client 10)
                          (->jsexpr (Location c-uri (Range (Pos 1 8) (Pos 1 19)))))

            (stdio-send! client "textDocument/didChange"
                         (json {#:textDocument (->jsexpr (DocIdentifier 1 a-uri))
                                #:contentChanges [(->jsexpr
                                                    (ContentChangeEvent-Incremental
                                                      (Range (Pos 2 14) (Pos 2 15))
                                                      (Nothing)
                                                      "2"))]}))
            (check-equal? (hash-ref (hash-ref (stdio-response client 8) 'error) 'code)
                          (->jsexpr ErrorCode-ContentModified))
            (query "textDocument/definition" 11 a-uri (Pos 3 14))
            (check-equal? (stdio-result client 11)
                          (->jsexpr (Location a-uri (Range (Pos 2 8) (Pos 2 13)))))
            (check-false (file-exists? (build-path root "release")))
            (write-text (build-path root "release") "")
            (stdio-send! client "textDocument/semanticTokens/full"
                         (json {#:textDocument {#:uri a-uri}}) 12)
            (stdio-result client 12)

            (define-values (shutdown-res exit-code) (stdio-shutdown-and-exit! client 13))
            (check-equal? shutdown-res (json null))
            (check-equal? exit-code 0)
            (check-false (hash-ref diagnostics b-uri #f)))))
      (lambda () (delete-directory/files root)))))
