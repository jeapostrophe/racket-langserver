#lang racket/base
(require racket/os)

(define init-req
  (hasheq 'jsonrpc "2.0"
          'id 0
          'method "initialize"
          'params (hasheq 'processId (getpid)
                          'rootPath "/home/conor/racket-langserver/"
                          'rootUri "file:///home/conor/racket-langserver/"
                          'capabilities (hasheq))))

(define shutdown-req
  (hasheq 'jsonrpc "2.0"
          'id 1
          'method "shutdown"))

(define exit-notf
  (hasheq 'jsonrpc "2.0"
          'method "exit"))

(module+ test
  (require rackunit
           json
           racket/port
           "../stdio-server.rkt"
           compiler/module-suffix
           "../../lsp/msg-io.rkt"
           "../../common/json-util.rkt"
           "../../common/interfaces.rkt")

  (call-with-server-process
    (lambda (sp stdout stdin stderr)
      (thread (lambda () (copy-port stderr (current-error-port))))

      ;; Initialize request
      (display-message/flush init-req stdin)
      (let ([resp (read-frame stdout)])
        (define expected-json (read-json (open-input-file "init_resp.json")))
        (define json (jsexpr-set expected-json '(result capabilities semanticTokensProvider legend)
                                 (hasheq 'tokenModifiers (map ->jsexpr *semantic-token-modifiers*)
                                         'tokenTypes (map ->jsexpr *semantic-token-types*))))
        (set! json (jsexpr-set json '(result capabilities workspace fileOperations didRename filters)
                               (map (lambda (ext)
                                      (hasheq 'scheme "file" 'pattern (hasheq 'glob (format "**/*.~a" ext))))
                                    (get-module-suffixes))))
        (check-equal? (jsexpr->string resp)
                      (jsexpr->string json)))

      ;; Shutdown request
      (display-message/flush shutdown-req stdin)
      (let ([resp (read-frame stdout)])
        (check-equal? (jsexpr->string resp)
                      (jsexpr->string (hasheq 'id (hash-ref shutdown-req 'id)
                                              'jsonrpc "2.0"
                                              'result (json-null)))))

      ;; Exit
      (display-message/flush exit-notf stdin)
      (check-not-false (sync/timeout 20 sp))
      (define st (subprocess-status sp))
      (check-true (zero? st)))))
