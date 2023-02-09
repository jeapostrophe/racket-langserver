#lang racket/base
(require chk
         json
         racket/os
         "../../msg-io.rkt")

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

(define ((forward-errors in))
  (for ([str (in-port read-line in)])
    (displayln str (current-error-port))))

(module+ test
  (define racket-path (find-executable-path "racket"))
  (define-values (sp stdout stdin stderr)
    (subprocess #f #f #f racket-path "-t" "../../main.rkt"))
  (define _err-thd (thread (forward-errors stderr)))

  ;; Initialize request
  (display-message/flush init-req stdin)
  (let ([resp (read-message stdout)])
    (chk #:= resp (read-json (open-input-file "init_resp.json"))))

  ;; Shutdown request
  (display-message/flush shutdown-req stdin)
  (let ([resp (read-message stdout)])
    (chk #:= resp (hasheq 'id (hash-ref shutdown-req 'id)
                          'jsonrpc "2.0"
                          'result (json-null))))

  ;; Exit
  (display-message/flush exit-notf stdin)
  (subprocess-wait sp)
  (define st (subprocess-status sp))
  (chk (zero? st)))
