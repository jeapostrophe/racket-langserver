#lang racket/base
(require chk
         racket/os
         "../msg-io.rkt")

(define init-msg
  (hasheq 'id 0
          'method "initialize"
          'params (hasheq 'processId (getpid)
                          'rootPath "/home/conor/racket-langserver/"
                          'rootUri "file:///home/conor/racket-langserver/"
                          'capabilities (hasheq))))

(define shutdown-msg
  (hasheq 'id 1
          'method "shutdown"))

(define exit-notf
  (hasheq 'method "exit"))

(define ((forward-errors in))
  (for ([str (in-port read-line in)])
    (displayln str (current-error-port))))

(module+ test
  (define racket-path (find-executable-path "racket"))
  (define-values (sp stdout stdin stderr)
    (subprocess #f #f #f racket-path "-t" "../main.rkt"))
  (define err-thd (thread (forward-errors stderr)))
  ;; Initialize request
  (display-message/flush init-msg stdin)
  (let ([resp (read-message stdout)])
    (chk*
     (chk #:= (hash-ref init-msg 'id) (hash-ref resp 'id))
     (chk (not (hash-ref resp 'error #f)))))
  ;; Shutdown request
  (display-message/flush shutdown-msg stdin)
  (let ([resp (read-message stdout)])
    (chk*
     (chk #:= (hash-ref shutdown-msg 'id) (hash-ref resp 'id))
     (chk (not (hash-ref resp 'error #f)))))
  ;; Exit
  (display-message/flush exit-notf stdin)
  (subprocess-wait sp)
  (define st (subprocess-status sp))
  (chk (zero? st)))
