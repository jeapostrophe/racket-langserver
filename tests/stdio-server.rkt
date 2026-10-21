#lang racket/base

;; Real stdio tests need strict framing: the production reader skips stray lines.
(require json
         racket/runtime-path)

(provide read-frame call-with-server-process)

(define-runtime-path main-path "../main.rkt")

(define (read-frame in)
  (define header (read-line in 'return-linefeed))
  (cond
    [(eof-object? header) eof]
    [else
     (define found (regexp-match #rx"^Content-Length: ([0-9]+)$" header))
     (unless found (error 'read-frame "unexpected protocol header: ~s" header))
     (unless (equal? (read-line in 'return-linefeed) "")
       (error 'read-frame "missing header separator"))
     (define size (string->number (cadr found)))
     (define body (read-bytes size in))
     (unless (and (bytes? body) (= (bytes-length body) size))
       (error 'read-frame "incomplete protocol body"))
     (bytes->jsexpr body)]))

;; The callback owns normal shutdown and reader joins. Cleanup also handles a
;; failed assertion; supplied stderr ports remain owned by the caller.
(define (call-with-server-process proc
                                  #:arguments [arguments '()]
                                  #:environment [environment (current-environment-variables)]
                                  #:stderr [error-port #f])
  (define readers (make-custodian))
  (define-values (server stdout stdin stderr)
    (parameterize ([current-environment-variables environment])
      (apply subprocess #f #f error-port (find-executable-path "racket")
             (append arguments (list "-t" (path->string main-path))))))
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([current-custodian readers])
        (proc server stdout stdin stderr)))
    (lambda ()
      (subprocess-kill server #t)
      (subprocess-wait server)
      (custodian-shutdown-all readers)
      (close-output-port stdin)
      (close-input-port stdout)
      (when stderr (close-input-port stderr)))))
