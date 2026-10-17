#lang racket/base

(require json
         rackunit
         "../lsp/msg-io.rkt")

(module+ test
  (test-case "writes complete UTF-8 frames before returning"
    (define out (open-output-bytes))
    (define messages (list (hasheq 'result "你好") (hasheq 'result "ok")))
    (define expected (open-output-bytes))
    (for ([message (in-list messages)])
      (define body (jsexpr->bytes message))
      (fprintf expected "Content-Length: ~a\r\n\r\n" (bytes-length body))
      (write-bytes body expected)
      (check-pred void? (display-message/flush message out))
      (check-equal? (get-output-bytes out) (get-output-bytes expected))))

  (test-case "write failures reach the caller"
    (define out (open-output-bytes))
    (close-output-port out)
    (check-exn exn:fail? (lambda () (display-message/flush (hasheq) out)))))
