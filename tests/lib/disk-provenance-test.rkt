#lang racket/base

(require rackunit
         racket/file
         racket/string
         "../../common/path-util.rkt"
         (submod "../../lsp/safedoc.rkt" test-support))

(module+ test
  (test-case
    "disk comparison requires exact decoded text, including line endings"
    (for ([texts (in-list '(("" "" #t)
                            ("abc" "abc" #t)
                            ("abc" "ab" #f)
                            ("ab" "abc" #f)
                            ("abc" "abd" #f)
                            ("" "a" #f)
                            ("a" "" #f)
                            ("λ😀\n" "λ😀\n" #t)
                            ("λ😀\r\n" "λ😀\n" #f)))])
      (check-equal?
        (port-matches-text? (open-input-string (car texts)) (cadr texts))
        (caddr texts)))
    ;; Like file->string, character reads decode malformed UTF-8 as U+FFFD.
    (check-true (port-matches-text? (open-input-bytes (bytes 255)) "�")))

  (test-case
    "a longer stream is rejected after at most one extra character"
    (define text "#lang racket/base\n")
    (define consumed 0)
    (define in
      (make-input-port
        'unending-source
        (lambda (buffer)
          (when (> consumed (string-length text))
            (error 'unending-source "comparison exceeded its input bound"))
          (bytes-set! buffer 0
                      (if (< consumed (string-length text))
                          (char->integer (string-ref text consumed))
                          120))
          (set! consumed (add1 consumed))
          1)
        #f void))
    (check-false (port-matches-text? in text))
    (check-equal? consumed (add1 (string-length text))))

  (test-case
    "only file URIs can establish disk provenance"
    (define path (make-temporary-file "disk-provenance~a"))
    (dynamic-wind
      void
      (lambda ()
        (display-to-file "λ😀\n" path #:exists 'truncate)
        (define uri (path->uri path))
        (check-true (file-matches-text? uri "λ😀\n"))
        (for ([scheme '("vscode-remote:" "untitled:" "https:")])
          (check-false (file-matches-text? (string-replace uri "file:" scheme) "λ😀\n")))
        (display-to-file "a\r\n" path #:mode 'binary #:exists 'truncate)
        (check-false (file-matches-text? uri "a\n"))
        (delete-file path)
        (check-false (file-matches-text? uri "λ😀\n")))
      (lambda () (when (file-exists? path) (delete-file path))))))
