#lang racket/base

(require "../silent-logger.rkt"
         "../json-dsl.rkt"
         rackunit
         racket/list
         racket/match
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../common/settings.rkt"
         "../../doclib/formatter/fmt.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/text-document.rkt")

(define racket-uri "file:///lsp-formatters.rkt")
(define racket-text "#lang racket/base\n(define x\n1)")
(define options (json {#:tabSize 2 #:insertSpaces #t}))
(define standard-racket-indentation?
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (procedure? (dynamic-require 'syntax-color/racket-indentation
                                 'racket-amount-to-indent))))

(define document-params
  (json {#:textDocument {#:uri racket-uri} #:options options}))
(define range-params
  (json {#:textDocument {#:uri racket-uri}
         #:range (->jsexpr (Range (Pos 1 0) (Pos 2 0)))
         #:options options}))
(define on-type-params
  (json {#:textDocument {#:uri racket-uri}
         #:position (->jsexpr (Pos 2 1))
         #:ch "\n"
         #:options options}))

(define (with-open-document uri text settings proc)
  (lsp-open-doc! uri text 0)
  (set-formatting-settings! settings)
  (dynamic-wind
    void
    proc
    (lambda ()
      (lsp-close-doc! uri)
      (set-formatting-settings! default-formatting-settings))))

(module+ test
  (test-case
    "document formatting uses only the document formatter"
    (with-open-document
      racket-uri
      racket-text
      (Formatting-Settings 'fmt 'drracket empty-fmt-settings)
      (lambda ()
        (parameterize ([current-fmt-runner
                        (lambda (_arguments _text)
                          (values 0
                                  "#lang racket/base\n(define\n  x\n  1)\n"
                                  ""))])
          (define document-response (formatting! 1 document-params))
          (check-equal?
            (hash-ref (first (hash-ref document-response 'result)) 'newText)
            "#lang racket/base\n(define\n  x\n  1)\n")))))

  (test-case
    "range formatting uses only the indentation formatter"
    (with-open-document
      racket-uri
      racket-text
      (Formatting-Settings 'fmt 'fixw empty-fmt-settings)
      (lambda ()
        (parameterize ([current-fmt-runner
                        (lambda (_arguments _text)
                          (error 'test "fmt must not be run"))])
          ;; The end line is included even when its requested character is zero.
          (define range-response (range-formatting! 2 range-params))
          (check-equal? (hash-ref range-response 'result)
                        (list (hasheq 'range
                                      (hasheq 'start (hasheq 'line 2 'character 0)
                                              'end (hasheq 'line 2 'character 2))
                                      'newText "  1)")))))))

  (when standard-racket-indentation?
    (test-case
      "on-type formatting uses only the indentation formatter"
      (with-open-document
        racket-uri
        racket-text
        (Formatting-Settings 'fmt 'drracket empty-fmt-settings)
        (lambda ()
          (parameterize ([current-fmt-runner
                          (lambda (_arguments _text)
                            (error 'test "fmt must not be run"))])
            (define response (on-type-formatting! 3 on-type-params))
            (check-equal?
              (hash-ref (first (hash-ref response 'result)) 'newText)
              "  "))))))

  (for ([spec (in-list
                (list
                  (list "missing selected fmt is reported without fallback"
                        4
                        "raco: Unrecognized command: fmt\n"
                        #rx"raco pkg install fmt")
                  (list "fmt command failure is reported as RequestFailed"
                        6
                        "pretty-print exploded\n"
                        #rx"raco fmt command failed")))])
    (match-define (list title request-id stderr-text expected-rx) spec)
    (test-case title
      (with-open-document
        racket-uri
        racket-text
        (Formatting-Settings 'fmt 'fixw empty-fmt-settings)
        (lambda ()
          (parameterize ([current-fmt-runner
                          (lambda (_arguments _text) (values 1 "" stderr-text))])
            (define error-result (hash-ref (formatting! request-id document-params) 'error))
            (check-equal? (hash-ref error-result 'code) (->jsexpr ErrorCode-RequestFailed))
            (check-regexp-match expected-rx (hash-ref error-result 'message))))))))
