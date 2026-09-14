#lang racket/base

(require rackunit
         racket/list
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../common/settings.rkt"
         "../../doclib/formatter/fmt.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/text-document.rkt")

(define racket-uri "file:///lsp-formatters.rkt")
(define racket-text "#lang racket/base\n(define x\n1)")
(define scribble-uri "file:///lsp-formatters.scrbl")
(define scribble-text "#lang scribble/base\n@itemlist[\n@item{one}\n]")
(define options (hasheq 'tabSize 2 'insertSpaces #t))
(define document-params
  (hasheq 'textDocument (hasheq 'uri racket-uri)
          'options options))
(define range-params
  (hasheq 'textDocument (hasheq 'uri scribble-uri)
          'range (hasheq 'start (hasheq 'line 0 'character 0)
                         'end (hasheq 'line 3 'character 0))
          'options options))
(define on-type-params
  (hasheq 'textDocument (hasheq 'uri racket-uri)
          'position (hasheq 'line 2 'character 1)
          'ch "\n"
          'options options))

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
      (Formatting-Settings 'fmt 'drracket)
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
    "scribble document formatting uses DrRacket when fmt is selected"
    (with-open-document
      scribble-uri
      scribble-text
      (Formatting-Settings 'fmt 'fixw)
      (lambda ()
        (parameterize ([current-fmt-runner
                        (lambda (_arguments _text)
                          (error 'test "fmt must not be run"))])
          (define response
            (formatting! 5 (hasheq 'textDocument (hasheq 'uri scribble-uri)
                                   'options options)))
          (check-equal? (hash-ref response 'result)
                        (list (hasheq 'range
                                      (hasheq 'start (hasheq 'line 2 'character 0)
                                              'end (hasheq 'line 2 'character 0))
                                      'newText " ")
                              (hasheq 'range
                                      (hasheq 'start (hasheq 'line 3 'character 0)
                                              'end (hasheq 'line 3 'character 0))
                                      'newText " ")))))))

  (test-case
    "range formatting uses only the indentation formatter"
    (with-open-document
      scribble-uri
      scribble-text
      (Formatting-Settings 'fmt 'drracket)
      (lambda ()
        ;; Scribble is not an s-expression language, so either formatter
        ;; selection reaches DrRacket. The focused edits still include the
        ;; end line of the requested range.
        (define range-response (range-formatting! 2 range-params))
        (check-equal? (hash-ref range-response 'result)
                      (list (hasheq 'range
                                    (hasheq 'start (hasheq 'line 2 'character 0)
                                            'end (hasheq 'line 2 'character 0))
                                    'newText " ")
                            (hasheq 'range
                                    (hasheq 'start (hasheq 'line 3 'character 0)
                                            'end (hasheq 'line 3 'character 0))
                                    'newText " "))))))

  (test-case
    "on-type formatting uses only the indentation formatter"
    (with-open-document
      racket-uri
      racket-text
      (Formatting-Settings 'fmt 'drracket)
      (lambda ()
        (parameterize ([current-fmt-runner
                        (lambda (_arguments _text)
                          (error 'test "fmt must not be run"))])
          (define response (on-type-formatting! 3 on-type-params))
          (check-equal?
            (hash-ref (first (hash-ref response 'result)) 'newText)
            "  ")))))

  (test-case
    "missing selected fmt is reported without fallback"
    (with-open-document
      racket-uri
      racket-text
      (Formatting-Settings 'fmt 'fixw)
      (lambda ()
        (parameterize ([current-fmt-runner
                        (lambda (_arguments _text)
                          (values 1 "" "raco: Unrecognized command: fmt\n"))])
          (define response (formatting! 4 document-params))
          (define error-result (hash-ref response 'error))
          (check-equal? (hash-ref error-result 'code)
                        (->jsexpr ErrorCode-RequestFailed))
          (check-regexp-match
            #rx"raco pkg install fmt"
            (hash-ref error-result 'message))))))

  (test-case
    "fmt command failure is reported as RequestFailed"
    (with-open-document
      racket-uri
      racket-text
      (Formatting-Settings 'fmt 'fixw)
      (lambda ()
        (parameterize ([current-fmt-runner
                        (lambda (_arguments _text)
                          (values 1 "" "pretty-print exploded\n"))])
          (define response (formatting! 6 document-params))
          (define error-result (hash-ref response 'error))
          (check-equal? (hash-ref error-result 'code)
                        (->jsexpr ErrorCode-RequestFailed))
          (check-regexp-match
            #rx"raco fmt command failed"
            (hash-ref error-result 'message)))))))
