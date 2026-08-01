#lang racket/base

(require rackunit
         "../../doclib/doc-trace.rkt"
         "../../doclib/editor.rkt"
         "../../doclib/lexer.rkt"
         "../../doclib/service/tooltip-log.rkt"
         racket/class
         racket/string)

(define source
  (string->path "/tmp/tooltip-log-test.rkt"))
(define foreign-source
  (string->path "/tmp/tooltip-log-foreign.rkt"))

(define (located-at src start end datum)
  (datum->syntax #f
                 datum
                 (list src 1 0 (add1 start) (- end start))))

(define (tooltip-leaf src start end text)
  (vector (located-at src start end 'value)
          start
          end
          text))

(define (online-log message property)
  (define carrier
    (syntax-property
      (located-at source 0 5 'carrier)
      'mouse-over-tooltips
      property))
  (vector 'info message (list carrier) 'online-check-syntax))

(module+ test
  (test-case
    "Generic online logs decode arbitrary pair trees in order"
    (define log
      (online-log
        "provider tooltip"
        (cons (tooltip-leaf source 0 1 "First")
              (cons 'ignored
                    (tooltip-leaf source 2 3 "Second")))))
    (check-true (online-tooltip-log? log))
    (check-equal? (tooltip-log-kind (online-tooltip-log-message log))
                  'mouse-over)
    (check-equal?
      (online-tooltip-log-tooltips log source)
      (list (Tooltip source 0 1 "First")
            (Tooltip source 2 3 "Second"))))

  (test-case
    "Foreign sources are rejected before forcing text"
    (define forced? #f)
    (define log
      (online-log
        "provider tooltip"
        (tooltip-leaf foreign-source
                      0
                      1
                      (lambda ()
                        (set! forced? #t)
                        "Foreign"))))
    (check-equal? (online-tooltip-log-tooltips log source) '())
    (check-false forced?))

  (test-case
    "A failing leaf does not suppress its valid sibling"
    (define log
      (online-log
        "provider tooltip"
        (cons (tooltip-leaf source
                            0
                            1
                            (lambda ()
                              (error 'tooltip "provider failure")))
              (tooltip-leaf source 2 3 "Survived"))))
    (define err
      (open-output-string))
    (define tooltips
      (parameterize ([current-error-port err])
        (online-tooltip-log-tooltips log source)))
    (check-equal? tooltips
                  (list (Tooltip source 2 3 "Survived")))
    (check-true
      (string-contains? (get-output-string err) "provider failure")))

  (test-case
    "build-trace forwards generic logged tooltips to hover"
    (define text
      "abcde")
    (define uri
      "file:///tmp/tooltip-log-test.rkt")
    (define doc-text
      (new lsp-editor%))
    (send doc-text insert text 0)
    (define trace
      (new build-trace%
        [src source]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text uri)]))
    (send trace
          walk-log
          (list (online-log
                  "provider tooltip"
                  (tooltip-leaf source 1 4 "Logged information"))))
    (define-values (start end hover-text)
      (send (send trace get-hover) mouse-over-at 2))
    (check-equal? (list start end hover-text)
                  (list 1 4 "Logged information")))

  (test-case
    "build-trace routes recognized Typed Racket tooltips exclusively"
    (define text
      "abcde")
    (define uri
      "file:///tmp/tooltip-log-test.rkt")
    (define doc-text
      (new lsp-editor%))
    (send doc-text insert text 0)
    (define trace
      (new build-trace%
        [src source]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text uri)]))
    (send trace
          walk-log
          (list (online-log
                  typed-racket-inferred-type-message
                  (tooltip-leaf source 1 4 "Integer"))))
    (define-values (_hover-start _hover-end hover-text)
      (send (send trace get-hover) mouse-over-at 2))
    (check-false hover-text)
    (define-values (type-start type-end type-text)
      (send (send trace get-typed-racket) inferred-type-at 2))
    (check-equal? (list type-start type-end type-text)
                  (list 1 4 "Integer"))))
