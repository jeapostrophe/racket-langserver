#lang racket/base

(require rackunit
         "../../doclib/doc-trace.rkt"
         "../../doclib/editor.rkt"
         "../../doclib/lexer.rkt"
         "../../doclib/service/hover/service.rkt"
         "../../doclib/service/tooltip-log.rkt"
         drracket/check-syntax
         racket/class
         racket/string)

(define add-mouse-over-status
  (generic syncheck-annotations<%>
           syncheck:add-mouse-over-status))

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
    (define-values (start end annotation)
      (send (send trace get-hover) annotation-at 2))
    (check-equal? (list start end annotation)
                  (list 1
                        4
                        (Hover-Annotation 'log-tooltip
                                          "Logged information"))))

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
    (define-values (_hover-start _hover-end annotation)
      (send (send trace get-hover) annotation-at 2))
    (check-false annotation)
    (define-values (type-start type-end type-text)
      (send (send trace get-typed-racket) inferred-type-at 2))
    (check-equal? (list type-start type-end type-text)
                  (list 1 4 "Integer")))

  (test-case
    "Narrower annotations win regardless of collection order"
    (define text
      "0123456789")
    (define uri
      "file:///tmp/tooltip-log-test.rkt")
    (define (make-trace)
      (define doc-text
        (new lsp-editor%))
      (send doc-text insert text 0)
      (new build-trace%
        [src source]
        [doc-text doc-text]
        [lexer-state (build-lexer-state text uri)]))

    (define narrow-first-trace
      (make-trace))
    (send-generic narrow-first-trace
                  add-mouse-over-status
                  source
                  2
                  5
                  "narrow first")
    (send narrow-first-trace add-log-tooltip source 0 8 "wide later")
    (define narrow-first
      (send narrow-first-trace get-hover))
    (define-values (nf-start nf-end nf-annotation)
      (send narrow-first annotation-at 3))
    (check-equal? (list nf-start nf-end nf-annotation)
                  (list 2
                        5
                        (Hover-Annotation 'mouse-over-status
                                          "narrow first")))

    (define narrow-later-trace
      (make-trace))
    (send-generic narrow-later-trace
                  add-mouse-over-status
                  source
                  0
                  8
                  "wide first")
    (send narrow-later-trace add-log-tooltip source 2 5 "narrow later")
    (define narrow-later
      (send narrow-later-trace get-hover))
    (define-values (nl-start nl-end nl-annotation)
      (send narrow-later annotation-at 3))
    (check-equal? (list nl-start nl-end nl-annotation)
                  (list 2
                        5
                        (Hover-Annotation 'log-tooltip
                                          "narrow later"))))

  (test-case
    "Equal-width annotations use later collection order"
    (define text
      "0123456789")
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
    (send-generic trace
                  add-mouse-over-status
                  source
                  0
                  5
                  "earlier")
    (send trace add-log-tooltip source 2 7 "later")
    (define hover
      (send trace get-hover))

    (define-values (left-start left-end left-annotation)
      (send hover annotation-at 1))
    (check-equal? (list left-start left-end left-annotation)
                  (list 0
                        2
                        (Hover-Annotation 'mouse-over-status "earlier")))
    (define-values (overlap-start overlap-end overlap-annotation)
      (send hover annotation-at 3))
    (check-equal? (list overlap-start overlap-end overlap-annotation)
                  (list 2
                        5
                        (Hover-Annotation 'log-tooltip "later")))))
