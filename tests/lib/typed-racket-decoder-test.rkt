#lang racket/base

;; Classifier and direct-decoder coverage for Typed Racket online-check-syntax
;; logs. These live under tests/ so CI (`raco test -c racket-langserver/tests`)
;; runs them. Chosen over full-document expansion for message matching,
;; property-tree decoding, source filtering, and records that must be ignored
;; without raising.

(require rackunit
         "../../doclib/editor.rkt"
         "../../doclib/service/typed-racket/decoder.rkt"
         "../../doclib/service/typed-racket/service.rkt"
         racket/class
         racket/set
         racket/string)

(define source "/tmp/typed-racket-tooltip-test.rkt")
(define foreign-source "/tmp/other-file.rkt")

(define (online-log message data)
  (vector 'info message data 'online-check-syntax))

(define (inferred-log data)
  (online-log (string-append "online-check-syntax: "
                             typed-racket-inferred-type-message)
              data))

(define (type-error-log data)
  (online-log (string-append "online-check-syntax: "
                             typed-racket-type-error-message)
              data))

(define (located-at src pos span datum)
  (datum->syntax #f
                 datum
                 (list src 1 0 (add1 pos) span)))

(define (tooltip-syntax #:source [src source]
                        #:left [left 0]
                        #:right [right 1]
                        #:text [text "Integer"]
                        #:thunk? [thunk? #f])
  (define located
    (located-at src left (- right left) 'value))
  (syntax-property
    (datum->syntax #f
                   '(void)
                   (list src 1 0 1 5))
    'mouse-over-tooltips
    (vector located
            left
            right
            (if thunk?
                (lambda () text)
                text))))

;; Two endpoint tooltips with the same message, as Typed Racket emits for some
;; compound type-error spans.
(define (pair-tooltip-syntax left-start left-end right-start right-end text
                             #:source [src source])
  (define left-located
    (located-at src left-start (- left-end left-start) 'left))
  (define right-located
    (located-at src right-start (- right-end right-start) 'right))
  (syntax-property
    (datum->syntax #f
                   '(void)
                   (list src 1 0 1 5))
    'mouse-over-tooltips
    (cons (vector left-located left-start left-end text)
          (vector right-located right-start right-end text))))

(define (annotate logs)
  (typed-racket-log-annotations logs source))

(module+ test
  (test-case
    "Inferred-type logs force thunks, skip empty text, and ignore unknown records"
    (define empty-tooltip-syntax
      (syntax-property
        (located-at source 1 3 'value)
        'mouse-over-tooltips
        (vector (located-at source 1 3 'value) 1 4 "")))
    (check-equal?
      (annotate
        (list (inferred-log
                (list (tooltip-syntax #:left 0 #:right 1 #:thunk? #t)
                      (tooltip-syntax #:left 4 #:right 5)
                      empty-tooltip-syntax))
              ;; Unknown records on the same topic are ignored.
              (online-log "online-check-syntax: unfamiliar tooltip record"
                          (list (tooltip-syntax)))
              ;; Recognized records with bad payloads are ignored without raising.
              (type-error-log (list 'not-syntax))))
      (list (Inferred-Type source 0 1 "Integer")
            (Inferred-Type source 4 5 "Integer"))))

  (test-case
    "Type-error logs merge matching endpoint tooltips into one range"
    (check-equal?
      (annotate
        (list (type-error-log
                (list (pair-tooltip-syntax 0 1 4 5 "type mismatch")))))
      (list (Type-Error source 0 5 "type mismatch"))))

  (test-case
    "Type-error logs keep distinct messages as separate diagnostics"
    (define left
      (tooltip-syntax #:left 0 #:right 1 #:text "expected Number"))
    (define right
      (tooltip-syntax #:left 4 #:right 5 #:text "given String"))
    (check-equal?
      (annotate (list (type-error-log (list left right))))
      (list (Type-Error source 0 1 "expected Number")
            (Type-Error source 4 5 "given String"))))

  (test-case
    "Foreign-source tooltip syntax is filtered before forcing its text"
    (define foreign-tooltip-forced? #f)
    (check-equal?
      (annotate
        (list (inferred-log
                (list (tooltip-syntax #:source foreign-source
                                      #:left 0
                                      #:right 3
                                      #:text
                                      (lambda ()
                                        (set! foreign-tooltip-forced? #t)
                                        "Ignored"))))))
      '())
    (check-false foreign-tooltip-forced?))

  (test-case
    "Property pair trees ignore malformed leaves and preserve valid order"
    (define first-tooltip
      (vector (located-at source 0 1 'first) 0 1 "First"))
    (define second-tooltip
      (vector (located-at source 2 1 'second) 2 3 "Second"))
    (define carrier
      (syntax-property
        (located-at source 0 3 'carrier)
        'mouse-over-tooltips
        (cons (cons first-tooltip 'ignored-leaf)
              second-tooltip)))
    (check-equal?
      (annotate (list (inferred-log (list carrier))))
      (list (Inferred-Type source 0 1 "First")
            (Inferred-Type source 2 3 "Second"))))

  (test-case
    "A failing provider thunk reports to stderr and keeps later records"
    (define failing-tooltip
      (tooltip-syntax #:text
                      (lambda ()
                        (error 'tooltip-text "provider failure"))))
    (define err (open-output-string))
    (define result
      (parameterize ([current-error-port err])
        (annotate
          (list (inferred-log (list failing-tooltip))
                (inferred-log
                  (list (tooltip-syntax #:left 4
                                        #:right 5
                                        #:text "Survived")))))))
    (check-equal? result
                  (list (Inferred-Type source 4 5 "Survived")))
    (define err-text (get-output-string err))
    (check-true (string-contains? err-text "inferred-type"))
    (check-true (string-contains? err-text "provider failure")))

  (test-case
    "The service rejects tooltip ranges beyond the document snapshot"
    (define doc-text
      (new lsp-editor%))
    (send doc-text insert "abc" 0)
    (define service
      (new typed-racket%
        [src source]
        [doc-text doc-text]))
    (send service
          walk-log
          (list
            (inferred-log
              (list (tooltip-syntax #:left 0
                                    #:right 1
                                    #:text "Valid")
                    (tooltip-syntax #:left 2
                                    #:right 4
                                    #:text "Outside")))
            (type-error-log
              (list (tooltip-syntax #:left 2
                                    #:right 4
                                    #:text "Outside error")))))
    (define-values (start end text)
      (send service inferred-type-at 0))
    (check-equal? (list start end text)
                  (list 0 1 "Valid"))
    (define-values (_outside-start _outside-end outside-text)
      (send service inferred-type-at 2))
    (check-false outside-text)
    (check-equal? (set-count (send service get-diagnostics)) 0))

  (test-case
    "Non online-check-syntax topics and malformed vectors are ignored"
    (check-equal?
      (annotate
        (list (vector 'info
                      (string-append "online-check-syntax: "
                                     typed-racket-inferred-type-message)
                      (list (tooltip-syntax))
                      'something-else)
              (vector 'info "short")
              (inferred-log #f)
              (inferred-log '())
              (inferred-log (list (tooltip-syntax)))))
      (list (Inferred-Type source 0 1 "Integer"))))

  (test-case
    ;; Invariant: suffix match must classify even when Typed Racket omits the
    ;; "online-check-syntax: " topic prefix.
    "Message matching accepts the Typed Racket suffix without a topic prefix"
    (check-equal?
      (annotate
        (list (online-log typed-racket-type-error-message
                          (list (tooltip-syntax #:text "boom")))))
      (list (Type-Error source 0 1 "boom")))))
