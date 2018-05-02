#lang br
(require br/indent
         framework
         racket/contract/base
         rackunit)

;; Procedure to be used by lang-server
(define (format text
                #:start [start 0]
                #:end [end (string-length text)]
                #:tab-size [tab-size 2])
  (define (str->text str)
    (define t (new racket:text%))
    (send t insert-port (open-input-string str))
    t)
  (define t (str->text text))
  (define start-line (line t start))
  (define end-line (line t end))
  (define start-pos (line-start t start-line))
  (define end-pos (add1 (line-end t end-line)))
  (define delta 1)
  (define indented-t
    (for/fold ([t-acc t])
        ([line-idx (in-range start-line (add1 end-line))])
      ;; simulate DrR indentation
      ;; by dropping leading spaces and applying new indent.
      (define line-start-pos (line-start t-acc line-idx))
      ;; add1 to grab ending newline too
      (define line-end-pos (add1 (line-end t-acc line-idx)))
      (define new-indent (or (send t-acc compute-amount-to-indent line-start-pos) 0))
      (define new-line-str
        (list->string (append (make-list new-indent #\space)
                              (dropf (line-chars t-acc line-idx) space-char?))))
      (send t-acc delete line-start-pos line-end-pos)
      (send t-acc insert new-line-str line-start-pos)
      ;; compute delta
      (define line-length (- line-end-pos line-start-pos))
      (define new-line-length (string-length new-line-str))
      (set! delta (+ delta (- new-line-length line-length)))
      t-acc))
  ;; Drop \0 from end
  (define string-\0 (send indented-t get-text start-pos (+ end-pos delta)))
  (define string (substring string-\0 0 (sub1 (string-length string-\0))))
  (list string start-pos end-pos))


(provide (contract-out
          [format (->* (string?) (#:start exact-nonnegative-integer?
                                  #:end exact-nonnegative-integer?
                                  #:tab-size exact-nonnegative-integer?)
                       any)]))

(module+ test
  (define test-input "(a\n(b\n(c\n(d\n))))\n\n")
  (check-equal? (format test-input)
                '("(a\n (b\n  (c\n   (d\n    ))))\n\n" 0 19))
  (check-equal? (format test-input #:start 6)
                '(" (c\n  (d\n   ))))\n\n" 6 19))
  (check-equal? (format test-input #:start 7)
                '(" (c\n  (d\n   ))))\n\n" 6 19))
  (check-equal? (format test-input #:end 6)
                '("(a\n (b\n  (c\n" 0 9))
  (check-equal? (format test-input #:end 7)
                '("(a\n (b\n  (c\n" 0 9))
  (check-equal? (format test-input #:start 3 #:end 7)
                '(" (b\n  (c\n" 3 9)))
