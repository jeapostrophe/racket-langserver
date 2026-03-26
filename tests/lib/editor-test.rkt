#lang racket

(require rackunit
         racket/class
         racket/sandbox
         "../../doclib/editor.rkt")

(define (make-editor [text ""])
  (define editor (new lsp-editor%))
  (send editor insert text 0)
  editor)

(define (vector-lower-bound sorted-vector target)
  (let loop ([low 0]
             [high (vector-length sorted-vector)])
    (if (= low high)
        low
        (let ([mid-index (quotient (+ low high) 2)])
          (if (< (vector-ref sorted-vector mid-index) target)
              (loop (add1 mid-index) high)
              (loop low mid-index))))))

(define (string-newline-offsets text)
  (list->vector
    (for/list ([character (in-string text)]
               [offset (in-naturals)]
               #:when (char=? character #\newline))
      offset)))

(define (string-line-count newline-offsets)
  (add1 (vector-length newline-offsets)))

(define (string-line-start-pos newline-offsets line text-length)
  (cond
    [(zero? line) 0]
    [(>= line (string-line-count newline-offsets)) text-length]
    [else (add1 (vector-ref newline-offsets (sub1 line)))]))

(define (string-line-end-pos newline-offsets line text-length)
  (if (< line (vector-length newline-offsets))
      (vector-ref newline-offsets line)
      text-length))

(define (string-offset->line/char newline-offsets offset)
  (define line (vector-lower-bound newline-offsets offset))
  (define line-start
    (if (zero? line)
        0
        (add1 (vector-ref newline-offsets (sub1 line)))))
  (list line (- offset line-start)))

(define (sample-ranges text-length)
  (remove-duplicates
    (list (list 0 text-length)
          (list 0 (min text-length 3))
          (list (max 0 (- text-length 3)) text-length)
          (let ([midpoint (quotient text-length 2)])
            (list (max 0 (- midpoint 2))
                  (min text-length (+ midpoint 2)))))))

(define replacement-characters
  (vector #\a #\b #\c #\x #\y #\z #\0 #\1 #\2 #\space #\return #\newline))

(define (make-random-replacement max-length)
  (define replacement-length (random (add1 max-length)))
  (build-string replacement-length
                (lambda (_)
                  (vector-ref replacement-characters
                              (random (vector-length replacement-characters))))))

(define (check-line-span editor line start end)
  (check-equal? (send editor line-start-pos line)
                start)
  (check-equal? (send editor line-end-pos line)
                end))

(define (check-offset-roundtrip editor expected-line/chars)
  (for ([expected-line/char (in-list expected-line/chars)]
        [offset (in-naturals)])
    (check-equal? (send editor pos->line/char offset)
                  expected-line/char)
    (check-equal? (send editor line/char->pos
                        (first expected-line/char)
                        (second expected-line/char))
                  offset)
    (check-equal? (send editor at-line offset)
                  (first expected-line/char))))

(define (check-editor-agrees-with-string-model editor expected-text)
  (define text-length (string-length expected-text))
  (define newline-offsets (string-newline-offsets expected-text))

  (check-equal? (send editor get-text)
                expected-text)
  (check-equal? (send editor end-pos)
                text-length)
  (check-equal? (for/list ([offset (in-range text-length)])
                  (send editor get-char offset))
                (string->list expected-text))

  (for ([offset (in-range (add1 text-length))])
    (define expected-line/char
      (string-offset->line/char newline-offsets offset))
    (check-equal? (send editor pos->line/char offset)
                  expected-line/char)
    (check-equal? (send editor line/char->pos
                        (first expected-line/char)
                        (second expected-line/char))
                  offset)
    (check-equal? (send editor at-line offset)
                  (first expected-line/char)))

  (for ([line (in-range (+ (string-line-count newline-offsets) 2))])
    (check-equal? (send editor line-start-pos line)
                  (string-line-start-pos newline-offsets line text-length))
    (check-equal? (send editor line-end-pos line)
                  (string-line-end-pos newline-offsets line text-length)))

  (for ([sample-range (in-list (sample-ranges text-length))])
    (match-define (list start end) sample-range)
    (check-equal? (send editor get-text start end)
                  (substring expected-text start end))))

(define (replace-line/char-range editor start-line start-character end-line end-character str)
  (send editor replace str
        (send editor line/char->pos start-line start-character)
        (send editor line/char->pos end-line end-character)))

(module+ test
  (test-case "editor exposes slice and EOF helpers"
    (define editor (make-editor "abc\ndef\n"))

    (check-line-span editor 0 0 3)
    (check-line-span editor 1 4 7)
    (check-equal? (send editor line-start-pos 10)
                  (send editor end-pos))
    (check-equal? (send editor line-end-pos 10)
                  (send editor end-pos))
    (check-equal? (send editor end-pos) 8)
    (check-equal? (send editor get-text 4 7) "def")
    (check-equal? (send editor get-line 0) "abc")
    (check-equal? (send editor get-text) "abc\ndef\n"))

  (test-case "editor supports line-based text access and edits"
    (define editor (make-editor "abc\ndef\n"))

    (check-equal? (send editor line/char->pos 1 0) 4)
    (check-equal? (send editor pos->line/char 4) '(1 0))
    (check-equal? (send editor line-start-pos 1) 4)
    (check-equal? (send editor line-end-pos 0) 3)
    (check-equal? (send editor get-line 0) "abc")

    (send editor replace-in-line "ZZ" 1 1 3)

    (check-equal? (send editor get-text) "abc\ndZZ\n")
    (check-equal? (send editor get-line 1) "dZZ")
    (check-equal? (send editor pos->line/char (send editor end-pos)) '(2 0)))

  (test-case "editor round-trips positions across CRLF boundaries"
    (define editor (make-editor "a\r\nbc\n"))

    (check-offset-roundtrip editor
                            (list '(0 0)
                                  '(0 1)
                                  '(0 2)
                                  '(1 0)
                                  '(1 1)
                                  '(1 2)
                                  '(2 0)))
    (check-line-span editor 0 0 2)
    (check-line-span editor 1 3 5)
    (check-line-span editor 2 6 6)
    (check-equal? (send editor get-char 1) #\return)
    (check-equal? (send editor get-char 2) #\newline)
    (check-equal? (send editor get-text 0 3) "a\r\n")
    (check-equal? (send editor get-text) "a\r\nbc\n"))

  (test-case "editor edits CRLF content without normalization"
    (define editor-1 (make-editor "1\r\n2\r\n"))
    (replace-line/char-range editor-1 1 0 1 1 "\r\n")
    (check-equal? (send editor-1 get-text) "1\r\n\r\n\r\n")

    (define editor-2 (make-editor "\r\n\r\n"))
    (replace-line/char-range editor-2 1 1 1 1 "text")
    (check-equal? (send editor-2 get-text) "\r\n\rtext\n"))

  (test-case "editor handles multi-step multiline edits"
    (define editor (make-editor "1\n2\n3\n4\n\n"))

    (replace-line/char-range editor 1 0 3 0 "a")
    (check-equal? (send editor get-text) "1\na4\n\n")

    (replace-line/char-range editor 0 0 0 1 "0\n2\n")
    (check-equal? (send editor get-text) "0\n2\n\na4\n\n")

    (replace-line/char-range editor 1 1 3 1 "abcd")
    (check-equal? (send editor get-text) "0\n2abcd4\n\n")

    (replace-line/char-range editor 0 0 0 2 "0")
    (check-equal? (send editor get-text) "02abcd4\n\n"))

  (test-case "editor delete updates newline bookkeeping"
    (define editor (make-editor "ab\ncd\nef\n"))
    (send editor delete
          (send editor line/char->pos 0 2)
          (send editor line/char->pos 2 1))

    (check-equal? (send editor get-text) "abf\n")
    (check-line-span editor 0 0 3)
    (check-line-span editor 1 4 4)
    (check-equal? (send editor line-start-pos 2)
                  (send editor end-pos))
    (check-equal? (send editor pos->line/char 4)
                  '(1 0))
    (check-equal? (send editor line/char->pos 1 0)
                  4)
    (check-equal? (send editor get-text 0 3)
                  "abf"))

  (test-case "editor clamps line lookups at EOF"
    (define editor (make-editor "abc\n"))

    (define eof (send editor end-pos))
    (check-equal? eof 4)
    (check-equal? (send editor line-start-pos 2) eof)
    (check-equal? (send editor line-end-pos 2) eof)
    (check-equal? (send editor at-line eof) 1))

  (test-case "editor copy keeps an immutable snapshot"
    (define editor (make-editor "abc\n"))

    (define editor-copy (send editor copy))
    (send editor replace "z" 0 1)

    (check-equal? (send editor get-text) "zbc\n")
    (check-equal? (send editor-copy get-text) "abc\n"))

  (test-case "editor stays aligned with the string model across deterministic edit sequences"
    (for ([seed (in-list '(17 203 4409))])
      (define generator (make-pseudo-random-generator))
      (parameterize ([current-pseudo-random-generator generator])
        (random-seed seed)

        (define initial-text "alpha\r\nbeta\n\ngamma\n")
        (define model-text initial-text)
        (define editor (make-editor initial-text))

        (for ([_ (in-range 80)])
          (define model-length (string-length model-text))
          (define start-offset (random (add1 model-length)))
          (define end-offset (+ start-offset (random (add1 (- model-length start-offset)))))
          (define replacement (make-random-replacement 12))

          (send editor replace replacement start-offset end-offset)
          (set! model-text
                (string-append (substring model-text 0 start-offset)
                               replacement
                               (substring model-text end-offset)))

          (check-editor-agrees-with-string-model editor model-text)))))

  ;; Practical performance tests ;;

  (define practical-line-count 2000)
  (define practical-line-width 40)
  ;; 0.001 time budget is enough for recent laptops, 0.01 for CI environments in parallel tests
  (define practical-time-budget 0.01)
  (define practical-memory-budget 0.01)

  (define practical-top-insert-line 5)
  (define practical-top-insert-column 10)
  (define practical-middle-line 1000)
  (define practical-middle-column 20)
  (define practical-multiline-start-line 999)
  (define practical-multiline-start-column 5)
  (define practical-multiline-end-line 1002)
  (define practical-multiline-end-column 10)
  (define practical-multiline-replacement
    "hello\nworld\n")

  (define practical-source-text
    (string-append*
      (for/list ([line (in-range practical-line-count)])
        (string-append
          (build-string practical-line-width
                        (lambda (column)
                          (if (= (modulo (+ line column 1) 31) 0)
                              #\return
                              (integer->char
                                (+ (char->integer #\a)
                                   (modulo (+ line column) 26))))))
          "\n"))))

  (define (check-performance #:seconds seconds #:megabytes megabytes thunk)
    (define repetitions 10)
    (collect-garbage)
    (thunk)
    (with-limits (* seconds repetitions)
                 megabytes
      (for ([_ (in-range repetitions)])
        (thunk))))

  (test-case "performance spec: build practical editor"
    (check-performance
      #:seconds practical-time-budget
      #:megabytes practical-memory-budget
      (lambda ()
        (void (make-editor practical-source-text)))))

  (test-case "performance spec: single top-of-file insert"
    (define editor (make-editor practical-source-text))
    (define insert-pos
      (send editor line/char->pos practical-top-insert-line practical-top-insert-column))
    (check-performance
      #:seconds practical-time-budget
      #:megabytes practical-memory-budget
      (lambda ()
        (send editor replace "x" insert-pos insert-pos)
        (send editor delete insert-pos (add1 insert-pos)))))

  (test-case "performance spec: single in-line replacement"
    (define editor (make-editor practical-source-text))
    (define replace-pos
      (send editor line/char->pos practical-middle-line practical-middle-column))
    (define original-text
      (send editor get-text replace-pos (add1 replace-pos)))
    (check-performance
      #:seconds practical-time-budget
      #:megabytes practical-memory-budget
      (lambda ()
        (send editor replace "x" replace-pos (add1 replace-pos))
        (send editor replace original-text replace-pos (add1 replace-pos)))))

  (test-case "performance spec: small multiline replace"
    (define editor (make-editor practical-source-text))
    (define replace-start
      (send editor line/char->pos practical-multiline-start-line practical-multiline-start-column))
    (define replace-end
      (send editor line/char->pos practical-multiline-end-line practical-multiline-end-column))
    (define original-text
      (send editor get-text replace-start replace-end))
    (define replacement-end
      (+ replace-start (string-length practical-multiline-replacement)))
    (check-performance
      #:seconds practical-time-budget
      #:megabytes practical-memory-budget
      (lambda ()
        (send editor replace practical-multiline-replacement replace-start replace-end)
        (send editor replace original-text replace-start replacement-end))))

  (test-case "performance spec: typing burst"
    (define editor (make-editor practical-source-text))
    (define insert-pos
      (send editor line/char->pos practical-top-insert-line practical-top-insert-column))
    (check-performance
      #:seconds practical-time-budget
      #:megabytes practical-memory-budget
      (lambda ()
        (for ([_ (in-range 50)])
          (send editor replace "x" insert-pos insert-pos))
        (send editor delete insert-pos (+ insert-pos 50))))))

