#lang racket/base
(require chk
         delimit-app/main
         "../text-document.rkt")

(define (test-did-open)
  (define params
    {'textDocument {'uri "/tmp/blank"
                    'languageId "racket"
                    'version 1
                    'text "I am a banana!\r\nOne!\nTwo!!"}})
  (chk (did-open (hash) params)
       (hash (cons "/tmp/blank" 1) '("I am a banana!" "One!" "Two!!"))))

(define (test-did-change)
  (define params
    {'textDocument {'uri "/tmp/blank"
                    'version 1}
     'contentChanges [{'text "Some more text"}
                      {'text "More text again\r\nNew line!"}]})
  (chk (did-change (hash (cons "/tmp/blank" 1) '("default" "text")) params)
       (hash (cons "/tmp/blank" 1) '("More text again" "New line!"))))

(define (test-string->lines)
  (chk (string->lines "a\nb") '("a" "b"))
  (chk (string->lines "a\rb") '("a" "b"))
  (chk (string->lines "a\r\nb") '("a" "b")))

(define (test-range-edit)
  (define test-lines '("line one" "line two" "line three" "line four" "line five"))
  (chk (range-edit/unicode test-lines 1 5 2 4 "TWO\nLINE")
       '("line one" "line TWO" "LINE three" "line four" "line five"))
  (chk (range-edit/unicode test-lines 0 5 1 4 "ONE\nLINE")
       '("line ONE" "LINE two" "line three" "line four" "line five"))
  (chk (range-edit/unicode test-lines 1 5 3 4 "TWO\nLINE THREE\nLINE")
       '("line one" "line TWO" "LINE THREE" "LINE four" "line five"))
  (chk (range-edit/unicode test-lines 0 0 1 0 "LINE ONE\n")
       '("LINE ONE" "line two" "line three" "line four" "line five"))
  (chk (range-edit/unicode '("line one") 0 5 0 8 "ONE")
       '("line ONE"))
  ;; unicode specific stuff
  (define mb-utf-16 (bytes->string/utf-8 #"\360\220\220\200"))
  (chk (range-edit/unicode `("abc" ,(string-append "ab" mb-utf-16 "cd") "asdf")
                           1 1 1 5 "NEW")
       '("abc" "aNEWd" "asdf"))
  ;; This is an invalid request, as the 'start-char' index is in the middle of
  ;; the multi-code-point UTF-16 character. It is expected to error due to an
  ;; invalid unicode conversion.
  (chk #:x (range-edit/unicode (list (string-append "ab" mb-utf-16 "cd"))
                               0 3 0 5 "NEW")
       exn:fail?)
  )

(module+ test
  (test-did-open)
  (test-did-change)
  (test-string->lines)
  (test-range-edit)
  )