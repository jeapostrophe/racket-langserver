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
       (hash "/tmp/blank" '("I am a banana!" "One!" "Two!!"))))

(define (test-did-close)
  (define params {'textDocument {'uri "/tmp/blank"}})
  (chk (did-close (hash "/tmp/blank" '("placeholder" "text")) params)
       (hash)))

(define (test-did-change)
  (define params
    {'textDocument {'uri "/tmp/blank"
                    'version 1}
     'contentChanges [{'text "Some more text"}
                      {'text "More text again\r\nNew line!"}]})
  (chk (did-change (hash "/tmp/blank" '("default" "text")) params)
       (hash "/tmp/blank" '("More text again" "New line!"))))

(define (test-string->lines)
  (chk (string->lines "a\nb") '("a" "b"))
  (chk (string->lines "a\rb") '("a" "b"))
  (chk (string->lines "a\r\nb") '("a" "b")))

(define (test-range-edit)
  (define test-lines '("line one" "line two" "line three" "line four" "line five"))
  (chk (range-edit test-lines 1 5 2 4 "TWO\nLINE")
       '("line one" "line TWO" "LINE three" "line four" "line five"))
  (chk (range-edit test-lines 0 5 1 4 "ONE\nLINE")
       '("line ONE" "LINE two" "line three" "line four" "line five"))
  (chk (range-edit test-lines 1 5 3 4 "TWO\nLINE THREE\nLINE")
       '("line one" "line TWO" "LINE THREE" "LINE four" "line five"))
  (chk (range-edit test-lines 0 0 1 0 "LINE ONE\n")
       '("LINE ONE" "line two" "line three" "line four" "line five"))
  (chk (range-edit '("line one") 0 5 0 8 "ONE")
       '("line ONE"))
  ;; Unicode specific tests - '𐐀' (Deseret Capital Long Letter I) is used as an
  ;; example of a unicode character which, when encoded as a UTF-16 string,
  ;; requires 2 code points to represent.
  (chk (range-edit '("abc" "wx𐐀yz" "asdf") 1 1 1 5 "NEW")
       '("abc" "wNEWz" "asdf"))
  ;; This is an invalid request, as the 'start-char' index is in the middle of
  ;; the multi-code-point UTF-16 character. It is expected to error due to an
  ;; invalid unicode conversion.
  (chk #:x (range-edit '("wx𐐀yz") 0 3 0 5 "NEW") #rx".*abort")
  )

(module+ test
  (test-did-open)
  (test-did-close)
  (test-did-change)
  (test-string->lines)
  (test-range-edit)
  )