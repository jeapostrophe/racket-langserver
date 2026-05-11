#lang racket

(module+ test
  (require rackunit
           "../../doclib/doc-lang.rkt"
           "../../doclib/lexer.rkt")

  (define (parse-prefix text)
    (parse-language-prefix (build-lexer-snapshot text)))

  (define (parse-known-language text [uri #f])
    (parse-language (build-lexer-snapshot text) uri))

  (define (language-name text [uri #f])
    (define language (parse-known-language text uri))
    (and (Known-Language? language)
         (Known-Language-name language)))

  (define (prefix source text end body-start-idx)
    (Language-Prefix source text 0 end body-start-idx))

  (test-case
    "Known-Language has a keyword constructor"
    (check-equal?
      (Known-Language~kw #:name 'demo
                         #:sexp? #t
                         #:suffixes '("demo")
                         #:name-rx #px"^demo$")
      (Known-Language 'demo #t '("demo") #px"^demo$")))

  (test-case
    "parse-language-prefix recognizes an ordinary #lang line"
    (check-equal?
      (parse-prefix "#lang racket/base\n(define x 1)\n")
      (prefix 'lang-directive "racket/base" (string-length "#lang racket/base") 1)))

  (test-case
    "parse-language-prefix recognizes #lang reader wrappers"
    (check-equal?
      (parse-prefix "#lang reader syntax/module-reader\nracket/base\n")
      (prefix 'reader-lang
              "syntax/module-reader"
              (string-length "#lang reader syntax/module-reader")
              1)))

  (test-case
    "parse-language-prefix keeps the full payload after #lang reader"
    (check-equal?
      (parse-prefix "#lang reader \"literal.rkt\"\nhello\n")
      (prefix 'reader-lang
              "\"literal.rkt\""
              (string-length "#lang reader \"literal.rkt\"")
              1))
    (check-equal?
      (parse-prefix
        "#lang reader (submod syntax/module-reader reader)\n1\n")
      (prefix 'reader-lang
              "(submod syntax/module-reader reader)"
              (string-length
                "#lang reader (submod syntax/module-reader reader)")
              1))
    (check-equal?
      (parse-prefix "#lang reader (foo)\n1\n")
      (prefix 'reader-lang
              "(foo)"
              (string-length "#lang reader (foo)")
              1)))

  (test-case
    "parse-language-prefix recognizes #reader directives"
    (check-equal?
      (parse-prefix "#reader scribble/reader\n@title{demo}\n")
      (prefix 'reader-directive
              "scribble/reader"
              (string-length "#reader scribble/reader")
              3))
    (check-equal?
      (parse-prefix "#reader (reader demo)\nbody\n")
      (prefix 'reader-directive
              "(reader demo)"
              (string-length "#reader (reader demo)")
              7)))

  (test-case
    "parse-language-prefix recognizes raw modules"
    (check-equal?
      (parse-prefix "(module demo typed/racket/base (define x 1))\n")
      (prefix 'raw-module
              "typed/racket/base"
              (string-length "(module demo typed/racket/base")
              0))
    (check-equal?
      (parse-prefix "(module demo (lib \"racket/base\") (define x 1))\n")
      (prefix 'raw-module
              "(lib \"racket/base\")"
              (string-length "(module demo (lib \"racket/base\")")
              0))
    (check-equal?
      (parse-prefix "(module demo \"literal.rkt\" (define x 1))\n")
      (prefix 'raw-module
              "\"literal.rkt\""
              (string-length "(module demo \"literal.rkt\"")
              0)))

  (test-case
    "parse-language-prefix skips leading comments and sexp comments"
    (check-equal?
      (parse-prefix "; preamble\n#; (define ignored 1)\n#lang rhombus\nfun f(): 1\n")
      (Language-Prefix 'lang-directive "rhombus" 33 46 13)))

  (test-case
    "parse-language-prefix reports present but unrecognized selectors"
    (check-equal? (parse-prefix "#lang \n(define x 1)\n")
                  (prefix 'malformed-lang-directive "" (string-length "#lang ") 1))
    (check-equal? (parse-prefix "#lang reader\n(define x 1)\n")
                  (prefix 'malformed-lang-directive ""
                          (string-length "#lang reader\n(define x 1)")
                          1))
    (check-equal? (parse-prefix "#lang not-a-real-language\n1\n")
                  (prefix 'lang-directive
                          "not-a-real-language"
                          (string-length "#lang not-a-real-language")
                          1))
    (check-equal? (parse-prefix "#reader\n")
                  (prefix 'malformed-reader-directive "" (string-length "#reader") 2))
    (check-equal? (parse-prefix "#reader does/not/exist\n")
                  (prefix 'reader-directive
                          "does/not/exist"
                          (string-length "#reader does/not/exist")
                          3))
    (check-equal? (parse-prefix "(module demo does/not/exist (define x 1))\n")
                  (prefix 'raw-module
                          "does/not/exist"
                          (string-length "(module demo does/not/exist")
                          0))
    (check-equal? (parse-prefix "(module demo 1 (define x 1))\n")
                  (prefix 'raw-module
                          "1"
                          (string-length "(module demo 1")
                          0))
    (check-equal? (parse-prefix "(module demo)\n")
                  (Language-Prefix
                    'malformed-raw-module
                    ""
                    0
                    (string-length "(module demo)")
                    0))
    (check-false (parse-prefix "(define x 1)\n(module demo racket/base x)\n")))

  (test-case
    "parse-language matches explicit known language families"
    (check-equal? (language-name "#lang racket/base\n(define x 1)\n")
                  'racket)
    (check-equal? (language-name "#lang typed/racket/base\n(define x : Integer 1)\n")
                  'typed/racket)
    (check-equal? (language-name "#reader scribble/reader\n@title{demo}\n")
                  'scribble)
    (check-equal? (language-name "#lang rhombus\nfun f(): 1\n")
                  'rhombus)
    (check-equal? (parse-known-language "#lang not-a-real-language\n1\n")
                  'unrecognized-language)
    (check-equal? (parse-known-language "#reader does/not/exist\n")
                  'unrecognized-language)
    (check-equal? (parse-known-language "(module demo does/not/exist 1)\n")
                  'unrecognized-language))

  (test-case
    "find-language-by-text matches known families"
    (check-equal? (Known-Language-name (find-language-by-text "racket/base"))
                  'racket)
    (check-equal? (Known-Language-name (find-language-by-text "typed/racket/base"))
                  'typed/racket)
    (check-false (find-language-by-text "not-a-real-language")))

  (test-case
    "parse-language can fall back to distinctive suffixes"
    (check-equal? (language-name "fun f(): 1\n" "file:///tmp/demo.rhm")
                  'rhombus)
    (check-equal? (language-name "@title{demo}\n" "file:///tmp/demo.scrbl")
                  'scribble)
    (check-false (language-name "(define x 1)\n" "file:///tmp/demo.rkt"))
    (check-equal? (parse-known-language "#lang not-a-real-language\n1\n"
                                        "file:///tmp/demo.rhm")
                  'unrecognized-language))

  (test-case
    "guess-language-by-uri only uses distinctive suffixes"
    (check-equal? (Known-Language-name (guess-language-by-uri "file:///tmp/demo.rhm"))
                  'rhombus)
    (check-equal? (Known-Language-name (guess-language-by-uri "file:///tmp/demo.scrbl"))
                  'scribble)
    (check-false (guess-language-by-uri "file:///tmp/demo.rkt")))

  (test-case
    "sexp-language? is true only for known sexp families"
    (check-true (sexp-language? "#lang racket/base\n(define x 1)\n"))
    (check-true (sexp-language? "(module demo typed/racket/base (define x 1))\n"))
    (check-false (sexp-language? "#lang scribble/manual\n@title{demo}\n"))
    (check-false (sexp-language? "fun f(): 1\n" "file:///tmp/demo.rhm"))
    (check-false (sexp-language? "(define x 1)\n" "file:///tmp/demo.rkt"))))
