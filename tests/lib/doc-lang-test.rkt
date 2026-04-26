#lang racket

(module+ test
  (require rackunit
           "../../doclib/doc-lang.rkt"
           "../../doclib/lexer.rkt")

  (define (parse-node text)
    (parse-language-node (build-lexer-snapshot text)))

  (define (parse-known-language text [uri #f])
    (parse-language (build-lexer-snapshot text) uri))

  (define (language-name text [uri #f])
    (define language (parse-known-language text uri))
    (and (Known-Language? language)
         (Known-Language-name language)))

  (define (node source text end)
    (Language-Node source text 0 end))

  (test-case
    "Known-Language has a keyword constructor"
    (check-equal?
      (Known-Language~kw #:name 'demo
                         #:sexp? #t
                         #:suffixes '("demo")
                         #:name-rx #px"^demo$")
      (Known-Language 'demo #t '("demo") #px"^demo$")))

  (test-case
    "parse-language-node recognizes an ordinary #lang line"
    (check-equal?
      (parse-node "#lang racket/base\n(define x 1)\n")
      (node 'lang-directive "racket/base" (string-length "#lang racket/base"))))

  (test-case
    "parse-language-node recognizes #lang reader wrappers"
    (check-equal?
      (parse-node "#lang reader syntax/module-reader\nracket/base\n")
      (node 'reader-lang
            "syntax/module-reader"
            (string-length "#lang reader syntax/module-reader"))))

  (test-case
    "parse-language-node keeps the full payload after #lang reader"
    (check-equal?
      (parse-node "#lang reader \"literal.rkt\"\nhello\n")
      (node 'reader-lang
            "\"literal.rkt\""
            (string-length "#lang reader \"literal.rkt\"")))
    (check-equal?
      (parse-node
        "#lang reader (submod syntax/module-reader reader)\n1\n")
      (node 'reader-lang
            "(submod syntax/module-reader reader)"
            (string-length
              "#lang reader (submod syntax/module-reader reader)")))
    (check-equal?
      (parse-node "#lang reader (foo)\n1\n")
      (node 'reader-lang
            "(foo)"
            (string-length "#lang reader (foo)"))))

  (test-case
    "parse-language-node recognizes #reader directives"
    (check-equal?
      (parse-node "#reader scribble/reader\n@title{demo}\n")
      (node 'reader-directive
            "scribble/reader"
            (string-length "#reader scribble/reader")))
    (check-equal?
      (parse-node "#reader (reader demo)\nbody\n")
      (node 'reader-directive
            "(reader demo)"
            (string-length "#reader (reader demo)"))))

  (test-case
    "parse-language-node recognizes raw modules"
    (check-equal?
      (parse-node "(module demo typed/racket/base (define x 1))\n")
      (node 'raw-module
            "typed/racket/base"
            (string-length "(module demo typed/racket/base")))
    (check-equal?
      (parse-node "(module demo (lib \"racket/base\") (define x 1))\n")
      (node 'raw-module
            "(lib \"racket/base\")"
            (string-length "(module demo (lib \"racket/base\")")))
    (check-equal?
      (parse-node "(module demo \"literal.rkt\" (define x 1))\n")
      (node 'raw-module
            "\"literal.rkt\""
            (string-length "(module demo \"literal.rkt\""))))

  (test-case
    "parse-language-node skips leading comments and sexp comments"
    (check-equal?
      (parse-node "; preamble\n#; (define ignored 1)\n#lang rhombus\nfun f(): 1\n")
      (Language-Node 'lang-directive "rhombus" 33 46)))

  (test-case
    "parse-language-node reports present but unrecognized selectors"
    (check-equal? (parse-node "#lang \n(define x 1)\n")
                  (node 'malformed-lang-directive "" (string-length "#lang ")))
    (check-equal? (parse-node "#lang reader\n(define x 1)\n")
                  (node 'malformed-lang-directive ""
                        (string-length "#lang reader\n(define x 1)")))
    (check-equal? (parse-node "#lang not-a-real-language\n1\n")
                  (node 'lang-directive
                        "not-a-real-language"
                        (string-length "#lang not-a-real-language")))
    (check-equal? (parse-node "#reader\n")
                  (node 'malformed-reader-directive "" (string-length "#reader")))
    (check-equal? (parse-node "#reader does/not/exist\n")
                  (node 'reader-directive
                        "does/not/exist"
                        (string-length "#reader does/not/exist")))
    (check-equal? (parse-node "(module demo does/not/exist (define x 1))\n")
                  (node 'raw-module
                        "does/not/exist"
                        (string-length "(module demo does/not/exist")))
    (check-equal? (parse-node "(module demo 1 (define x 1))\n")
                  (node 'raw-module
                        "1"
                        (string-length "(module demo 1")))
    (check-equal? (parse-node "(module demo)\n")
                  (Language-Node
                    'malformed-raw-module
                    ""
                    0
                    (string-length "(module demo)")))
    (check-false (parse-node "(define x 1)\n(module demo racket/base x)\n")))

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
    (check-false (sexp-language? "(define x 1)\n" "file:///tmp/demo.rkt")))

  (test-case
    "get-indenter returns a procedure or false"
    (check-false (get-indenter "(define x 1)\n"))
    (check-true (procedure? (get-indenter "#lang scribble/manual\n@title{demo}\n")))
    (check-false (get-indenter "#lang does/not/exist\n1\n"))))
