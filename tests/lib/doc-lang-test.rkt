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

  (define (language-info text)
    (define snapshot (build-lexer-snapshot text))
    (lexer-language-info (LexerSnapshot-text snapshot)
                         (LexerSnapshot-tokens snapshot)))

  (define (prefix declaration end body-start-idx)
    (Language-Prefix declaration 0 end body-start-idx))

  (define (lang-prefix text end body-start-idx)
    (prefix (HashLang-Declaration text) end body-start-idx))

  (define (wrapped-lang-prefix wrapper delegate-text end body-start-idx)
    (prefix (WrapperHashLang-Declaration wrapper delegate-text) end body-start-idx))

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
      (lang-prefix "racket/base" (string-length "#lang racket/base") 1)))

  (test-case
    "parse-language-prefix recognizes wrapped #lang lines"
    (check-equal?
      (parse-prefix "#lang at-exp racket\n@(define x 1)\n")
      (wrapped-lang-prefix "at-exp"
                           "racket"
                           (string-length "#lang at-exp racket")
                           1))
    (check-equal?
      (parse-prefix "#lang s-exp racket/base\n(define x 1)\n")
      (wrapped-lang-prefix "s-exp"
                           "racket/base"
                           (string-length "#lang s-exp racket/base")
                           1))
    (check-equal?
      (parse-prefix "#lang unknown-wrapper racket/base\n(define x 1)\n")
      (lang-prefix "unknown-wrapper"
                   (string-length "#lang unknown-wrapper")
                   1)))

  (test-case
    "parse-language-prefix recognizes #lang reader wrappers"
    (check-equal?
      (parse-prefix "#lang reader syntax/module-reader\nracket/base\n")
      (prefix (WrapperHashLang-Declaration "reader" "syntax/module-reader")
              (string-length "#lang reader syntax/module-reader")
              1)))

  (test-case
    "parse-language-prefix keeps the full payload after #lang reader"
    (check-equal?
      (parse-prefix "#lang reader \"literal.rkt\"\nhello\n")
      (prefix (WrapperHashLang-Declaration "reader" "\"literal.rkt\"")
              (string-length "#lang reader \"literal.rkt\"")
              1))
    (check-equal?
      (parse-prefix
        "#lang reader (submod syntax/module-reader reader)\n1\n")
      (prefix (WrapperHashLang-Declaration
                "reader"
                "(submod syntax/module-reader reader)")
              (string-length
                "#lang reader (submod syntax/module-reader reader)")
              1))
    (check-equal?
      (parse-prefix "#lang reader (foo)\n1\n")
      (prefix (WrapperHashLang-Declaration "reader" "(foo)")
              (string-length "#lang reader (foo)")
              1)))

  (test-case
    "parse-language-prefix recognizes #reader directives"
    (check-equal?
      (parse-prefix "#reader scribble/reader\n@title{demo}\n")
      (prefix (Reader-Declaration "scribble/reader")
              (string-length "#reader scribble/reader")
              3))
    (check-equal?
      (parse-prefix "#reader (reader demo)\nbody\n")
      (prefix (Reader-Declaration "(reader demo)")
              (string-length "#reader (reader demo)")
              7)))

  (test-case
    "parse-language-prefix recognizes raw modules"
    (check-equal?
      (parse-prefix "(module demo typed/racket/base (define x 1))\n")
      (prefix (Module-Declaration "typed/racket/base")
              (string-length "(module demo typed/racket/base")
              0))
    (check-equal?
      (parse-prefix "(module demo (lib \"racket/base\") (define x 1))\n")
      (prefix (Module-Declaration "(lib \"racket/base\")")
              (string-length "(module demo (lib \"racket/base\")")
              0))
    (check-equal?
      (parse-prefix "(module demo \"literal.rkt\" (define x 1))\n")
      (prefix (Module-Declaration "\"literal.rkt\"")
              (string-length "(module demo \"literal.rkt\"")
              0)))

  (test-case
    "parse-language-prefix skips leading comments and sexp comments"
    (check-equal?
      (parse-prefix "; preamble\n#; (define ignored 1)\n#lang rhombus\nfun f(): 1\n")
      (Language-Prefix (HashLang-Declaration "rhombus")
                       33
                       46
                       13)))

  (test-case
    "parse-language-prefix reports present but unrecognized selectors"
    (check-equal? (parse-prefix "#lang \n(define x 1)\n")
                  (prefix (HashLang-Declaration #f)
                          (string-length "#lang ")
                          1))
    (check-equal? (parse-prefix "#lang reader\n(define x 1)\n")
                  (prefix (HashLang-Declaration #f)
                          (string-length "#lang reader\n(define x 1)")
                          1))
    (check-equal? (parse-prefix "#lang not-a-real-language\n1\n")
                  (lang-prefix "not-a-real-language"
                               (string-length "#lang not-a-real-language")
                               1))
    (check-equal? (parse-prefix "#reader\n")
                  (prefix (Reader-Declaration #f)
                          (string-length "#reader")
                          2))
    (check-equal? (parse-prefix "#reader does/not/exist\n")
                  (prefix (Reader-Declaration "does/not/exist")
                          (string-length "#reader does/not/exist")
                          3))
    (check-equal? (parse-prefix "(module demo does/not/exist (define x 1))\n")
                  (prefix (Module-Declaration "does/not/exist")
                          (string-length "(module demo does/not/exist")
                          0))
    (check-equal? (parse-prefix "(module demo 1 (define x 1))\n")
                  (prefix (Module-Declaration "1")
                          (string-length "(module demo 1")
                          0))
    (check-equal? (parse-prefix "(module demo)\n")
                  (Language-Prefix
                    (Module-Declaration #f)
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
    (for ([lang+name (in-list '(("scheme" scheme)
                                ("scheme/base" scheme)
                                ("scheme/list" scheme)
                                ("mzscheme" mzscheme)
                                ("r5rs" r5rs)
                                ("r6rs" r6rs)
                                ("r7rs" r7rs)
                                ("lazy" lazy)
                                ("slideshow" slideshow)
                                ("plai" plai)
                                ("plai-typed" plai)
                                ("plai-lazy" plai)
                                ("plait" plait)
                                ("htdp/bsl" htdp)
                                ("htdp/isl" htdp)
                                ("htdp/isl+" htdp)
                                ("htdp/asl" htdp)
                                ("eopl" eopl)
                                ("sicp" sicp)
                                ("swindle" swindle)
                                ("frtime" frtime)
                                ("rosette" rosette)
                                ("rosette/safe" rosette)
                                ("pie" pie)))])
      (check-equal? (language-name (format "#lang ~a\n1\n" (first lang+name)))
                    (second lang+name)))
    (check-equal? (language-name "#reader scribble/reader\n@title{demo}\n")
                  'scribble)
    (check-equal? (language-name "#lang rhombus\nfun f(): 1\n")
                  'rhombus)
    (check-equal? (language-name "#lang at-exp racket\n@(define x 1)\n")
                  'at-exp)
    (check-equal? (language-name "#lang s-exp racket/base\n1\n")
                  's-exp)
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
    (check-equal? (Known-Language-name (find-language-by-text "scheme/base"))
                  'scheme)
    (check-equal? (Known-Language-name (find-language-by-text "r6rs"))
                  'r6rs)
    (check-equal? (Known-Language-name (find-language-by-text "htdp/isl+"))
                  'htdp)
    (check-equal? (Known-Language-name (find-language-by-text "rosette/safe"))
                  'rosette)
    (check-equal? (Known-Language-name (find-language-by-text "pie"))
                  'pie)
    (check-equal? (Known-Language-name (find-language-by-text "at-exp"))
                  'at-exp)
    (check-equal? (Known-Language-name (find-language-by-text "s-exp"))
                  's-exp)
    (check-false (find-language-by-text "plai/foo"))
    (check-false (find-language-by-text "htdp/bsl/foo"))
    (check-false (find-language-by-text "rosette/unsafe"))
    (check-false (find-language-by-text "s-exp racket/base"))
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
    "wrapped #lang lines classify wrapper languages and body mode"
    (define at-exp-info (language-info "#lang at-exp racket\n@(define x 1)\n"))
    (check-equal? (Known-Language-name (Language-Info-language at-exp-info))
                  'at-exp)
    (check-equal? (Language-Info-body-mode at-exp-info) 'non-sexp)

    (define s-exp-info (language-info "#lang s-exp racket/base\n(define x 1)\n"))
    (check-equal? (Known-Language-name (Language-Info-language s-exp-info))
                  's-exp)
    (check-equal? (Language-Info-body-mode s-exp-info) 'sexp))

  (test-case
    "rktd files are Racket data, not expandable modules"
    (define path (string->path "/tmp/demo.rktd"))
    (check-true (racket-data-file-path? path))
    (check-false (requires-expansion? path))
    (check-false (requires-language-declaration? path)))

  (test-case
    "rkt files still require module policy"
    (define path (string->path "/tmp/demo.rkt"))
    (check-false (racket-data-file-path? path))
    (check-true (requires-expansion? path))
    (check-true (requires-language-declaration? path)))

  (test-case
    "sexp-language? is true only for known sexp families"
    (check-true (sexp-language? "#lang racket/base\n(define x 1)\n"))
    (check-true (sexp-language? "(module demo typed/racket/base (define x 1))\n"))
    (check-true (sexp-language? "#lang scheme/base\n(define x 1)\n"))
    (check-true (sexp-language? "#lang r5rs\n(define x 1)\n"))
    (check-true (sexp-language? "#lang r6rs\n(import (rnrs))\n(define x 1)\n"))
    (check-true (sexp-language? "#lang r7rs\n(define x 1)\n"))
    (check-true (sexp-language? "#lang lazy\n(define x 1)\n"))
    (check-true (sexp-language? "#lang htdp/isl+\n(define x 1)\n"))
    (check-true (sexp-language? "#lang rosette\n(define x 1)\n"))
    (check-true (sexp-language? "#lang pie\n(claim n Nat)\n"))
    (check-true (sexp-language? "#lang s-exp racket/base\n(define x 1)\n"))
    (check-false (sexp-language? "#lang plai/foo\n(define x 1)\n"))
    (check-false (sexp-language? "#lang at-exp racket\n@(define x 1)\n"))
    (check-false (sexp-language? "#lang scribble/manual\n@title{demo}\n"))
    (check-false (sexp-language? "fun f(): 1\n" "file:///tmp/demo.rhm"))
    (check-false (sexp-language? "(define x 1)\n" "file:///tmp/demo.rkt"))))
