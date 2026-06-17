#lang racket

(module+ test
  (require rackunit
           "../../common/interfaces.rkt"
           "../../doclib/doc-lang.rkt"
           "../../doclib/lexer.rkt")

  (define (parse-header text)
    (parse-language-header (build-lexer-snapshot text)))

  (define (policy text [uri #f])
    (source->language-policy (build-lexer-snapshot text) uri))

  (define (range start end)
    (CharRange start end))

  (define (header-range text)
    (range 0 (string-length text)))

  (define (check-policy text
                        #:uri [uri #f]
                        #:status status
                        #:language language
                        #:body-mode body-mode
                        #:format? format?
                        #:require-header? require-header?
                        #:expand? expand?)
    (define actual (policy text uri))
    (check-equal? (Language-Policy-header-status actual) status)
    (check-equal? (Language-Policy-policy-language actual) language)
    (check-equal? (Language-Policy-body-mode actual) body-mode)
    (check-equal? (Language-Policy-format? actual) format?)
    (check-equal? (Language-Policy-require-header? actual) require-header?)
    (check-equal? (Language-Policy-expand? actual) expand?))

  (test-case
    "parse-language-header recognizes ordinary hash-lang headers"
    (check-equal?
      (parse-header "#lang racket/base\n(define x 1)\n")
      (HashLang-Header '("racket/base")
                       (header-range "#lang racket/base")
                       1)))

  (test-case
    "parse-language-header keeps hash-lang language chains"
    (check-equal?
      (parse-header "#lang at-exp racket\n@(define x 1)\n")
      (HashLang-Header '("at-exp" "racket")
                       (header-range "#lang at-exp racket")
                       1))
    (check-equal?
      (parse-header "#lang s-exp racket/base\n(define x 1)\n")
      (HashLang-Header '("s-exp" "racket/base")
                       (header-range "#lang s-exp racket/base")
                       1))
    (check-equal?
      (parse-header "#lang errortrace at-exp racket/base\n@(define x 1)\n")
      (HashLang-Header '("errortrace" "at-exp" "racket/base")
                       (header-range "#lang errortrace at-exp racket/base")
                       1))
    (check-equal?
      (parse-header "#lang unknown-wrapper racket/base\n(define x 1)\n")
      (HashLang-Header '("unknown-wrapper")
                       (header-range "#lang unknown-wrapper")
                       1)))

  (test-case
    "parse-language-header recognizes #lang reader headers"
    (check-equal?
      (parse-header "#lang reader syntax/module-reader\nracket/base\n")
      (HashLangReader-Header "syntax/module-reader"
                             (header-range "#lang reader syntax/module-reader")
                             1))
    (check-equal?
      (parse-header "#lang reader\nsyntax/module-reader\nracket/base\n")
      (HashLangReader-Header "syntax/module-reader"
                             (header-range "#lang reader\nsyntax/module-reader")
                             1))
    (check-equal?
      (parse-header "#lang reader \"literal.rkt\"\nhello\n")
      (HashLangReader-Header "\"literal.rkt\""
                             (header-range "#lang reader \"literal.rkt\"")
                             1))
    (check-equal?
      (parse-header "#lang reader (submod syntax/module-reader reader)\n1\n")
      (HashLangReader-Header
        "(submod syntax/module-reader reader)"
        (header-range "#lang reader (submod syntax/module-reader reader)")
        1)))

  (test-case
    "parse-language-header recognizes #reader directives"
    (check-equal?
      (parse-header "#reader scribble/reader\n@title{demo}\n")
      (Reader-Header "scribble/reader"
                     (header-range "#reader scribble/reader")
                     3))
    (check-equal?
      (parse-header "#reader (reader demo)\nbody\n")
      (Reader-Header "(reader demo)"
                     (header-range "#reader (reader demo)")
                     7)))

  (test-case
    "parse-language-header recognizes raw modules"
    (check-equal?
      (parse-header "(module demo typed/racket/base (define x 1))\n")
      (Module-Header "typed/racket/base"
                     (header-range "(module demo typed/racket/base")
                     0))
    (check-equal?
      (parse-header "(module demo (lib \"racket/base\") (define x 1))\n")
      (Module-Header "(lib \"racket/base\")"
                     (header-range "(module demo (lib \"racket/base\")")
                     0))
    (check-equal?
      (parse-header "(module demo \"literal.rkt\" (define x 1))\n")
      (Module-Header "\"literal.rkt\""
                     (header-range "(module demo \"literal.rkt\"")
                     0)))

  (test-case
    "parse-language-header skips leading comments and sexp comments"
    (check-equal?
      (parse-header "; preamble\n#; (define ignored 1)\n#lang rhombus\nfun f(): 1\n")
      (HashLang-Header '("rhombus")
                       (range 33 46)
                       13)))

  (test-case
    "parse-language-header reports missing and incomplete headers"
    (check-equal? (parse-header "(define x 1)\n(module demo racket/base x)\n")
                  (Missing-Header))
    (check-equal? (parse-header "#lang \n(define x 1)\n")
                  (Incomplete-Header 'hash-lang
                                     (header-range "#lang ")
                                     1))
    (check-equal? (parse-header "#lang reader\n(define x 1)\n")
                  (HashLangReader-Header "(define x 1)"
                                         (range 0 (string-length "#lang reader\n(define x 1)"))
                                         1))
    (check-equal? (parse-header "#reader\n")
                  (Incomplete-Header 'reader
                                     (header-range "#reader")
                                     2))
    (check-equal? (parse-header "(module demo)\n")
                  (Incomplete-Header 'module
                                     (header-range "(module demo)")
                                     0)))

  (test-case
    "source->language-policy matches explicit known language families"
    (for ([lang+name (in-list '(("racket/base" racket)
                                ("typed/racket/base" typed/racket)
                                ("scheme/base" scheme)
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
      (define actual
        (Language-Policy-policy-language
          (policy (format "#lang ~a\n1\n" (first lang+name)))))
      (check-equal? actual (second lang+name))))

  (test-case
    "source->language-policy applies selector policy"
    (check-policy "#lang errortrace racket/base\n1\n"
                  #:status 'complete
                  #:language 'racket
                  #:body-mode 'sexp
                  #:format? #t
                  #:require-header? #t
                  #:expand? #t)
    (check-policy "#lang errortrace at-exp racket/base\n@(define x 1)\n"
                  #:status 'complete
                  #:language 'at-exp
                  #:body-mode 'non-sexp
                  #:format? #f
                  #:require-header? #t
                  #:expand? #t)
    (check-policy "#lang at-exp racket/base\n@(define x 1)\n"
                  #:status 'complete
                  #:language 'at-exp
                  #:body-mode 'non-sexp
                  #:format? #f
                  #:require-header? #t
                  #:expand? #t)
    (check-policy "#lang s-exp racket/base\n(define x 1)\n"
                  #:status 'complete
                  #:language 's-exp
                  #:body-mode 'sexp
                  #:format? #t
                  #:require-header? #t
                  #:expand? #t)
    (check-policy "#reader scribble/reader\n@title{demo}\n"
                  #:status 'complete
                  #:language 'scribble
                  #:body-mode 'non-sexp
                  #:format? #f
                  #:require-header? #t
                  #:expand? #t)
    (check-policy "#lang reader syntax/module-reader\nracket/base\n"
                  #:status 'complete
                  #:language 'unrecognized-language
                  #:body-mode 'unknown
                  #:format? #f
                  #:require-header? #t
                  #:expand? #t)
    (check-policy "(module demo typed/racket/base (define x 1))\n"
                  #:status 'complete
                  #:language 'typed/racket
                  #:body-mode 'sexp
                  #:format? #t
                  #:require-header? #t
                  #:expand? #t)
    (check-policy "#lang not-a-real-language\n1\n"
                  #:uri "file:///tmp/demo.rhm"
                  #:status 'complete
                  #:language 'unrecognized-language
                  #:body-mode 'unknown
                  #:format? #f
                  #:require-header? #t
                  #:expand? #t)
    (check-policy "#lang \n1\n"
                  #:status 'incomplete
                  #:language #f
                  #:body-mode 'unknown
                  #:format? #f
                  #:require-header? #t
                  #:expand? #t))

  (test-case
    "source->language-policy falls back by suffix only for missing headers"
    (check-policy "fun f(): 1\n"
                  #:uri "file:///tmp/demo.rhm"
                  #:status 'missing
                  #:language 'rhombus
                  #:body-mode 'non-sexp
                  #:format? #f
                  #:require-header? #t
                  #:expand? #t)
    (check-policy "@title{demo}\n"
                  #:uri "file:///tmp/demo.scrbl"
                  #:status 'missing
                  #:language 'scribble
                  #:body-mode 'non-sexp
                  #:format? #f
                  #:require-header? #t
                  #:expand? #t)
    (check-policy "(define x 1)\n"
                  #:uri "file:///tmp/demo.rkt"
                  #:status 'missing
                  #:language #f
                  #:body-mode 'unknown
                  #:format? #f
                  #:require-header? #t
                  #:expand? #t))

  (test-case
    "rktd files keep data-file policy"
    (check-policy "#lang racket/base\n1\n"
                  #:uri "file:///tmp/demo.rktd"
                  #:status 'complete
                  #:language 'racket
                  #:body-mode 'sexp
                  #:format? #t
                  #:require-header? #t
                  #:expand? #t)
    (check-policy "(define x 1)\n"
                  #:uri "file:///tmp/demo.rktd"
                  #:status 'missing
                  #:language 'racket-data
                  #:body-mode 'unknown
                  #:format? #f
                  #:require-header? #f
                  #:expand? #f)))
