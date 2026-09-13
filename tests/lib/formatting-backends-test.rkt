#lang racket/base

(require rackunit
         racket/class
         "../../common/interfaces.rkt"
         "../../doclib/editor.rkt"
         "../../doclib/formatting.rkt"
         "../../doclib/formatter/fmt.rkt")

(define (formatting-options tab-size insert-spaces extras)
  (FormattingOptions #:tab-size tab-size
                     #:insert-spaces insert-spaces
                     #:trim-trailing-whitespace #f
                     #:insert-final-newline #f
                     #:trim-final-newlines #f
                     #:extras extras))

(define (editor-with text)
  (define editor (new lsp-editor%))
  (send editor insert text 0)
  editor)

(module+ test
  (test-case
    "fixw ignores unsupported LSP formatting options"
    (define text "#lang racket/base\n(define x\n1)")
    (define conservative-options
      (formatting-options 2 #t (hasheq)))
    (define unrelated-options
      (formatting-options 8 #f (hasheq 'unsupported "value")))
    (define expected
      (list (TextEdit (Range (Pos 2 0) (Pos 2 2)) "  1)")))
    (check-equal?
      (formatting text
                  0
                  2
                  #:backend 'fixw
                  #:formatting-options conservative-options)
      expected)
    (check-equal?
      (formatting text
                  0
                  2
                  #:backend 'fixw
                  #:formatting-options unrelated-options)
      expected))

  (test-case
    "fmt forwards supported extension options and ignores all others"
    (define calls '())
    (define (loader)
      (make-keyword-procedure
        (lambda (keywords keyword-values . arguments)
          (set! calls (list keywords keyword-values arguments))
          "#lang racket/base\n(define\n   x\n   1)\n")))
    (define options
      (jsexpr->FormattingOptions
        (hasheq 'tabSize 8
                'insertSpaces #f
                'trimTrailingWhitespace #t
                'insertFinalNewline #t
                'trimFinalNewlines #t
                'width 91
                'indent 3
                'limit 40
                'maxBlankLines 2
                'unsupported "ignored")))
    (check-true (FormattingOptions? options))
    (define text "#lang racket/base\n(define x 1)")
    (parameterize ([current-fmt-program-format-loader loader])
      (check-equal?
        (formatting text
                    0
                    1
                    #:backend 'fmt
                    #:editor (editor-with text)
                    #:formatting-options options)
        (list
          (TextEdit (Range (Pos 0 0) (Pos 1 12))
                    "#lang racket/base\n(define\n   x\n   1)\n"))))
    (check-equal? calls
                  (list '(#:indent #:limit #:max-blank-lines #:width)
                        '(3 40 2 91)
                        (list text))))

  (test-case
    "fmt forwards a zero extra option and rejects a mistyped extra"
    (define calls '())
    (define (loader)
      (make-keyword-procedure
        (lambda (keywords keyword-values . arguments)
          (set! calls (list keywords keyword-values arguments))
          (car arguments))))
    (define zero-indent
      (jsexpr->FormattingOptions
        (hasheq 'tabSize 2
                'insertSpaces #t
                'indent 0)))
    (check-true (FormattingOptions? zero-indent))
    (parameterize ([current-fmt-program-format-loader loader])
      (check-equal?
        (formatting "(define x 1)"
                    0
                    0
                    #:backend 'fmt
                    #:editor (editor-with "(define x 1)")
                    #:formatting-options zero-indent)
        '()))
    (check-equal? calls
                  (list '(#:indent)
                        '(0)
                        '("(define x 1)")))
    (define mistyped
      (jsexpr->FormattingOptions
        (hasheq 'tabSize 2
                'insertSpaces #t
                'width "91")))
    (check-true (FormattingOptions? mistyped))
    (check-exn
      #rx"Fmt-Extra-Options"
      (lambda ()
        (parameterize ([current-fmt-program-format-loader loader])
          (formatting "(define x 1)"
                      0
                      0
                      #:backend 'fmt
                      #:editor (editor-with "(define x 1)")
                      #:formatting-options mistyped)))))

  (test-case
    "missing fmt reports the installation action"
    (define raised
      (with-handlers ([exn:fail:fmt-unavailable? values])
        (parameterize ([current-fmt-program-format-loader
                        (lambda ()
                          (error 'dynamic-require "collection not found"))])
          (formatting "(define x 1)"
                      0
                      0
                      #:backend 'fmt
                      #:editor (editor-with "(define x 1)")
                      #:formatting-options (formatting-options 2 #t (hasheq))))))
    (check-true (exn:fail:fmt-unavailable? raised))
    (check-regexp-match #rx"raco pkg install fmt" (exn-message raised)))

  (test-case
    "fmt replacement range uses the original text buffer"
    (define original "a\r\nb\n")
    (define replacement "reflowed")
    (parameterize ([current-fmt-program-format-loader
                    (lambda () (lambda (_text) replacement))])
      (check-equal?
        (formatting original
                    0
                    0
                    #:backend 'fmt
                    #:editor (editor-with original)
                    #:formatting-options (formatting-options 2 #t (hasheq)))
        (list (TextEdit (Range (Pos 0 0) (Pos 2 0)) replacement))))))
