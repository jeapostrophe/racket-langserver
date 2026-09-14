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

(define racket-text "#lang racket/base\n(define x 1)")
(define scribble-text "#lang scribble/base\n@itemlist[\n@item{one}\n]")

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
      (formatting (editor-with text)
                  0
                  2
                  #:backend 'fixw
                  #:formatting-options conservative-options)
      expected)
    (check-equal?
      (formatting (editor-with text)
                  0
                  2
                  #:backend 'fixw
                  #:formatting-options unrelated-options)
      expected))

  (test-case
    "fmt forwards supported extension options and ignores all others"
    (define calls '())
    (define (runner arguments text)
      (set! calls (list arguments text))
      (values 0 "#lang racket/base\n(define\n   x\n   1)\n" ""))
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
    (parameterize ([current-fmt-runner runner])
      (check-equal?
        (formatting (editor-with racket-text)
                    0
                    1
                    #:backend 'fmt
                    #:formatting-options options)
        (list
          (TextEdit (Range (Pos 0 0) (Pos 1 12))
                    "#lang racket/base\n(define\n   x\n   1)\n"))))
    (check-equal? calls
                  (list '("--indent" "3" "--max-blank-lines" "2" "--width" "91")
                        racket-text)))

  (test-case
    "fmt forwards a zero extra option and rejects a mistyped extra"
    (define calls '())
    (define (runner arguments text)
      (set! calls (list arguments text))
      (values 0 text ""))
    (define zero-indent
      (jsexpr->FormattingOptions
        (hasheq 'tabSize 2
                'insertSpaces #t
                'indent 0)))
    (check-true (FormattingOptions? zero-indent))
    (parameterize ([current-fmt-runner runner])
      (check-equal?
        (formatting (editor-with racket-text)
                    0
                    0
                    #:backend 'fmt
                    #:formatting-options zero-indent)
        '()))
    (check-equal? calls
                  (list '("--indent" "0")
                        racket-text))
    (define mistyped
      (jsexpr->FormattingOptions
        (hasheq 'tabSize 2
                'insertSpaces #t
                'width "91")))
    (check-true (FormattingOptions? mistyped))
    (check-exn
      #rx"Fmt-Extra-Options"
      (lambda ()
        (parameterize ([current-fmt-runner runner])
          (formatting (editor-with racket-text)
                      0
                      0
                      #:backend 'fmt
                      #:formatting-options mistyped)))))

  (test-case
    "missing fmt reports the installation action"
    (define raised
      (with-handlers ([exn:fail:fmt-unavailable? values])
        (parameterize ([current-fmt-runner
                        (lambda (_arguments _text)
                          (values 1 "" "raco: Unrecognized command: fmt\n"))])
          (formatting (editor-with racket-text)
                      0
                      0
                      #:backend 'fmt
                      #:formatting-options (formatting-options 2 #t (hasheq))))))
    (check-true (exn:fail:fmt-unavailable? raised))
    (check-regexp-match #rx"raco pkg install fmt" (exn-message raised)))

  (test-case
    "fmt command failure is a fmt error"
    (define raised
      (with-handlers ([exn:fail:fmt? values])
        (parameterize ([current-fmt-runner
                        (lambda (_arguments _text)
                          (values 1 "" "pretty-print exploded\n"))])
          (formatting (editor-with racket-text)
                      0
                      0
                      #:backend 'fmt
                      #:formatting-options (formatting-options 2 #t (hasheq))))))
    (check-true (exn:fail:fmt? raised))
    (check-false (exn:fail:fmt-unavailable? raised))
    (check-regexp-match #rx"raco fmt command failed" (exn-message raised)))

  (test-case
    "fmt replacement range uses the original text buffer"
    (define original "#lang racket/base\na\r\nb\n")
    (define replacement "reflowed")
    (define editor (editor-with original))
    (parameterize ([current-fmt-runner
                    (lambda (_arguments _text) (values 0 replacement ""))])
      (check-equal?
        (formatting editor
                    0
                    0
                    #:backend 'fmt
                    #:formatting-options (formatting-options 2 #t (hasheq)))
        (list (TextEdit (Range (Pos 0 0)
                               (abs-pos->Pos editor (send editor end-pos)))
                        replacement)))))

  (test-case
    "formatting does not run fixw or fmt on unsupported languages"
    (define options (formatting-options 2 #t (hasheq)))
    (define scribble-edits
      (list (TextEdit (Range (Pos 2 0) (Pos 2 0)) " ")
            (TextEdit (Range (Pos 3 0) (Pos 3 0)) " ")))
    (parameterize ([current-fmt-runner
                    (lambda (_arguments _text)
                      (error 'test "fmt must not be run"))])
      (check-equal?
        (formatting (editor-with scribble-text)
                    0
                    3
                    #:backend 'fmt
                    #:formatting-options options)
        scribble-edits)
      (check-equal?
        (formatting (editor-with scribble-text)
                    0
                    3
                    #:backend 'fixw
                    #:formatting-options options)
        scribble-edits))))
