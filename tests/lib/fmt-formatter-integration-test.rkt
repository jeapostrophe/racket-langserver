#lang racket/base

;; Real `raco fmt` coverage. Skip when fmt is not installed; the dedicated
;; CI job installs it.

(require rackunit
         "../../common/interfaces.rkt"
         "../../doclib/doc.rkt"
         "../../doclib/formatter/fmt.rkt")

(define source
  "#lang racket/base\n(define (f x)\n(+ x 1))")
(define formatted
  "#lang racket/base\n(define (f x)\n  (+ x 1))\n")
(define options
  (FormattingOptions #:tab-size 2
                     #:insert-spaces #t
                     #:trim-trailing-whitespace #f
                     #:insert-final-newline #f
                     #:trim-final-newlines #f
                     #:extras (hasheq)))

(define (fmt-available?)
  (with-handlers ([exn:fail:fmt-unavailable? (lambda (_exn) #f)])
    (fmt-format-document source empty-fmt-settings)
    #t))

(module+ test
  (define has-fmt? (fmt-available?))
  (unless has-fmt?
    (displayln "Skipping fmt integration because fmt is unavailable."))

  (test-case
    "installed fmt formats through the document backend"
    (when has-fmt?
      (define doc (make-doc "file:///fmt-integration.rkt" source))
      (check-equal?
        (doc-format-edits doc
                          (Range (Pos 0 0) (Pos 2 8))
                          #:backend 'fmt
                          #:formatting-options options)
        (list (TextEdit (Range (Pos 0 0) (Pos 2 8)) formatted))))))
