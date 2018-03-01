#lang info
(define collection "racket-langserver")
(define deps '("base"
               "compatibility-lib"
               "data-lib"
               "drracket-tool-lib"
               "gui-lib"
               "syntax-color-lib"))
(define build-deps '("chk"))
(define pkg-desc "Language Server Protocol implementation for Racket.")
(define version "1.0")