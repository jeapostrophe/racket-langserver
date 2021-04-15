#lang info
(define collection "racket-langserver")
(define deps '("base"
               "compatibility-lib"
               "data-lib"
               "drracket-tool-lib"
               "gui-lib"
               "syntax-color-lib"
               "scribble-lib" ;; for blueboxes (scribble/blueboxes)
               "racket-index" ;; for cross references (setup/xref)
               ))
(define build-deps '("chk"))
(define pkg-desc "Language Server Protocol implementation for Racket.")
(define version "1.0")
