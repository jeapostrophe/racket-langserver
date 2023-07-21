#lang info
(define collection "racket-langserver")
(define deps '("base"
               "compatibility-lib"
               "data-lib"
               ;; Newer packages need only to depend on drracket-tool-text-lib.
               ;; However, we still refer to drracket-tool-lib because it is split
               ;; into the text- variant starting from Racket 8.4.
               "drracket-tool-lib"
               "gui-lib"
               "syntax-color-lib"
               "sandbox-lib"  ;; running macro expansion with time limits
               "scribble-lib" ;; for blueboxes (scribble/blueboxes)
               "racket-index" ;; for cross references (setup/xref)
               "html-parsing" ;; for parsing documentation text
               ))
(define build-deps '("chk-lib"))
(define pkg-desc "Language Server Protocol implementation for Racket.")
(define version "1.0")
