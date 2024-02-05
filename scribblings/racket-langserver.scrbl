#lang scribble/manual
@(require scribble/extract
          (for-label racket))

@title{racket-langserver}

The @tt{racket-langserver} is a @hyperlink["https://langserver.org/"]{Language Server Protocol}
implementation for Racket. This project seeks to use 
@seclink[#:indirect? #t #:doc '(lib "scribblings/drracket-tools/drracket-tools.scrbl") "top"]{DrRacket's public APIs} 
to provide functionality that mimics DrRacket's code tools as closely as possible.

@section{Installation and usage}

A Racket runtime is a prerequisite, so before using @tt{racket-langserver}, ensure that a Racket runtime 
is installed. You can install an from the @hyperlink["https://download.racket-lang.org"]{official download page}
or install one from your package manager.

First, install an LSP runtime for your editor.

Next, install the package via @tt{raco}:

@commandline{raco pkg install racket-langserver}

@margin-note{To update the @tt{racket-langserver} use
@commandline{raco pkg update racket-langserver}}

Once it is installed, you can configure your editor to use a custom LSP client for Racket files (@tt{.rkt}), 
and set the command for the custom client to

@commandline{racket -l racket-langserver}

You may need to restart your LSP runtime or your editor for @tt{racket-langserver} to start.

@subsection{VSCode}

Use the @hyperlink["https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket"]{Magic Racket} extension.
