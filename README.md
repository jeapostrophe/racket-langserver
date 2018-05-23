# racket-langserver

[Language Server Protocol](http://langserver.org/) implementation for Racket. This project seeks to use DrRacket's public APIs to provide functionality that mimics DrRacket's code tools as closely as possible.

## Installation

Install via the command line with `raco pkg install racket-langserver`

If you plan on using this package with Atom, you should install [atom-ide-racket](https://github.com/cfinegan/atom-ide-racket) instead. This package will be installed automatically by the *atom-ide-racket* installer (although the Racket runtime should be installed prior to this).

## Capabilities

#### *Currently Supported:*

- **Hover** (textDocument/hover)
- **Jump to Definition** (textDocument/definition)
- **Find References** (textDocument/references)
  - *Note:* Currently only considers references within the current file.
- **Document Highlight** (textDocument/documentHighlight)
- **Diagnostics** (textDocument/publishDiagnostics)

#### *Work in Progress:*

- **Document Outline** (textDocument/documentSymbol)
- **Code Formatting** (textDocument/formatting & textDocument/rangeFormatting)

#### *Would be Nice:*

- **Rename** (textDocument/rename)
- **Code completion** (textDocument/completion)

## Notes for Contributers

It is useful to think of this project as a "headless mode" for DrRacket. Contributions to this project should seek to avoid re-implementing functionality that is already exposed through DrRacket's public API.

Currently, we do not support  workspace-wide (or project-wide) methods because the underlying DrRacket code tools only operate on one file at a time. If multi-file code tools are a desirable feature, then they should be considered for inclusion into [DrRacket proper](https://github.com/racket/drracket) before being implemented in this project.
