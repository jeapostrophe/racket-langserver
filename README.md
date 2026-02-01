# racket-langserver

`racket-langserver` is a [Language Server Protocol](http://langserver.org/) implementation for Racket. This project seeks to use [DrRacket](https://github.com/racket/drracket)'s public APIs to provide functionality that mimics DrRacket's code tools as closely as possible.

## Installation and usage

A Racket runtime is a prerequisite, so before using `racket-langserver`, ensure that a Racket runtime is installed. You can install an from the [official download page](https://download.racket-lang.org) or install one from your package manager. Currently `racket-langserver` is compatible with Racket version 7.6 to 9.0.

### Atom

You can use the [atom-ide-racket](https://github.com/cfinegan/atom-ide-racket) package. The language server will be automatically installed when `atom-ide-racket` installs.

### VSCode

Use the [Magic Racket](https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket) extension.

### Other editors and IDEs

First, install an LSP runtime for your editor.

Next, install the package via `raco`:

```
raco pkg install racket-langserver
```

Once it is installed, you can configure your editor to use a custom LSP client for Racket (and all installed module, e.g. Rhombus) files (usually `.rkt`), and set the command for the custom client to

```
racket -l racket-langserver
```

You may need to restart your LSP runtime or your editor for `racket-langserver` to start.

## Capabilities

#### *Currently Supported:*

- **Hover** (textDocument/hover)
- **Jump to Definition** (textDocument/definition)
- **Find References** (textDocument/references)
  - *Note:* Currently only considers references within the current file.
- **Document Highlight** (textDocument/documentHighlight)
- **Diagnostics** (textDocument/publishDiagnostics)
- **Code Formatting** (textDocument/formatting & textDocument/rangeFormatting & textDocument/onTypeFormatting)
- **Code Action** (textDocument/codeAction)
- **Signature Help** (textDocument/signatureHelp)
- **Rename** (textDocument/rename & textDocument/prepareRename)
  - *Note:* Currently only allows renaming symbols defined within the current file.
- **Code completion** (textDocument/completion)

#### *Work in Progress:*

- **Document Outline** (textDocument/documentSymbol)

## Development

See [CONTRIBUTING.md](CONTRIBUTING.md).

## Notes for Contributers

It is useful to think of this project as a "headless mode" for DrRacket. Contributions to this project should seek to avoid re-implementing functionality that is already exposed through DrRacket's public API.

Currently, we do not support  workspace-wide (or project-wide) methods because the underlying DrRacket code tools only operate on one file at a time. If multi-file code tools are a desirable feature, then they should be considered for inclusion into [DrRacket proper](https://github.com/racket/drracket) before being implemented in this project.

[jeapostrophe commented on Apr 29, 2020](https://github.com/jeapostrophe/racket-langserver/issues/8#issuecomment-621242014)
> I think that the right way to implement most features in Racket-LSP is to find the corresponding feature in DrRacket and then disentangle from DrR's GUI and then expose the feature through the LSP. In many cases, DrR has already been internally organized to do that, but we just haven't done enough spelunking yet.
