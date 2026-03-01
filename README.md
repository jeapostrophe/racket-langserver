# racket-langserver

`racket-langserver` is a [Language Server Protocol](http://langserver.org/) implementation for Racket. This project seeks to use [DrRacket](https://github.com/racket/drracket)'s public APIs to provide functionality that mimics DrRacket's code tools as closely as possible.

## Installation and usage

A Racket runtime is a prerequisite, so before using `racket-langserver`, ensure that a Racket runtime is installed. You can install an from the [official download page](https://download.racket-lang.org) or install one from your package manager. Currently `racket-langserver` is compatible with Racket version 7.6 to 9.1.

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
  - *Note:* Currently only considers references from opened files within the workspace.
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
