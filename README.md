# racket-langserver

`racket-langserver` is a [Language Server Protocol](http://langserver.org/) implementation for Racket. This project seeks to use [DrRacket](https://github.com/racket/drracket)'s public APIs to provide functionality that mimics DrRacket's code tools as closely as possible.

## Installation and usage

A Racket runtime is a prerequisite, so before using `racket-langserver`, ensure that a Racket runtime is installed. You can install an from the [official download page](https://download.racket-lang.org) or install one from your package manager. Currently `racket-langserver` is compatible with Racket version 7.6 to 9.2.

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

## Language Support

The server recognizes language families and provides different levels of
support depending on whether the language uses s-expression syntax.

- Racket - The standard Racket language (`#lang racket`, `#lang racket/*`, etc.).
- Typed Racket - (`#lang typed/racket`, `#lang typed/racket/*`, etc.).
- Other sexp - Predefined s-expression language families beyond Racket and Typed Racket.
- Scribble - (`#lang scribble`, `#lang scribble/*`, etc.).
- Rhombus - (`#lang rhombus`, `#lang rhombus/*`, etc.).
- Unknown - Language declaration found and parsed, but not in the predefined list.
- Unrecognized - No language declaration found (missing `#lang`, `#reader`, or `(module ...)` form).

### Legend

| Mark | Meaning |
|---|---|
| ✅ | Feature works well and produces useful results. |
| ⚠️ | Partial support - the feature runs but may produce incomplete or imprecise results. |
| ❌ | Not implemented or intentionally filtered out for this language family. |

The matrix rates expected usefulness for each language family. Expansion-based features are marked supported when they only depend on successful expansion and `check-syntax` data. Features are marked partial when they have additional syntax-family limits, lexer limits, or intentionally noisy results.

### Support Matrix

| Feature | Racket | Typed Racket | Other sexp | Scribble | Rhombus | Unknown | Unrecognized |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| Completion | ✅ | ✅ | ✅ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Definition | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| Hover | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| Signature Help | ✅ | ✅ | ✅ | ❌ | ❌ | ⚠️ | ❌ |
| References | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| Document Highlight | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| Rename | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| Prepare Rename | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| Code Action | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| Diagnostics | ✅ | ✅ | ✅ | ✅ | ✅ | ⚠️ | ✅ |
| Document Symbols | ⚠️ | ⚠️ | ⚠️ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Semantic Tokens, Delta | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |
| Semantic Tokens, Full | ✅ | ✅ | ✅ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Semantic Tokens, Range | ✅ | ✅ | ✅ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Formatting | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ |
| Range Formatting | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ |
| On-Type Formatting | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ |
| Inlay Hints | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |

### Features

See [features.md](features.md) for a detailed breakdown of each feature.

## Development

See [CONTRIBUTING.md](CONTRIBUTING.md).
