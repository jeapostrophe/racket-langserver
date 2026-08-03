# Features

For a quick overview of which features are supported per language family, see the **Support Matrix** in [README.md](README.md).

Many features need your code to expand without errors. After each edit, the server tries re-expanding your file. Each new edit cancels any running expansion and starts a fresh one. Results from the last successful expansion stay available until the new one finishes, so features keep working during editing. If expansion never succeeds, features that require expansion will never return useful results. The code needs to be correct for at least one moment, and stay that correct state for a few seconds to let expansion finish.

Expansion based features mostly use DrRacket's `check-syntax` APIs. The server expands the module, collects the binding, documentation, diagnostic, and highlighting information that `check-syntax` reports, and translates those results into LSP responses.

Several features also use the lexer from `syntax-color`. The lexer dispatches to a language-specific tokenizer based on the `#lang` declaration. Recognized languages get accurate tokenization. Unrecognized languages fallback to the Racket lexer, which probably does not understand their syntax and produces unreliable tokens.

## Integrations

### Resyntax

[Resyntax](https://github.com/jackfirth/resyntax) provides automated refactoring suggestions. If you have Resyntax installed, it is used automatically with no configuration. Suggestions appear as diagnostics and code actions in your editor. If Resyntax is not installed, the server works normally without it.

### racket-fixw

The Formatting feature uses [racket-fixw](https://github.com/6cdh/racket-fixw) for recognized sexp language indentation. This is a required dependency and is included when you install the server. Other external formatters can be supported, open an issue if you'd like one added.

## Code Action *(requires expansion)*

The Quick Fix menu offers two kinds of actions:

- **Unused variable** suggests adding a `_` prefix to silence the warning.
- **Refactoring** suggestions powered by Resyntax, shown when Resyntax is installed. Resyntax works automatically with no configuration needed. If it is not installed, these suggestions are simply not shown.

Uses DrRacket's `check-syntax` for unused variable detection.

Language behavior: not filtered by language family. Works for any language where expansion succeeds and check-syntax or Resyntax produce useful results.

## Completion *(identifier completion requires expansion)*

The autocomplete popup provides two kinds of results:

- **Identifiers** defined in your file and imported from required modules. These need expansion.
- **Module paths** for `require` forms, based on your installed collections. These work without expansion. It covers both bare paths like `racket/base` and string paths like `"racket/base"`.

Identifier completion is powered by DrRacket's `check-syntax`. The server registers `(` as a trigger character. In VS Code, extensions control the `wordPattern` setting, it determines whether completion can trigger for other places except after `(`.

Language behavior: not filtered by language family for either source. Identifier completion works where expansion succeeds and useful binding data is produced. Module-path completion works without expansion. The `(` trigger character is sexp-oriented.

## Definition *(requires expansion)*

Jump to the definition of the identifier under the cursor. Works for both local definitions and identifiers imported from other files. Powered by DrRacket's `check-syntax`.

Language behavior: not filtered by language family. Works where expansion succeeds and check-syntax produces binding data with reliable source ranges.

## Diagnostics *(expansion for 3 of 4 sources)*

Problems are shown from these sources:

- **Reader and expander errors** (syntax errors, missing modules, broken `.zo` files). Shown even if expansion fails. `.zo` version mismatch errors include a suggestion telling you which `raco` command to run.
- **Check-syntax warnings** for unused variables and unused `require` forms. These only appear after a successful expansion. Powered by DrRacket's `check-syntax`.
- **Typed Racket type errors** produced during expansion by the type checker. The server reads these from Typed Racket's online-check-syntax tooltip channel (same source as inferred types on hover).
- **Language declaration check** warns about missing `#lang` lines or unrecognized language names. Works without expansion.

Language behavior: mostly not filtered by language family. Reader errors, expander errors, and check-syntax warnings work for any language that reads and expands. The language declaration check only recognizes the predefined language families, so other valid `#lang` names are reported as unrecognized. Typed Racket type errors are specific to Typed Racket.

## Document Highlight *(requires expansion)*

Placing the cursor on an identifier highlights all of its occurrences in the file. Both the definition site and all usage sites light up. The server uses `check-syntax` to find which declaration the identifier at the cursor resolves to, then looks up every location in the file that refers to the same declaration.

Language behavior: not filtered by language family. Works where expansion succeeds and check-syntax produces binding data with reliable source ranges.

## Document Symbols *(no expansion)*

Shows a document outline. The server uses the lexer to produce results. It scans the text for every symbol (identifier), string, and constant and lists each one as a symbol entry with a kind label. This means every occurrence is listed rather than just top-level definitions, making the outline noisy and currently not very useful for navigation.

Language behavior: not filtered by language family. The server does not actively suppress entries for any language family, but actual entries depend on what the lexer can tokenize.

## Formatting *(no expansion)*

Indents Racket code by calling an external formatter. Currently uses [racket-fixw](https://github.com/6cdh/racket-fixw). Works for recognized sexp language families. Does not change anything for other languages.

Three trigger modes are supported:

- Format document - indents the whole file.
- Format selection - indents only the selected lines.
- Format on type - indents when you press `)`, `]`, or Enter. Pressing `)` or `]` re-indents the enclosing form; pressing Enter re-indents the current line.

Language behavior: only recognized sexp language families are supported. Other languages return no edits.

## Hover *(requires expansion)*

Hovering at a position shows a fixed Markdown card. The renderer only chooses
whether each slot appears and where it appears; slot text stays verbatim from
its source.

Slots, top to bottom:

1. **Type** - Typed Racket inferred type in a `racket` fence, labeled `Type` or
   `Type (stale)` when it comes from a retained trace. Always fenced; never
   inlined by length.
2. **Source / Signature** - same-file source snippet when the trace confirms a local
   declaration or use, otherwise a Scribble bluebox / docs signature. Source
   wins when both exist. Local uses share declaration detail. Display prefers
   the outermost same-line structural candidate when one exists: a compact
   binding clause, a collapsed one-line header, or a complete one-line form.
   Otherwise it shows the full nearest enclosing form. Contiguous own-line
   leading comments above the form are included. Racket-family forms use a
   `racket` fence. Rhombus forms use a `rhombus` fence. This slot is always
   labeled `Source` or `Signature`.
3. **Annotation** - the winning Check Syntax callback or logged tooltip,
   labeled `Mouse-over status` or `Log tooltip`. At every character, the
   narrowest source range wins; equal-width conflicts prefer the annotation
   collected later.
4. **Documentation** - online docs link, then locally installed docs excerpt
   when available, always labeled `Documentation`.

Language behavior: source forms are read from the current buffer using kept
trace ranges while a refresh runs. When the trace is old, retained types stay
visible as `Type (stale)`. The shown form or type can be useful, but the
binding link or type may be wrong until expansion finishes. The hover range
prefers the inferred-type interval when present (including literal and
expression-delimiter spans); otherwise it uses the winning annotation or a
kept same-file source-detail range. Code context is limited to 10 lines
and 1000 source characters. Leading comments are limited to 10 lines and 200
source characters per line. Other hover data works where expansion succeeds
and check-syntax produces hover and documentation data with reliable source
ranges.

## Inlay Hints

A handler is registered, but it is just a stub, not yet implemented.

## References *(requires expansion)*

Finds all references to the identifier under the cursor. Local references are always found via the `syncheck:add-jump-to-definition` and `syncheck:add-arrow/name-dup` check-syntax callbacks. Cross-file references only shows for identifiers that were referenced by a file in the workspace that has been opened and expanded, unopened files are not scanned.

Language behavior: not filtered by language family. Works where expansion succeeds and check-syntax produces binding data with reliable source ranges. Cross-file references are limited to files that have been opened and expanded in the workspace.

## Rename *(requires expansion)*

Renames an identifier and all its uses within the current file. Collects the declaration position and all binding positions from check-syntax, then replaces each with the new name. Only identifiers defined in the current file can be renamed.

Language behavior: not filtered by language family. Works where expansion succeeds and check-syntax produces binding data with reliable source ranges.

## Prepare Rename *(requires expansion)*

Called by the editor before the rename dialog opens to check whether renaming is allowed at the cursor position.

Language behavior: same as Rename.

## Semantic Tokens *(expansion for 2 of 3 sources)*

Provides semantic syntax highlighting with token types like function, variable, string, number, and comment. Combines analysis from these sources:

- DrRacket-style highlighting from check-syntax (needs expansion).
- Traverse the syntax tree (needs expansion).
- Sexp comment detection via the lexer, works without expansion.

Supports highlighting the full document or a specified range. Delta (incremental) highlighting is not yet implemented.

Semantic tokens depend on expansion and lexer data. Each request waits for any pending expansion to finish before responding. If expansion succeeds, fresh expansion-based tokens are used. If expansion fails, the last successful expansion tokens may remain available with adjusted ranges, and current lexer-derived sexp-comment tokens can still appear. If no expansion has ever succeeded, only lexer-derived tokens can appear.

In Lisp, due to dynamic typing and the S-expression syntax, many identifiers share the same token type and look similar, so it is recommended to use a semantic token aware editor plugin that gives each different identifier a unique color.

Language behavior: check-syntax color tokens are not filtered by language family and work where expansion succeeds. Sexp-comment tokens are available when the structural lexer can parse s-expression code.

## Signature Help *(requires expansion)*

Shows signature information when you are inside a form. The server finds the head symbol of the enclosing s-expression form and looks up its signatures using locally installed documentation. If expansion is in progress or has failed, the last successful result is used as a fallback. Powered by DrRacket's `check-syntax`.

Language behavior: sexp-specific. Callee detection relies on s-expression form-head lookup through the structural forest. Known non-sexp languages return no result.
