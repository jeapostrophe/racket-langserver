# Contributing

Thank you for contributing to `racket-langserver`!

## Setup

1. **Clone**
   ```bash
   git clone https://github.com/jeapostrophe/racket-langserver.git
   cd racket-langserver
   ```

2. **Install**
   Standard (installs dependencies and compiles project):
   ```bash
   raco pkg install --auto
   # Or with a custom package name:
   raco pkg install --auto -n my-package-name
   ```

## Compilation

The project is automatically compiled during installation (`raco pkg install`).

If you modify files, Racket will automatically use the updated source code (ignoring the older bytecode). To improve startup time after changes, you can optionally:

Compile specific file (and its dependencies):

```bash
raco make path/to/file.rkt
```

Compile entire project:

```bash
raco make **/*.rkt
```

## Formatting
We use [`fixw`](https://docs.racket-lang.org/fixw/index.html) ([6cdh/fixw](https://github.com/6cdh/fixw)). Custom indentation rules are defined in [`.lispwords`](.lispwords).

Run it manually:
```bash
raco pkg install fixw
raco fixw .
```
Or use [pre-commit](https://pre-commit.com/) (the repository provides the necessary `.pre-commit-config.yaml`):
```bash
pip install pre-commit
pre-commit install
```

## Testing
```bash
raco test tests/
```
In headless environments, use `xvfb-run`.

## Pull Requests
- Format code with `fixw`.
- Ensure tests pass.
- CI checks builds, tests, formatting, and `resyntax` integration.

## Notes for Contributers

It is useful to think of this project as a "headless mode" for DrRacket. Contributions to this project should seek to avoid re-implementing functionality that is already exposed through DrRacket's public API.

Currently, we do not support  workspace-wide (or project-wide) methods because the underlying DrRacket code tools only operate on one file at a time. If multi-file code tools are a desirable feature, then they should be considered for inclusion into [DrRacket proper](https://github.com/racket/drracket) before being implemented in this project.

[jeapostrophe commented on Apr 29, 2020](https://github.com/jeapostrophe/racket-langserver/issues/8#issuecomment-621242014)
> I think that the right way to implement most features in Racket-LSP is to find the corresponding feature in DrRacket and then disentangle from DrR's GUI and then expose the feature through the LSP. In many cases, DrR has already been internally organized to do that, but we just haven't done enough spelunking yet.

