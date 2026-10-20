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
We use [`fixw`](https://docs.racket-lang.org/fixw/index.html) ([6cdh/racket-fixw](https://github.com/6cdh/racket-fixw)). Custom indentation rules are defined in [`.lispwords`](.lispwords).

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
- **Verify your commits.** Commits must show as [verified](https://docs.github.com/en/authentication/managing-commit-signature-verification/about-commit-signature-verification) on GitHub (GPG or SSH signing) before a pull request can be merged. See [signing commits](https://docs.github.com/en/authentication/managing-commit-signature-verification/signing-commits) and [generating a new SSH key for signing](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent#generating-a-new-ssh-key).

## Notes for Contributors

It is useful to think of this project as a "headless mode" for DrRacket. Contributions to this project should seek to avoid re-implementing functionality that is already exposed through DrRacket's public API.

Check-syntax and related DrRacket tools still analyze one file per expansion, but this server aggregates accepted per-file contributions for cross-file references, definitions, and hover. Workspace-wide search (for example `workspace/symbol`) is not implemented. Broader multi-file tooling in DrRacket itself remains a useful upstream direction before expanding that surface here.

[jeapostrophe commented on Apr 29, 2020](https://github.com/jeapostrophe/racket-langserver/issues/8#issuecomment-621242014)
> I think that the right way to implement most features in Racket-LSP is to find the corresponding feature in DrRacket and then disentangle from DrR's GUI and then expose the feature through the LSP. In many cases, DrR has already been internally organized to do that, but we just haven't done enough spelunking yet.

