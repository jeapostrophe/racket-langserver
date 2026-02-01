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
We use [fixw](https://docs.racket-lang.org/fixw/index.html). Install and run it manually:
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
