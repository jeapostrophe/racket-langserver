# Test Suite Guide

## Choosing a Test Harness

| Test area | Primary helper | Notes |
| --- | --- | --- |
| `tests/textDocument/` | [`tests/textDocument/with-document.rkt`](textDocument/with-document.rkt) | `with-document` and `lsp-result!` (checks `"jsonrpc": "2.0"` and errors); re-exports `change!`, `notify`, and `client-wait-notification` |
| `tests/lsp/` | [`tests/lsp/controller-support.rkt`](lsp/controller-support.rkt) | Re-exports `with-session` and session ops alongside `barrier`, `Gate`, `with-temp-dir`, and `call-with-resyntax-enabled`; installs the silent logger |
| `tests/lifecycle/` | [`tests/stdio-server.rkt`](stdio-server.rkt) | Subprocess stdio harness (`call-with-stdio-client`, `stdio-initialize!`, `stdio-shutdown-and-exit!`) with strict `Content-Length` framing and JSON-RPC validation |
| `tests/lib/` (`doc-*`) | [`tests/lib/doc-test-support.rkt`](lib/doc-test-support.rkt) | `make-trace`, `check-syntax-diagnostics`, `typed-racket-available?`, `make-expanded-doc` |
| `tests/lib/` (Resyntax place) | [`tests/lib/resyntax-place-test-support.rkt`](lib/resyntax-place-test-support.rkt) | Controlled worker-place factory (`call-with-controlled-workers`, `reset-resyntax-worker!`) |

## Shared Primitives & Conventions

- [`tests/client.rkt`](client.rkt) — Underlying in-process `Session` runner (`with-racket-lsp`, `with-session`) used by `with-document.rkt` and `controller-support.rkt`; use directly only when neither wrapper fits (e.g. `tests/sync/`).
- [`tests/json-dsl.rkt`](json-dsl.rkt) — Compile-time `json` literal macro (`{#:k v ...}`, `[e ...]`, `null`) expanding directly to `hasheq`/`list`/`json-null`. Prefer this over production structs/`->jsexpr` for protocol expectations so serializer or enum regressions are caught.
- [`tests/silent-logger.rkt`](silent-logger.rkt) — Require before production modules inside `(module+ test ...)` in new standalone tests to keep `racket-langserver` logs off stderr while keeping direct log receivers working.
- [`tests/textDocument/inlay-hint-support.rkt`](textDocument/inlay-hint-support.rkt) — `inlay-hints-for-range` and `hint->triple` for inlay-hint tests.
