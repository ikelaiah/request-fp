# Contributing to Request-FP

Thank you for helping make Request-FP easier and more reliable.

## Prerequisites

- Free Pascal 3.2.2+
- Lazarus 4.8+
- `lazbuild` available on `PATH`
- OpenSSL libraries for HTTPS integration tests
- Git

The project is currently tested on Windows and Linux.

## Get started

1. Fork and clone the repository:

   ```bash
   git clone https://github.com/YOUR-USERNAME/request-fp.git
   cd request-fp
   ```

2. Create a branch:

   ```bash
   git switch -c feature/short-description
   ```

3. Make the change, add tests where appropriate, and update the relevant
   documentation.

## Build every example

The bulk-build scripts discover all Lazarus projects below `examples/`, skip
backup directories, clean `example-bin/`, and build everything in Release
mode.

Windows PowerShell:

```powershell
.\build-examples.ps1
```

If required by the local execution policy:

```powershell
powershell -ExecutionPolicy Bypass -File .\build-examples.ps1
```

Linux or Git Bash:

```bash
bash ./build-examples.sh
```

Successful builds are placed in `example-bin/`, which is intentionally ignored
by Git.

## Build and run the tests

Compile the FPCUnit runner:

```bash
lazbuild --build-mode=Release tests/TestRunner.lpi
```

Run it on Windows:

```powershell
.\tests\TestRunner.exe -a --format=plain
```

Run it on Linux:

```bash
./tests/TestRunner -a --format=plain
```

The integration suite calls `https://httpbin.org` and requires outbound
network access. A transient upstream failure can occasionally require a
rerun.

## Code guidelines

- Use two spaces for indentation and no tabs.
- Follow existing Object Pascal naming and layout.
- Keep public APIs small, explicit, and backward compatible where practical.
- Use `try..finally` for owned resources.
- Raise `ERequestError` with actionable messages for request failures.
- Document new public APIs and add a short usage example.
- Prefer focused changes over unrelated cleanup.

## Documentation checklist

For user-facing changes, review:

- `README.md`
- `docs/Request.md`
- `docs/Request.Session.md`
- `docs/cheat-sheet.md`
- relevant programs under `examples/`
- `CHANGELOG.md`
- `docs/RELEASE-vX.Y.Z.md`
- `docs/PR-vX.Y.Z.md`

## Commit messages

Use a concise Conventional Commit subject:

```text
feat: add request timeout options
fix: preserve response headers across redirects
docs: clarify OpenSSL setup on Windows
```

Add a body when several user-visible changes need explanation.

## Pull request checklist

Before opening a pull request:

- [ ] The test project compiles.
- [ ] The relevant tests pass.
- [ ] Both platform scripts remain syntactically valid.
- [ ] All examples compile with the bulk-build script.
- [ ] Public API changes are documented.
- [ ] `CHANGELOG.md` is updated.
- [ ] No generated files from `example-bin/` are committed.
- [ ] `git diff --check` passes.

## Reporting bugs

Include:

- operating system;
- FPC and Lazarus versions;
- Request-FP version;
- a minimal reproducible example;
- the full exception message; and
- OpenSSL version/architecture when HTTPS is involved.

Please follow the repository issue template when one is available.

## Code of conduct and license

Contributors must follow [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md). By
contributing, you agree that your work is licensed under the
[MIT License](LICENSE.md).
