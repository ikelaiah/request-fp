# Pull request: v1.3.0 ease of use

## Summary

This change makes Request-FP's common paths shorter and more explicit without
removing or changing existing APIs.

## User-facing changes

- Adds `KV` shorthand.
- Adds `GetWithParams` / `TryGetWithParams`.
- Adds encoded `PostForm` / `TryPostForm`.
- Adds `TJSONData` overloads and `TryPostJSON`.
- Adds `Response.OK`, `Result.OK`, and `Response.RaiseForStatus`.
- Makes sessions usable without an explicit `Init`.
- Adds `Session.PostJSON`.
- Normalizes base URL/path slash joining.
- Adds PowerShell and Bash scripts that compile all examples into
  `example-bin/`.
- Makes OpenSSL 3 load correctly on Windows with FPC 3.2.2 while retaining
  FPC's OpenSSL 1.1 fallback.
- Uses a deterministic local HTTP fixture for CI integration tests.

## Documentation changes

- Rewrites the README as a common-task quick start.
- Rewrites the stateless reference, session guide, and cheat sheet.
- Updates shipped examples to use v1.3.0's shortest syntax.
- Adds release notes and a complete changelog entry.
- Keeps detailed TLS setup in the dedicated SSL documentation.
- Corrects the OpenSSL version guide's description of FPC 3.2.2 and removes
  unsafe advice to rename DLLs in `System32`.
- Documents the Windows OpenSSL 3 filename override in the README, SSL guide,
  version-selection guide, and technical details.
- Documents the bulk example build workflow for users and contributors.

## Compatibility

The release is additive and backward compatible. Existing overloads,
`TKeyValue.Create`, and explicit `Session.Init` calls remain supported.

## Validation

- Compile the FPCUnit project in Release mode.
- Run the complete 44-test integration suite against the local HTTP fixture
  on Linux and Windows.
- Verify OpenSSL initialization with Lazarus 4.8/FPC 3.2.2 and OpenSSL 3 on
  Windows.
- Compile all example projects.
- Verify both example build scripts produce all executables in `example-bin/`.
- Check the final diff for stale 1.2.0 version references and mandatory
  `Session.Init` guidance.
