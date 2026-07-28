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

## Documentation changes

- Rewrites the README as a common-task quick start.
- Rewrites the stateless reference, session guide, and cheat sheet.
- Updates shipped examples to use v1.3.0's shortest syntax.
- Adds release notes and a complete changelog entry.
- Keeps detailed TLS setup in the dedicated SSL documentation.

## Compatibility

The release is additive and backward compatible. Existing overloads,
`TKeyValue.Create`, and explicit `Session.Init` calls remain supported.

## Validation

- Compile the FPCUnit project in Release mode.
- Run the complete integration suite against `https://httpbin.org`.
- Compile all example projects.
- Check the final diff for stale 1.2.0 version references and mandatory
  `Session.Init` guidance.
