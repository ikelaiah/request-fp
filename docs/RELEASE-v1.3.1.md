# Request-FP v1.3.1 release notes

**Release date:** November 8, 2026

Bug-fix release. No API removals.

## Behavior changes

- **Sessions return every status.** `THttpSession.Get/Post/PostJSON/Put/Delete`
  no longer raise for statuses such as 201, 202, 404, or 500. Check
  `Response.OK` or call `Response.RaiseForStatus`, as with `Http.*`.
- **Sessions raise `ERequestError` for transport failures**, including
  `Post`/`PostJSON`, which previously leaked FPC exception types.
- **OpenSSL loads lazily.** It loads on the first HTTPS request, so HTTP-only
  programs start without OpenSSL. Call `EnsureSSL` at startup to fail fast.
- **No console output from the library** outside `DEBUG` builds.

## Fixes

- `Response.SaveToFile` no longer corrupts binary bodies.
- Unix now treats a failed `InitSSLInterface` as an error.
- Removed a test-mode shortcut that could downgrade `https://httpbin.org`
  requests to HTTP.

## Upgrading

- Code that relied on a session raising for 4xx/5xx should call
  `Response.RaiseForStatus`.
- Handlers that caught `Exception` around session calls keep working; they can
  narrow to `ERequestError`.
