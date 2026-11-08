# Request-FP v1.5.0 release notes

**Release date:** November 9, 2026

Feature release. No API removals.

## New

```pascal
// Stream a large file to disk, with progress
Response := Http.Download(URL, 'big.zip', @ShowProgress);
Response := Http.GetToStream(URL, MyStream);

// Retries are on by default; tune or disable them
Http.SetRetry(5, 100);
Http.SetRetry(0, 0);

// Auth, JSON, and header helpers
Response := Http.Get(URL, [KVBearer(Token)]);
Response := Http.PutJSON(URL, '{"name":"Ada"}');
Values := Response.HeaderValues('Set-Cookie');

// Connection settings
Http.SetConnectTimeout(5000);
Http.SetProxy('proxy.local', 3128);

// Sessions
Session.SetBearerToken(Token);
Response := Session.Download('/backup.zip', 'backup.zip');
```

- **Streaming downloads:** only a 2xx body is saved, to `file.part`, renamed
  when complete. Errors, cancels, and short bodies never leave a
  half-written file.
- **Streaming uploads:** `PostMultipart` reads files from disk while sending
  instead of loading them into memory.
- **Retries:** GET, HEAD, PUT, and DELETE retry twice (250 ms, then 500 ms) on
  network errors, 429, and 503, honouring `Retry-After`. POST and PATCH are
  never retried.
- **Session cookies** follow `Domain`, `Path`, `Secure`, `Max-Age`, and
  `Expires`.
- **Helpers:** `KVBasic` / `KVBearer`, `PutJSON` / `PatchJSON`,
  `HeaderValues` / `HeaderNames`, `SetConnectTimeout`, `SetProxy` (http://
  only).
- **macOS:** Homebrew and MacPorts OpenSSL 3 are found automatically; CI
  tests on macOS.
- **Linux:** the OpenSSL 3 runtime (`libssl3`) is enough; no `-dev` package.
- **Documentation site** at
  [ikelaiah.github.io/request-fp](https://ikelaiah.github.io/request-fp/)
  with Getting started, How it works, and diagrams.
- **Examples:** `streaming_download`, `session_login_redirect`,
  `typed_errors`; `retry_on_error` rewritten.

## Fixes

- **Credentials:** `Authorization` is no longer sent when a redirect moves to
  another host.
- **Truncated downloads:** a body shorter than `Content-Length` raises
  `ENetworkError` instead of being accepted.
- **Empty responses:** a server closing without replying raises
  `ENetworkError` instead of returning status 0.
- **Timeouts** close to the limit are reported as `ETimeoutError` on Windows.

## Behavior changes

- Retries are on by default, so an unreachable server fails after three
  connection attempts plus 750 ms of waiting. `Http.SetRetry(0, 0)` restores
  the old behavior.
- Session cookies from servers are only sent to matching hosts and paths.
  Cookies set with `SetCookie` are still sent everywhere.
- `https://` requests raise `ERequestError` while a proxy is set.

## For contributors

- `run-tests.sh` / `run-tests.ps1` build and run everything against a local
  test server; `--leak-check` fails on unfreed memory. CI runs both on Linux,
  Windows, and macOS.
- The examples and tests build with plain `fpc` when Lazarus is not
  installed.

## Known limits

- HTTPS through a proxy is not supported (FPC 3.2.2 has no CONNECT tunnel).
- Cookie domains are checked without a public-suffix list.
- Timeouts during reads are detected by elapsed time, because FPC reports
  them as a generic socket read error.
