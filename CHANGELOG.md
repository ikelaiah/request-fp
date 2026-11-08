# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added

- Streaming downloads: `Http.Download(URL, FilePath)` and
  `Http.GetToStream(URL, Stream)`, with `Try*`, header/parameter, and progress
  overloads, plus session `Download`/`GetToStream`. Only a 2xx body is saved;
  files are written to `.part` and replaced when complete; a body shorter
  than `Content-Length` raises `ENetworkError`.
- `TProgressProc` / `TProgressEvent` progress callbacks; raise to cancel.
- Automatic retries, on by default: GET, HEAD, PUT, and DELETE are retried
  twice (250 ms, then 500 ms) on network errors (not timeouts), 429, and 503,
  honouring `Retry-After`. POST and PATCH are never retried.
  `Http.SetRetry` / `Session.SetRetry` change it; `SetRetry(0, 0)` disables.
- `TryParseHTTPDate` building block.
- Auth helpers: `KVBasic(User, Pass)` and `KVBearer(Token)`, and session
  `SetBasicAuth` / `SetBearerToken`.
- `Http.PutJSON` / `PatchJSON` (with `Try*` variants) and session
  `PutJSON` / `PatchJSON` / `TryPutJSON` / `TryPatchJSON`.
- `Response.HeaderValues(Name)` and `Response.HeaderNames`.
- `Http.SetConnectTimeout` / `GetConnectTimeout` and session
  `SetConnectTimeout` (default 3000 ms).
- `Http.SetProxy` / session `SetProxy` for `http://` URLs, with optional proxy
  credentials. `https://` requests raise `ERequestError` while a proxy is
  set, because FPC 3.2.2 cannot tunnel HTTPS.
- Session cookies honour `Domain`, `Path`, `Secure`, `Max-Age`, and `Expires`
  (RFC 6265), via the public `TCookieJar`. `SetCookie` cookies stay global.
- Examples: `streaming_download`, `session_login_redirect`, `typed_errors`.
- macOS support: Request-FP finds Homebrew or MacPorts OpenSSL 3, and CI
  builds and tests on macOS with FPC 3.2.2.
- `run-tests.sh` and `run-tests.ps1`: build the suite, start the local test
  server, and run all tests; `--leak-check` / `-LeakCheck` fails on unfreed
  memory.
- `fpc` fallback for the run scripts and `build-examples.sh` when Lazarus is
  not installed.
- CI runs a heaptrc leak check on Linux, Windows, and macOS.

### Changed

- Multipart uploads (`PostMultipart`, `TryPostMultipart`, session
  `PostMultipart`) stream files from disk instead of loading them into
  memory. `BuildMultipartBody` returns a read-only stream that keeps the files
  open until it is freed.
- Linux needs only the OpenSSL 3 runtime (`libssl3`), not the `-dev` package:
  `.3` is added to FPC 3.2.2's library version search.
- Tests default to the local test server instead of `https://httpbin.org`.
- Session cookies from servers are no longer sent to every host and path; see
  Added. Cookies set with `SetCookie` behave as before.
- `retry_on_error` example shows the built-in retries.

### Fixed

- `Authorization` headers were resent when a redirect went to another host;
  they are now dropped (stateless and session requests).
- A connection closed without any response returned status 0 instead of
  raising; it now raises `ENetworkError`.
- Timeouts close to the configured limit were sometimes reported as
  `ENetworkError` instead of `ETimeoutError` on Windows, because of the
  system timer's ~16 ms resolution.

## [1.4.0] - 2026-11-09

### Added

- `Http.Patch`, `Http.Head`, `Http.TryPatch`, and `Http.TryHead`.
- `Http.SetTimeout` / `Http.GetTimeout` for the stateless I/O timeout.
- `ENetworkError` and `ETimeoutError` (subclasses of `ERequestError`).
- `Response.Bytes` and `Response.SaveToStream`.
- Session `Patch`, `Head`, `PostForm`, `PostMultipart`, `GetWithParams`,
  `Get(URL, Headers)`, and the general `Send(Method, URL, Body, ContentType,
  Headers, Params)`.
- Session `Try*` variants: `TryGet`, `TryPost`, `TryPostJSON`, `TryPostForm`,
  `TryPostMultipart`, `TryPut`, `TryPatch`, `TryDelete`, `TryHead`, `TrySend`.
- Public building blocks shared by both APIs: `ExecuteRequest`,
  `AppendQueryParams`, `EncodeFormFields`, `BuildMultipartBody`.
- Fixture endpoints and tests for PATCH, HEAD, timeouts, typed errors, bytes,
  multipart encoding, server cookies, cookie expiry, and redirects.

### Changed

- All stateless methods share one request pipeline; `Request.pas` is about
  250 lines shorter.
- Stateless requests now default to a 30-second I/O timeout (previously no
  limit). Use `Http.SetTimeout(0)` for the old behavior.
- Session transport errors use the same messages as the stateless API
  (`Network Error: ...`) instead of `GET request failed: ...`.
- `PostJSON` and `PostMultipart` no longer add a second `Content-Type` when
  you pass one in `Headers`.
- Multipart uploads send a per-file `Content-Type` based on the extension,
  percent-encode quotes and line breaks in names, and use a new random
  boundary for every request.
- `PostForm` with no fields sends an empty form body.

### Fixed

- Session cookies set on redirect responses (for example after a login POST)
  are now kept and sent on the next hop.
- Session cookies with `Max-Age=0` or a past `Expires` are now removed.
- Sessions no longer send their cookies when a redirect goes to another host.
- Sessions no longer send a second `Cookie` header with cookies FPC kept from
  the previous response, so `ClearCookies` fully clears cookies.
- A missing multipart upload file raises `ERequestError` instead of FPC's
  `EFOpenError`.

## [1.3.1] - 2026-11-08

### Added

- `EnsureSSL` to load OpenSSL up front and fail fast when it is missing.
- Integration tests for session status codes, session transport errors, and
  binary `SaveToFile`.

### Changed

- OpenSSL now loads on the first HTTPS request instead of at program start.
  HTTP-only programs no longer fail to start when OpenSSL is missing.
- The library no longer writes OpenSSL errors to the console outside `DEBUG`
  builds; the same text is in the raised `ERequestError`.

### Fixed

- `THttpSession` methods now return any HTTP status instead of raising.
  Previously `Get` raised for anything but 200, `Post`/`Put` for anything but
  200/201/204, and `Delete` for anything but 200/204.
- `Session.Post` and `Session.PostJSON` now raise `ERequestError` for
  transport failures instead of raw FPC exceptions.
- `Response.SaveToFile` now writes the body bytes unchanged. Binary downloads
  were previously re-encoded and corrupted.
- Unix OpenSSL initialization now treats a `False` result from
  `InitSSLInterface` as a failure, matching Windows.
- Removed a test-only shortcut that silently downgraded `https://httpbin.org`
  requests to HTTP in programs whose name contained "test".
- Removed stale backup sources and a leftover `.new` test file from the repo.

## [1.3.0] - 2026-07-29

### Added

- `KV(Key, Value)` shorthand for headers, query parameters, form fields, and
  multipart values.
- `Http.GetWithParams` and `Http.TryGetWithParams` so query-only GET requests
  no longer need an empty headers array.
- `Http.PostForm` and `Http.TryPostForm`, with automatic UTF-8 percent-encoding
  for `application/x-www-form-urlencoded` bodies.
- `Http.PostJSON` overloads that accept `TJSONData` directly.
- `Http.TryPostJSON` overloads for both JSON strings and `TJSONData`.
- `Response.OK` and `Result.OK` convenience properties. `Response.OK` checks
  for a 2xx status; `Result.OK` checks both transport success and 2xx status.
- `Response.RaiseForStatus` for opting in to exceptions on non-2xx responses.
- `Session.PostJSON` overloads for JSON strings and `TJSONData`.
- New SVG project logo displayed in the README.
- `build-examples.ps1` and `build-examples.sh` scripts that compile every
  Lazarus example in Release mode into `example-bin/`.

### Changed

- `THttpSession` is now documented and tested as automatically initialized.
  `Session.Init` remains available as an optional reset operation.
- Session base URLs and relative paths now join correctly with or without
  leading/trailing slashes.
- Updated the version constant and default user agent to 1.3.0.
- Reworked the README, API reference, session guide, and cheat sheet around
  short, copyable common-task examples and a consistent error-handling guide.
- Reduced setup noise in the main README while retaining detailed SSL
  diagnostics in the dedicated guides.
- Reworked the Windows OpenSSL documentation to explain FPC 3.2.2's DLL-name
  behavior, Request-FP's automatic compatibility override, and safe
  deployment without modifying `System32`.
- Added a one-command example build workflow to the README, examples guide,
  contributor guide, release notes, and pull request notes.
- Made CI integration tests self-contained with a local HTTP fixture instead
  of relying on a public test service.

### Fixed

- `Session.Init` now frees an existing client, clears headers and cookies, and
  uses the current version in the default user agent.
- Corrected case-sensitive documentation links to `docs/cheat-sheet.md`.
- Windows builds with FPC 3.2.2 now try the standard OpenSSL 3 DLL names
  before FPC's existing OpenSSL 1.1 fallback names.
- SSL initialization now treats a `False` result from `InitSSLInterface` as a
  failure instead of marking OpenSSL as initialized.


## [1.2.0] - 2026-01-01

### Added

- **Architecture Detection in ssl_debug**: ssl_debug example now displays executable architecture (32-bit vs 64-bit) and required DLL names upfront
- **Architecture-Specific Error Messages**: InitSSL error messages now explicitly state whether 32-bit or 64-bit DLLs are required based on executable architecture
- **Dynamic DLL Detection**: New `FindSSLDLLPath` function using `EnumProcessModules` to show which OpenSSL DLL FPC actually loaded

### Changed

- **Enhanced ssl_debug Output**: Now shows "Executable architecture: 64-bit" and "Required DLL names: libssl-*-x64.dll and libcrypto-*-x64.dll" before attempting HTTPS request
- **Improved Error Messages**: Added "IMPORTANT: Ensure DLL architecture (32-bit vs 64-bit) matches your executable!" warning to help diagnose architecture mismatch issues
- **Debug Mode DLL Path Detection**: Uses dynamic module enumeration to find and display which OpenSSL DLL FPC loaded, revealing version disguises

### Fixed

- **Architecture Mismatch Detection**: Users with mismatched executable/DLL architectures now get clear error messages explaining the issue instead of cryptic "Error loading library" failures
- **fpcupdeluxe Architecture Documentation**: Added documentation clarifying that fpcupdeluxe defaults to 32-bit FPC (common for lower memory footprint), which requires matching 32-bit OpenSSL DLLs
- **Vendor Version Disguise Detection**: Dynamic detection reveals when vendors disguise OpenSSL 3.x as 1.1.x by showing both filename and actual version string


## [1.1.0] - 2026-01-01

### Added

- **Windows OpenSSL Documentation**: Comprehensive Windows-specific OpenSSL installation instructions with multiple methods (Chocolatey, Scoop, manual installer from Shining Light Productions)
- **Troubleshooting Guide**: New troubleshooting section in README.md with specific DLL names for OpenSSL 1.1.x and 3.x (both 32-bit and 64-bit variants)
- **DEBUG Mode**: Added debug mode support to show OpenSSL version and DLL paths at runtime
  - Shows exact OpenSSL version loaded
  - Displays full path to loaded libssl and libcrypto DLLs
  - Helps diagnose version conflicts when multiple OpenSSL installations exist
- **SSL Initialization Test**: Added `Test25_SSLInitialization` to verify SSL setup with helpful Windows-specific error messages
- **SSL Debug Example**: Created `examples/ssl_debug/` with pre-configured Debug build mode to help users troubleshoot OpenSSL issues
- **Documentation**:
  - `docs/SSL-HTTPS-GUIDE.md`: Complete guide on HTTPS enforcement strategies and debug mode usage
  - `docs/OPENSSL-VERSION-SELECTION.md`: Guide for resolving conflicts when multiple OpenSSL versions are installed
  - Updated `docs/Request.md` and `docs/Request.Session.md` with Windows OpenSSL setup instructions

### Changed

- **Enhanced InitSSL Error Messages**: Windows error messages now include specific installation instructions, DLL names, and links to installers
- **Improved Linux Error Messages**: Added Fedora/RHEL installation commands alongside Ubuntu/Debian instructions
- **README.md**: Replaced ambiguous "Windows: Usually included" with clear, actionable installation steps
- **Request.pas**: Added Windows API integration to detect loaded DLL paths in debug mode

### Fixed

- Addressed user confusion about Windows OpenSSL setup after fresh FPC/Lazarus installation via FPCUpDeluxe
- Resolved documentation gap for users encountering "OpenSSL initialization failed" errors on Windows


## [1.0.0] - 2025-09-08

### Added

- Test suite enhancements and coverage: URL encoding, JSON access errors, Try* behavior, and header extraction via `Response.HeaderValue`.
 - Request API: `TryPostMultipart(...)` with ergonomic overloads, mirroring existing `Try*` patterns. Provides exception-free multipart uploads returning `TRequestResult`.
 - Tests: Added success and failure coverage for `TryPostMultipart` in `tests/Request.Test.pas`.
 - Session tests: Added multipart upload success/failure tests using `THttpSession.Post` with a manually constructed multipart body.
 - `TResponse` helpers:
  - `IsSuccessStatus` to quickly check for 2xx responses.
  - `SaveToFile(FilePath)` to persist the response body as UTF-8 bytes.
  - `SetHeadersText` (internal) to allow session API to populate response headers.
- Session API now captures response headers into `TResponse` so `Response.HeaderValue(...)` works consistently across stateless and session styles.
- Tests:
  - `Test23_IsSuccessStatus` and `Test24_SaveToFile` in `tests/Request.Test.pas`.
  - `Test26_Session_ResponseHeaderValue` in `tests/Request.Session.Test.pas`.

### Changed

- Documentation updates across `README.md`, `docs/Request.md`, `docs/Request.Session.md`, and `docs/Cheat-Sheet.md`:
  - Clarified Try* behavior (never raises; uses `TRequestResult`).
  - Documented JSON parse errors raising `ERequestError` with a clear prefix.
  - Added examples for `Response.HeaderValue()`.
  - Added CI/testing notes about `httpbin.org` usage and intermittent upstream 502.
  
  
- Query parameter encoding now follows strict RFC 3986 over UTF-8 bytes for consistent behavior on Windows and Linux.
 - Documentation updates to include `TryPostMultipart` usage and behavior across `docs/Request.md`, `docs/Request.Session.md`, and `docs/Cheat-Sheet.md`.

### Fixed

- Stabilized flaky HTTP tests by adding a minimal one-time retry on transient HTTP 502 responses in tests only (no change to core library behavior).
- Fixed UTF-8 URL parameter percent-encoding on Linux; now percent-encoding and decoding are identical across platforms.
- Fixed HTTP response body decoding by reading raw bytes and explicitly treating them as UTF-8, preventing mojibake when parsing JSON.
- Ensured JSON-related tests involving Unicode characters pass consistently on both Windows and Ubuntu.
 - Stabilized multipart tests against intermittent httpbin 502 by adding a one-time retry in the test code (no internal retry in the library).

## [0.6.0] - 2025-07-01

### Added

- **Procedural API Refactor**: Complete refactoring to use only a procedural API (removed all fluent/builder pattern code).
- **Custom Headers & Query Parameters**: Added support for custom headers and query parameters to all procedural/stateless HTTP methods.
- **Ergonomic TKeyValue.Create**: Added `TKeyValue.Create(key, value)` helper constructor for cleaner syntax when creating key-value pairs.
- **Method Overloads**: Added ergonomic overloads for all HTTP methods so users don't need to always specify empty arrays for headers/params.
- **Enhanced Error Handling**: Improved error handling with proper const parameter management and better exception messages.

### Changed

- **Breaking**: Removed all fluent/builder API code and references - now uses only procedural API.
- **Breaking**: All HTTP methods now require explicit `Headers` and `Params` parameters (with convenient overloads available).
- **API Improvement**: Simplified syntax for creating custom headers and parameters using `TKeyValue.Create()`.
- **Memory Management**: Fixed all const parameter assignment issues and improved memory safety.
- **Documentation**: Updated all documentation, examples, and tests to reflect the new procedural-only API.

### Fixed

- **Compilation Issues**: Fixed all "Can't assign values to const variable" errors.
- **Method Signatures**: Corrected all "Wrong number of parameters" errors in HTTPMethod calls.
- **Multipart POST**: Fixed multipart POST to correctly capture response body and handle request streams.
- **Request Body Handling**: Fixed POST, PUT, and PostJSON to properly send request data using TStringStream.
- **Response Reading**: Ensured all HTTP methods read response body from the correct response stream.

## [0.5.0] - 2025-06-30

### Added

- Initial public release of Request-FP.
- Stateless HTTP API (`Request.pas`) with fluent interface for GET, POST, PUT, DELETE, PATCH, and multipart requests.
- Session-based HTTP API (`Request.Session.pas`) with persistent cookies, headers, and connection pooling.
- Automatic memory management for all main types (`THttpRequest`, `THttpSession`, `TResponse`) using advanced records.
- Built-in JSON parsing and safe access via `Response.JSON` (with clear ownership and memory safety).
- Robust multipart file upload support (stateless and session APIs).
- Comprehensive documentation: API reference, cheat sheet, technical details, and usage examples.
- Full test suite with 100% passing tests and no memory leaks.
- Cross-platform support (Windows, Linux) and HTTPS/SSL via OpenSSL.
- Project badges for license, version, build status, tests, platform, and FPC version.

### Changed
- N/A (first release)

### Fixed
- N/A (first release)

---

Older versions and changes will be listed here as the project evolves.
