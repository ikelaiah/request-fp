# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added

- (none)

### Changed

- (none)

### Fixed

- (none)


## [1.1.1] - 2026-01-01

### Added

- **Architecture Detection in ssl_debug**: ssl_debug example now displays executable architecture (32-bit vs 64-bit) and required DLL names upfront
- **Architecture-Specific Error Messages**: InitSSL error messages now explicitly state whether 32-bit or 64-bit DLLs are required based on executable architecture

### Changed

- **Enhanced ssl_debug Output**: Now shows "Executable architecture: 64-bit" and "Required DLL names: libssl-*-x64.dll and libcrypto-*-x64.dll" before attempting HTTPS request
- **Improved Error Messages**: Added "IMPORTANT: Ensure DLL architecture (32-bit vs 64-bit) matches your executable!" warning to help diagnose architecture mismatch issues

### Fixed

- **Architecture Mismatch Detection**: Users with mismatched executable/DLL architectures now get clear error messages explaining the issue instead of cryptic "Error loading library" failures


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
