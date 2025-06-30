# Changelog

All notable changes to this project will be documented in this file.

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
