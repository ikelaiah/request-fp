# Changelog

All notable changes to this project will be documented in this file.

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
