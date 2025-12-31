# Pull Request: Release v1.1.0 - Windows OpenSSL Documentation & Debug Improvements

## Summary

This PR significantly improves the Windows user experience by addressing OpenSSL setup confusion and adding powerful diagnostic tools. Users reported encountering "OpenSSL initialization failed" errors after fresh FPC/Lazarus installations, with no clear guidance on how to resolve them.

## What's New in v1.1.0

### 🪟 Windows OpenSSL Support

**Problem**: Users installing Request-FP on Windows encountered cryptic OpenSSL errors with no clear resolution path. The documentation simply stated "Windows: Usually included" which proved misleading for fresh installations.

**Solution**: Comprehensive Windows-specific documentation and tooling:

- **Clear Installation Instructions**: Multiple installation methods documented
  - Chocolatey: `choco install openssl`
  - Scoop: `scoop install openssl`
  - Manual installer from [Shining Light Productions](https://slproweb.com/products/Win32OpenSSL.html)
- **Specific DLL Names**: Exact filenames for both OpenSSL versions
  - OpenSSL 1.1.x: `libssl-1_1-x64.dll`, `libcrypto-1_1-x64.dll`
  - OpenSSL 3.x: `libssl-3-x64.dll`, `libcrypto-3-x64.dll`
- **Deployment Options**: Clear guidance on DLL placement (executable folder vs PATH)

### 🔍 Debug Mode & DLL Path Detection

Added powerful debugging capabilities to diagnose OpenSSL issues:

```
[DEBUG] Initializing OpenSSL...
[DEBUG] OpenSSL initialized successfully (Windows)
[DEBUG] OpenSSL version: OpenSSL 1.1.1o  3 May 2022
[DEBUG] libssl loaded from: C:\WINDOWS\SYSTEM32\libssl-1_1-x64.dll
[DEBUG] libcrypto loaded from: C:\WINDOWS\SYSTEM32\libcrypto-1_1-x64.dll
```

This helps users immediately identify:
- Which OpenSSL version is being used
- Where DLLs are loaded from
- Version conflicts when multiple installations exist

**How to enable**: Compile with `-dDEBUG` flag

### 📚 New Documentation

1. **[docs/SSL-HTTPS-GUIDE.md](docs/SSL-HTTPS-GUIDE.md)**
   - HTTPS enforcement strategies
   - Debug mode usage guide
   - Helper function examples for forcing HTTPS

2. **[docs/OPENSSL-VERSION-SELECTION.md](docs/OPENSSL-VERSION-SELECTION.md)**
   - Resolving version conflicts
   - Understanding Windows DLL search order
   - Solutions when wrong version loads

3. **Enhanced README Troubleshooting Section**
   - Platform-specific OpenSSL error solutions
   - Verification commands
   - Common network/certificate error guidance

### 🧪 Testing & Tools

- **New Test**: `Test25_SSLInitialization` verifies SSL setup and provides Windows-specific error messages
- **SSL Debug Example**: New `examples/ssl_debug/` project with pre-configured Debug build mode
  - Instant OpenSSL diagnostics
  - Perfect for troubleshooting setup issues

### 🔧 Enhanced Error Messages

**Before:**
```
OpenSSL initialization failed: <cryptic error>
```

**After:**
```
OpenSSL initialization failed: <error details>
Install OpenSSL and copy the required DLLs to your executable folder or add to PATH:
  OpenSSL 1.1.x: libssl-1_1-x64.dll and libcrypto-1_1-x64.dll
  OpenSSL 3.x: libssl-3-x64.dll and libcrypto-3-x64.dll
Install via: choco install openssl OR scoop install openssl
Or download from: https://slproweb.com/products/Win32OpenSSL.html
```

## Files Changed

### Modified
- `README.md` - Updated version to 1.1.0, added comprehensive troubleshooting section
- `CHANGELOG.md` - Added v1.1.0 release notes
- `src/Request.pas` - Version bump, debug mode, Windows DLL path detection
- `docs/Request.md` - Windows OpenSSL instructions, troubleshooting section
- `docs/Request.Session.md` - Windows OpenSSL instructions
- `tests/Request.Test.pas` - Added SSL initialization test

### Added
- `examples/ssl_debug/ssl_debug.pas` - Debug tool for OpenSSL diagnostics
- `examples/ssl_debug/ssl_debug.lpi` - Lazarus project with Debug mode configured
- `examples/ssl_debug/README.md` - Usage instructions
- `docs/SSL-HTTPS-GUIDE.md` - Complete HTTPS enforcement guide
- `docs/OPENSSL-VERSION-SELECTION.md` - Version conflict resolution guide

## Testing

All tests pass (41/41) with zero memory leaks:

```
Number of run tests: 41
Number of errors:    0
Number of failures:  0

0 unfreed memory blocks : 0
```

The new `Test25_SSLInitialization` successfully verifies SSL functionality and provides helpful error messages on failure.

## Breaking Changes

None - this is a purely additive release focused on documentation and tooling.

## Migration Guide

No migration needed. All existing code continues to work unchanged.

## User Impact

**Before v1.1.0:**
- Users encounter "OpenSSL initialization failed" with no guidance
- No way to determine which OpenSSL version is loaded
- Confusing documentation: "Windows: Usually included"
- Time-consuming trial-and-error to resolve issues

**After v1.1.0:**
- Clear, actionable installation instructions
- Debug mode shows exactly what's happening
- Comprehensive troubleshooting guides
- ssl_debug tool for instant diagnostics
- Version conflict resolution guide

## Checklist

- [x] Version bumped in README.md (1.1.0)
- [x] Version bumped in src/Request.pas (1.1.0)
- [x] CHANGELOG.md updated with release notes
- [x] All tests passing (41/41)
- [x] No memory leaks (0 unfreed blocks)
- [x] Documentation updated
- [x] New examples tested and working
- [x] Debug mode verified on Windows

## Related Issues

Addresses user feedback: "I was getting errors about OpenSSL when trying out your request library. I've installed FPC using FPCUpDeluxe, latest stable fpc + laz. Any ideas? I'm on Windows."

## Screenshots

**Debug Output Example:**
```
[DEBUG] Initializing OpenSSL...
[DEBUG] OpenSSL initialized successfully (Windows)
[DEBUG] OpenSSL version: OpenSSL 1.1.1o  3 May 2022
[DEBUG] libssl loaded from: C:\WINDOWS\SYSTEM32\libssl-1_1-x64.dll
[DEBUG] libcrypto loaded from: C:\WINDOWS\SYSTEM32\libcrypto-1_1-x64.dll
```

## Reviewer Notes

Key areas to review:
1. Windows-specific instructions in README.md troubleshooting section
2. Debug mode implementation in src/Request.pas (lines 480-549)
3. New documentation files for completeness and accuracy
4. ssl_debug example builds and runs correctly
