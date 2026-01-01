# Request-FP v1.1.1 Release Notes

**Release Date:** January 1, 2026

This is a minor patch release that adds architecture mismatch detection to help diagnose the most common OpenSSL setup issue on Windows: mixing 32-bit and 64-bit binaries.

## 🎯 What's New

### Architecture Mismatch Detection

After v1.1.0 release, we discovered that some users were still having OpenSSL failures even after following the installation instructions correctly. The root cause? They had 64-bit executables but 32-bit OpenSSL DLLs (or vice versa).

**v1.1.1 makes this obvious:**

When you run `ssl_debug.exe`, you now see:
```
=== OpenSSL Debug Information ===

Executable architecture: 64-bit
Required DLL names: libssl-*-x64.dll and libcrypto-*-x64.dll
```

If initialization fails, the error message now explicitly states:
```
OpenSSL initialization failed: Error loading library
This is a 64-bit executable. You need 64-bit DLLs:
  OpenSSL 1.1.x: libssl-1_1-x64.dll and libcrypto-1_1-x64.dll
  OpenSSL 3.x: libssl-3-x64.dll and libcrypto-3-x64.dll
IMPORTANT: Ensure DLL architecture (32-bit vs 64-bit) matches your executable!
```

No more mystery failures!

## 📦 Changes

### Enhanced Diagnostics

1. **ssl_debug Example** - Shows executable architecture upfront
2. **Architecture-Specific Error Messages** - Error messages now detect and display whether you need 32-bit or 64-bit DLLs
3. **ReadLn Pause** - ssl_debug now pauses before exit when run from IDE (community contribution)

## 🔄 Upgrading from v1.1.0

**Breaking Changes:** None

**Migration Required:** No

Simply update your repository:
```bash
git pull origin main
```

All v1.1.0 code continues to work without modification.

## 📊 Changes Summary

- **Files Modified:** 3 (CHANGELOG.md, README.md, src/Request.pas, examples/ssl_debug/ssl_debug.pas)
- **New Features:** Architecture detection and display
- **Bug Fixes:** Better diagnostics for architecture mismatch
- **Breaking Changes:** 0

## 🐛 What This Fixes

**Problem:** Users with mismatched executable/DLL architectures got cryptic "Error loading library" messages with no hint about the real issue.

**Solution:**
- Detect executable architecture at runtime
- Display required DLL names based on architecture
- Provide explicit warnings about architecture matching

**Impact:** Users can now instantly diagnose whether they downloaded the wrong OpenSSL installer or compiled for the wrong architecture.

## 📝 Full Changelog

See [CHANGELOG.md](CHANGELOG.md#111---2026-01-01) for complete details.

## 🔗 Quick Links

- **Documentation**: [README.md](README.md)
- **Troubleshooting**: [README.md#-troubleshooting](README.md#-troubleshooting)
- **SSL Debug Tool**: [examples/ssl_debug/](examples/ssl_debug/)

## 💡 For Users Still Having Issues

If you're still experiencing OpenSSL failures after upgrading to v1.1.1:

1. **Run the debug tool:**
   ```bash
   cd examples/ssl_debug
   lazbuild --build-mode=Debug ssl_debug.lpi
   ssl_debug.exe
   ```

2. **Check the architecture line** - Does it say 64-bit or 32-bit?

3. **Verify your DLLs match** - If you have a 64-bit exe, you MUST have DLLs with `-x64` in the filename

4. **Check the installer you downloaded** - Make sure it's Win64 OpenSSL, not Win32

## 💝 Thank You

Special thanks to the community member who suggested adding `ReadLn` to prevent console auto-close in the IDE!

---

**Download:** [v1.1.1 Release](https://github.com/yourusername/request-fp/releases/tag/v1.1.1)

**Previous Release:** [v1.1.0](https://github.com/yourusername/request-fp/releases/tag/v1.1.0)

Happy coding! 🚀
