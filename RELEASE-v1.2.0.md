# Request-FP v1.2.0 Release Notes

**Release Date:** January 1, 2026

This is a minor feature release that adds architecture mismatch detection and dynamic DLL discovery to completely solve Windows OpenSSL setup issues, including support for vendors with non-standard DLL naming conventions.

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

## 📦 Changes

### Enhanced Diagnostics

1. **ssl_debug Example** - Shows executable architecture upfront
2. **Architecture-Specific Error Messages** - Error messages now detect and display whether you need 32-bit or 64-bit DLLs
3. **Dynamic DLL Detection** - New `FindSSLDLLPath` function finds loaded OpenSSL DLLs regardless of vendor naming conventions
4. **SetDllDirectory Integration** - Forces local DLL loading over System32
5. **ReadLn Pause** - ssl_debug now pauses before exit when run from IDE (community contribution)

## 🔄 Upgrading from v1.1.0

**Breaking Changes:** None

**Migration Required:** No

Simply update your repository:
```bash
git pull origin main
```

All v1.1.0 code continues to work without modification.

## 📊 Changes Summary

- **Files Modified:** 4 (CHANGELOG.md, README.md, src/Request.pas, examples/ssl_debug/ssl_debug.pas)
- **New Features:** Architecture detection, dynamic DLL discovery, SetDllDirectory integration
- **Bug Fixes:** Better diagnostics for architecture mismatch, DLL search order issues, vendor naming variations
- **Breaking Changes:** 0

## 🐛 What This Fixes

**Problem 1:** Users with mismatched executable/DLL architectures got cryptic "Error loading library" messages with no hint about the real issue.

**Solution:**
- Detect executable architecture at runtime
- Display required DLL names based on architecture
- Provide explicit warnings about architecture matching

**Problem 2:** Vendors using non-standard OpenSSL naming (e.g., disguising 3.x as 1.1.x) broke debug mode DLL path detection.

**Solution:**
- Dynamic module enumeration finds ANY DLL with "libssl" or "libcrypto" in the name
- No more hardcoded filename guessing

**Problem 3:** System32 OpenSSL DLLs taking priority over local installations.

**Solution:**
- SetDllDirectoryW in ssl_debug forces executable directory to be searched first

**Impact:** Users can now instantly diagnose OpenSSL issues regardless of their FPC architecture, OpenSSL vendor, or installation method.

## 📝 Full Changelog

See [CHANGELOG.md](CHANGELOG.md#120---2026-01-01) for complete details.

## 🔗 Quick Links

- **Documentation**: [README.md](README.md)
- **Troubleshooting**: [README.md#-troubleshooting](README.md#-troubleshooting)
- **SSL Debug Tool**: [examples/ssl_debug/](examples/ssl_debug/)

## 💡 For Users Still Having Issues

If you're still experiencing OpenSSL failures after upgrading to v1.2.0:

1. **Run the debug tool:**
   ```bash
   cd examples/ssl_debug
   lazbuild --build-mode=Debug ssl_debug.lpi
   ssl_debug.exe
   ```

2. **Check the architecture line** - Does it say 64-bit or 32-bit?

3. **Verify your DLLs match** - If you have a 64-bit exe, you MUST have DLLs with `-x64` in the filename

4. **Check the installer you downloaded** - Make sure it matches your architecture (Win64 vs Win32)

### Understanding OpenSSL DLL Dependencies

**Important discovery from community testing:** FPC's `openssl.pas` tries multiple OpenSSL DLL names in priority order:

- **FPC tries (64-bit)**: `libssl-3-x64` → `libssl-1_1-x64` → `ssleay32` → `libssl32`
- **FPC tries (32-bit)**: `libssl-3` → `libssl-1_1` → `ssleay32` → `libssl32`

FPC correctly prioritizes OpenSSL 3.x! However, Windows DLL dependency chains can still cause confusion when multiple versions are installed.

This is a **Windows-specific complexity** - Linux uses dynamic library linking and works flawlessly.

**v1.2.0's dynamic DLL detection** uses `EnumProcessModules` to show users exactly which OpenSSL DLL Windows actually loaded from FPC's search list. This eliminates guesswork about which version is running.

### Note for fpcupdeluxe Users

**FYI:** fpcupdeluxe with default settings installs **32-bit FPC**, even on 64-bit Windows. This is a common choice for lower memory footprint and smaller executables.

If you're using 32-bit FPC, you need **32-bit OpenSSL**:
- Download: `Win32OpenSSL_Light-3_6_0.exe` (the Win32 installer)
- Use DLLs: `libssl-3.dll` and `libcrypto-3.dll` (without `-x64` suffix)

The v1.2.0 debug output shows your executable architecture (32-bit or 64-bit) upfront, making it easy to verify which OpenSSL version you need.

## 💝 Thank You

Special thanks to:
- The community member who suggested adding `ReadLn` to prevent console auto-close in the IDE!
- Gustavo for pointing out the hardcoded DLL name issue and suggesting dynamic detection!

---

**Download:** [v1.2.0 Release](https://github.com/yourusername/request-fp/releases/tag/v1.2.0)

**Previous Release:** [v1.1.0](https://github.com/yourusername/request-fp/releases/tag/v1.1.0)

Happy coding! 🚀
