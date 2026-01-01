# Pull Request: v1.2.0 - Architecture Detection & Dynamic DLL Discovery

## Summary

This PR adds architecture mismatch detection, dynamic DLL discovery, and automatic DLL loading fixes to completely resolve Windows OpenSSL setup issues. Based on user feedback and community input, we discovered that:

1. Architecture mismatches occurred when fpcupdeluxe (32-bit default) was paired with 64-bit OpenSSL
2. System32 DLLs were taking priority over local OpenSSL installations
3. Users had no way to tell which architecture their executable was
4. Hardcoded DLL name guessing failed with vendors using non-standard naming conventions

v1.2.0 solves all four issues with dynamic DLL detection and improved diagnostics.

## What's New

### 🎯 Architecture Detection (ssl_debug)

- Shows executable architecture (32-bit vs 64-bit) upfront
- Displays required DLL names based on architecture
- Makes architecture mismatches immediately obvious

**Example output:**
```
=== OpenSSL Debug Information ===

Executable architecture: 64-bit
Required DLL names: libssl-*-x64.dll and libcrypto-*-x64.dll
```

### 🔧 SetDllDirectory Fix (ssl_debug)

- Forces Windows to search executable directory first for DLLs
- Prevents System32 DLLs from taking priority over local installations
- Solves the "wrong version loaded" problem automatically

### 📝 Enhanced Error Messages (Request.pas)

- Error messages now explicitly state whether 32-bit or 64-bit DLLs are needed
- Warns about ensuring architecture matches between exe and DLLs
- Helps users diagnose issues without running debug tools

### 🔍 Dynamic DLL Detection (Request.pas)

- New `FindSSLDLLPath` function using `EnumProcessModules`
- Searches all loaded modules for "libssl" or "libcrypto" in filename
- Works with ANY vendor naming convention (no more hardcoded guesses)
- Supports vendors who disguise OpenSSL 3.x as 1.1.x

### 📚 Documentation Updates

- README.md: Added FPC's automatic OpenSSL version detection explanation and 32-bit FPC information
- CHANGELOG.md: Documented all v1.2.0 changes
- RELEASE-v1.2.0.md: Comprehensive release notes with FPC DLL search order details

### ✨ Community Contributions

- Added ReadLn pause to ssl_debug (prevents console auto-close in IDE)
- Dynamic DLL detection suggestion from Gustavo

## Files Changed

### Modified Files

1. **src/Request.pas**
   - Updated version to 1.2.0
   - Added architecture-specific error messages in InitSSL
   - Added `FindSSLDLLPath` function using `EnumProcessModules`
   - Replaced hardcoded DLL name guessing with dynamic module enumeration

2. **examples/ssl_debug/ssl_debug.pas**
   - Added SetDllDirectoryW external declaration
   - Added architecture detection output
   - Added SetDllDirectory call to force local DLL loading
   - Added ReadLn pause for IDE users

3. **README.md**
   - Updated version badge to 1.2.0
   - Added fpcupdeluxe 32-bit default warning

4. **CHANGELOG.md**
   - Added v1.2.0 section with Added/Changed/Fixed entries
   - Documented dynamic DLL detection feature

### Created Files

5. **RELEASE-v1.2.0.md**
   - Complete release notes for v1.2.0
   - Troubleshooting guide for fpcupdeluxe users
   - Documentation of dynamic DLL discovery

6. **PR-v1.2.0.md** (this file)
   - Pull request description

## Testing

### Manual Testing

Tested on Windows 10/11 with:
- ✅ 64-bit FPC + 64-bit OpenSSL 3.6.0
- ✅ 64-bit FPC + old System32 OpenSSL 1.1.1 (SetDllDirectory override works)
- ✅ 32-bit FPC (fpcupdeluxe default) + 32-bit OpenSSL 3.6.0

### Test Results

- All 41 existing tests continue to pass
- Zero memory leaks
- ssl_debug correctly identifies architecture
- SetDllDirectory successfully loads local DLLs over System32

**Example debug output (successful):**
```
=== OpenSSL Debug Information ===

Executable architecture: 64-bit
Required DLL names: libssl-*-x64.dll and libcrypto-*-x64.dll

Making HTTPS request to verify OpenSSL is working...
[DEBUG] Initializing OpenSSL...
[DEBUG] OpenSSL initialized successfully (Windows)
[DEBUG] libssl loaded from: C:\Users\...\ssl_debug\libssl-1_1-x64.dll
[DEBUG] libcrypto loaded from: C:\Users\...\ssl_debug\libcrypto-3-x64.dll
[DEBUG] OpenSSL version: OpenSSL 1.1.1o  3 May 2022

=== SUCCESS ===
Status Code: 200
OpenSSL is working correctly!
```

## Breaking Changes

**None** - This release is 100% backward compatible with v1.1.0.

## Migration Required

**No** - All existing code continues to work without modification.

## Impact

### User Experience Improvements

1. **Immediate diagnosis** - Users see architecture mismatch instantly
2. **Automatic fixes** - SetDllDirectory eliminates manual DLL placement issues
3. **Clear guidance** - Error messages tell users exactly what to do

### Resolved Issues

- ✅ fpcupdeluxe users downloading wrong OpenSSL architecture
- ✅ System32 DLLs overriding local installations
- ✅ Cryptic "Error loading library" messages with no hints
- ✅ Console auto-closing in IDE before users can read output
- ✅ Hardcoded DLL name guessing failing with non-standard vendor naming
- ✅ Debug mode breaking when vendors disguise OpenSSL 3.x as 1.1.x

## Community Feedback

This release directly addresses real user issues:

> "I was getting errors about OpenSSL when trying out your request library. I've installed FPC using FPCUpDeluxe, latest stable fpc + laz. I'm on Windows."

With v1.2.0:
- User sees "Executable architecture: 32-bit"
- Error message says "You need 32-bit OpenSSL DLLs (no -x64 suffix)"
- Problem solved immediately!

And from Discord:
> "... hardcoded the dll name..."

With v1.2.0:
- Dynamic DLL detection finds ANY OpenSSL DLL, regardless of naming
- Works with vendors using non-standard names
- Future-proof solution!

## Code Quality

- No new dependencies (uses Windows psapi.dll)
- Clean, focused code changes
- Well-documented with inline comments
- Follows existing code style
- Zero compiler warnings

## Reviewer Notes

### Key Areas to Review

1. **FindSSLDLLPath implementation** (Request.pas)
   - Verify EnumProcessModules usage is correct
   - Ensure it works with both 32-bit and 64-bit processes
   - Check that module enumeration doesn't miss DLLs

2. **SetDllDirectoryW declaration** (ssl_debug.pas:12)
   - Ensure external declaration is correct for all Windows versions

3. **Architecture-specific error messages** (Request.pas)
   - Verify conditional compilation works correctly for both CPU32 and CPU64

4. **README documentation** (README.md)
   - Check that fpcupdeluxe warning is clear and accurate

5. **CHANGELOG completeness** (CHANGELOG.md:20-40)
   - Ensure all v1.2.0 changes are documented

### Testing Checklist

- [ ] Build ssl_debug in Debug mode
- [ ] Build ssl_debug in Release mode
- [ ] Run all 41 tests (`TestRunner.exe -a`)
- [ ] Verify no memory leaks in heaptrc output
- [ ] Test with OpenSSL 1.1.x DLLs
- [ ] Test with OpenSSL 3.x DLLs
- [ ] Test with non-standard vendor DLL names (e.g., OpenSSL 3.x disguised as 1.1)
- [ ] Test architecture detection output (32-bit vs 64-bit)
- [ ] Test SetDllDirectory prevents System32 DLL loading

## Related Issues

This PR resolves common user confusion about OpenSSL setup on Windows, particularly for fpcupdeluxe users.

## Screenshots

**Architecture Detection (64-bit):**
```
Executable architecture: 64-bit
Required DLL names: libssl-*-x64.dll and libcrypto-*-x64.dll
```

**Architecture Detection (32-bit):**
```
Executable architecture: 32-bit
Required DLL names: libssl-*.dll and libcrypto-*.dll (32-bit)
```

**Enhanced Error Message:**
```
OpenSSL initialization failed: Error loading library
This is a 64-bit executable. You need 64-bit DLLs:
  OpenSSL 1.1.x: libssl-1_1-x64.dll and libcrypto-1_1-x64.dll
  OpenSSL 3.x: libssl-3-x64.dll and libcrypto-3-x64.dll
IMPORTANT: Ensure DLL architecture (32-bit vs 64-bit) matches your executable!
```

## Additional Notes

### SetDllDirectoryW Approach

The `SetDllDirectoryW` approach is the cleanest solution for the DLL search order issue. Alternative approaches considered:

1. ❌ Modify global PATH - Too invasive, affects other applications
2. ❌ Manual DLL placement - Requires user intervention, error-prone
3. ✅ SetDllDirectory - Application-scoped, automatic, no side effects

### Dynamic DLL Detection Approach

The `FindSSLDLLPath` with `EnumProcessModules` complements FPC's existing DLL search:

**FPC already handles version fallback:**
- FPC's `openssl.pas` tries: `libssl-3-x64` → `libssl-1_1-x64` → `ssleay32` → `libssl32`
- This works great for loading DLLs

**Our contribution:**
- Shows users which version FPC actually loaded
- Helps diagnose "Is my app using OpenSSL 3.x or 1.1.x?"
- Works with any vendor naming, including non-standard conventions

This approach leverages Windows' guarantee that loaded modules match process architecture, so we don't need to check DLL architecture ourselves. We're showing what FPC loaded, not duplicating its loading logic.

