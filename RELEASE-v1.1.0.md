# Request-FP v1.1.0 Release Notes

**Release Date:** January 1, 2026

We're excited to announce Request-FP v1.1.0, a major documentation and tooling update focused on improving the Windows user experience and making OpenSSL setup effortless.

## 🎯 What's New

### Windows OpenSSL Made Easy

We've overhauled the OpenSSL documentation and error messages to make setup straightforward.

**What changed:**
- ✅ Clear installation instructions with 3 easy methods (Chocolatey, Scoop, or manual installer)
- ✅ Specific DLL filenames for both OpenSSL 1.1.x and 3.x
- ✅ Step-by-step troubleshooting guide
- ✅ Enhanced error messages that tell you exactly what to do

**No more guessing!** When OpenSSL is missing, you'll see:

```
OpenSSL initialization failed: Error loading library
Install OpenSSL and copy the required DLLs to your executable folder or add to PATH:
  OpenSSL 1.1.x: libssl-1_1-x64.dll and libcrypto-1_1-x64.dll
  OpenSSL 3.x: libssl-3-x64.dll and libcrypto-3-x64.dll
Install via: choco install openssl OR scoop install openssl
Or download from: https://slproweb.com/products/Win32OpenSSL.html
```

### 🔍 Debug Mode - See What's Really Happening

Ever wondered which OpenSSL version your app is using? Or where the DLLs are coming from? Now you can see exactly what's happening!

Enable debug mode and get instant visibility:

```pascal
{$DEFINE DEBUG}
```

**Output:**
```
[DEBUG] Initializing OpenSSL...
[DEBUG] OpenSSL initialized successfully (Windows)
[DEBUG] OpenSSL version: OpenSSL 1.1.1o  3 May 2022
[DEBUG] libssl loaded from: C:\WINDOWS\SYSTEM32\libssl-1_1-x64.dll
[DEBUG] libcrypto loaded from: C:\WINDOWS\SYSTEM32\libcrypto-1_1-x64.dll
```

This is **incredibly useful** when:
- You have multiple OpenSSL versions installed
- Wondering why an old version is loading instead of your new installation
- Diagnosing SSL/TLS issues
- Verifying your deployment

### 🛠️ New SSL Debug Tool

We've added a ready-to-use diagnostic tool at `examples/ssl_debug/`:

```bash
cd examples/ssl_debug
lazbuild --build-mode=Debug ssl_debug.lpi
./ssl_debug.exe
```

**Instantly see:**
- Whether OpenSSL is installed and working
- Exact version and DLL paths
- Clear success/failure messages

Perfect for troubleshooting or verifying your setup!

### 📚 Comprehensive New Guides

Three new documentation files to help you master HTTPS and OpenSSL:

1. **[SSL-HTTPS-GUIDE.md](docs/SSL-HTTPS-GUIDE.md)**
   - How HTTPS is determined (by URL protocol)
   - Strategies to enforce HTTPS in your apps
   - Helper function examples
   - Debug mode details

2. **[OPENSSL-VERSION-SELECTION.md](docs/OPENSSL-VERSION-SELECTION.md)**
   - Understanding Windows DLL search order
   - Why the wrong version might load
   - 3 solutions to control which version is used
   - OpenSSL 1.1.x vs 3.x comparison

3. **Enhanced README Troubleshooting**
   - Platform-specific error solutions
   - Verification commands
   - Network and certificate error guidance

### 🧪 Better Testing

Added `Test25_SSLInitialization` - an optional integration test that:
- Verifies OpenSSL is properly initialized
- Makes a real HTTPS request
- Provides clear Windows-specific troubleshooting on failure
- Can be run manually: `TestRunner.exe Test25_SSLInitialization`

All 41 tests pass with **zero memory leaks**! 🎉

## 📦 Installation

### New Users

```bash
git clone https://github.com/yourusername/request-fp.git
cd request-fp
```

**Windows OpenSSL Setup (choose one):**

```powershell
# Option 1: Chocolatey
choco install openssl

# Option 2: Scoop
scoop install openssl

# Option 3: Manual
# Download from https://slproweb.com/products/Win32OpenSSL.html
# Then copy DLLs to your exe folder or add to PATH
```

**Verify it works:**
```bash
cd examples/ssl_debug
lazbuild --build-mode=Debug ssl_debug.lpi
./ssl_debug.exe
```

### Existing Users

Simply update your repository:

```bash
git pull origin main
```

No code changes needed - this release is 100% backward compatible!

## 🔄 Upgrading from v1.0.0

**Breaking Changes:** None

**Migration Required:** No

All existing code continues to work without modification. This release only adds new features and documentation.

## 📊 Statistics

- **41 tests**, all passing
- **0 memory leaks**
- **5 new documentation files**
- **1 new example project**
- **180+ lines** of improved documentation

## 🙏 Community Feedback

This release was driven by real user feedback:

> "I was getting errors about OpenSSL when trying out your request library. I've installed FPC using FPCUpDeluxe, latest stable fpc + laz. Any ideas? I'm on Windows. I appreciate any help you could provide 🙏🏻"

We listened, and v1.1.0 ensures no one else faces this confusion!

## 🐛 Bug Fixes

- Resolved documentation gap causing Windows user confusion about OpenSSL requirements
- Clarified ambiguous "Windows: Usually included" statement
- Added missing Fedora/RHEL installation commands for Linux users

## 📝 Full Changelog

See [CHANGELOG.md](CHANGELOG.md#110---2026-01-01) for complete details.

## 🔗 Quick Links

- **Documentation**: [README.md](README.md)
- **API Reference**: [docs/Request.md](docs/Request.md)
- **SSL/HTTPS Guide**: [docs/SSL-HTTPS-GUIDE.md](docs/SSL-HTTPS-GUIDE.md)
- **Troubleshooting**: [README.md#troubleshooting](README.md#-troubleshooting)
- **Examples**: [examples/](examples/)

## 🎯 What's Next?

We're always listening to community feedback. If you have suggestions or encounter issues, please:

- 📝 [Open an issue](https://github.com/yourusername/request-fp/issues)
- 💬 Join the discussion
- ⭐ Star the repo if you find it useful!

## 💝 Thank You

Special thanks to the community members who provided feedback that made this release possible. Your input directly shapes the direction of Request-FP!

---

**Download:** [v1.1.0 Release](https://github.com/ikelaiah/request-fp/releases/tag/v1.1.0)


Happy coding! 🚀
