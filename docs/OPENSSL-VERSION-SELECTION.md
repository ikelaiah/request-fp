# OpenSSL Version Selection on Windows

## The Problem

When you have multiple OpenSSL versions installed on Windows, the wrong version might be loaded. This happens because:

1. **FPC's opensslsockets unit** searches for DLLs in this order:
   - OpenSSL 1.1.x: `libssl-1_1-x64.dll`, `libcrypto-1_1-x64.dll`
   - OpenSSL 3.x: `libssl-3-x64.dll`, `libcrypto-3-x64.dll`

2. **Windows DLL search order:**
   - Application directory (where your .exe is)
   - `C:\Windows\System32\`
   - Directories in PATH environment variable

## Diagnosing Which Version is Loaded

Use the debug mode to see exactly which DLLs are being loaded:

```bash
cd examples/ssl_debug
lazbuild --build-mode=Debug ssl_debug.lpi
./ssl_debug.exe
```

You'll see output like:

```
[DEBUG] OpenSSL version: OpenSSL 1.1.1o  3 May 2022
[DEBUG] libssl loaded from: C:\WINDOWS\SYSTEM32\libssl-1_1-x64.dll
[DEBUG] libcrypto loaded from: C:\WINDOWS\SYSTEM32\libcrypto-1_1-x64.dll
```

## Solutions

### Solution 1: Copy DLLs to Your Executable Directory (Recommended)

This ensures your application always uses the specific version you want:

```bash
# If you installed OpenSSL 3.6.0
copy "C:\Windows\System32\libssl-3-x64.dll" "your-exe-directory\"
copy "C:\Windows\System32\libcrypto-3-x64.dll" "your-exe-directory\"
```

Advantages:
- ✅ Application-specific - won't affect other programs
- ✅ No admin rights needed
- ✅ Portable - you can distribute these DLLs with your app

### Solution 2: Remove Old DLLs from System32 (Requires Admin)

**⚠️ WARNING:** This may break other applications that depend on OpenSSL 1.1.x!

```powershell
# Run PowerShell as Administrator
cd C:\Windows\System32
ren libssl-1_1-x64.dll libssl-1_1-x64.dll.bak
ren libcrypto-1_1-x64.dll libcrypto-1_1-x64.dll.bak
```

After this, FPC will find and use the OpenSSL 3.x DLLs.

To undo:
```powershell
cd C:\Windows\System32
ren libssl-1_1-x64.dll.bak libssl-1_1-x64.dll
ren libcrypto-1_1-x64.dll.bak libcrypto-1_1-x64.dll
```

### Solution 3: Modify FPC's opensslsockets Unit (Advanced)

You can modify FPC's opensslsockets unit to search for OpenSSL 3.x first:

1. Locate: `C:\lazarus\fpc\3.2.2\source\packages\openssl\src\opensslsockets.pp`
2. Find the DLL name constants
3. Swap the order so 3.x is checked before 1.1.x
4. Recompile the RTL

**This is complex and not recommended for most users.**

## Which OpenSSL Version Should You Use?

### OpenSSL 1.1.1 (May 2022 - EOL September 2023)
- ✅ Widely compatible
- ✅ Stable and well-tested
- ❌ **No longer supported** (end of life)
- ❌ No security updates

### OpenSSL 3.x (Current - LTS until 2026)
- ✅ **Current LTS version**
- ✅ Active security updates
- ✅ Modern cryptography
- ⚠️ Some API changes from 1.1.x (mostly transparent to users)

**Recommendation:** Use OpenSSL 3.x for new applications. The FPC opensslsockets unit supports both versions transparently.

## Verifying Your Choice

After applying one of the solutions, run the debug tool again:

```bash
cd examples/ssl_debug
./ssl_debug.exe
```

You should see:

```
[DEBUG] OpenSSL version: OpenSSL 3.6.0 ...
[DEBUG] libssl loaded from: [your chosen location]
```

## Troubleshooting

### Multiple Versions in Different Locations

Run this command to see all OpenSSL DLLs on your system:

```powershell
# Find OpenSSL 1.1.x
where libssl-1_1-x64.dll
where libcrypto-1_1-x64.dll

# Find OpenSSL 3.x
where libssl-3-x64.dll
where libcrypto-3-x64.dll
```

### Application Still Uses Wrong Version

1. Make sure you copied **both** DLLs (libssl AND libcrypto)
2. Check the DLL names match exactly (x64 suffix, version number)
3. Restart your terminal/IDE to pick up environment changes
4. Use the debug tool to verify

## See Also

- [Troubleshooting Guide](../README.md#-troubleshooting) - General OpenSSL troubleshooting
- [SSL Debug Example](../examples/ssl_debug/) - Tool to diagnose DLL loading
