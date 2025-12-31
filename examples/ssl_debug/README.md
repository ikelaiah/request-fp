# SSL Debug Example

This example demonstrates how to enable debug mode to see detailed information about OpenSSL DLL loading.

## Purpose

Use this to troubleshoot OpenSSL issues and verify where the DLLs are being loaded from on your system.

## Building

### Using lazbuild (Command Line):

**Debug mode (with SSL debug output):**
```bash
lazbuild --build-mode=Debug ssl_debug.lpi
```

**Default mode (no debug output):**
```bash
lazbuild --build-mode=Default ssl_debug.lpi
```

### Using FPC directly:

```bash
fpc -dDEBUG -Fu../../src ssl_debug.pas
```

### Using Lazarus IDE:

1. Open `ssl_debug.lpi` in Lazarus
2. Select **Debug** build mode from the dropdown (next to the Build button)
3. Build and run (F9)

Or to enable debug in Default mode:
1. Project → Project Options → Compiler Options → Custom Options
2. Add: `-dDEBUG`
3. Build and run

## What You'll See

When running with DEBUG mode enabled, you'll see output like:

**On Windows:**
```
=== OpenSSL Debug Information ===

Making HTTPS request to verify OpenSSL is working...
[DEBUG] Initializing OpenSSL...
[DEBUG] OpenSSL initialized successfully (Windows)
[DEBUG] OpenSSL version: OpenSSL 3.0.11 19 Sep 2023

=== SUCCESS ===
Status Code: 200
OpenSSL is working correctly!
```

**On Linux:**
```
=== OpenSSL Debug Information ===

Making HTTPS request to verify OpenSSL is working...
[DEBUG] Initializing OpenSSL...
[DEBUG] OpenSSL initialized successfully (Unix)

=== SUCCESS ===
Status Code: 200
OpenSSL is working correctly!
```

This information helps verify:
- OpenSSL is loading correctly
- **Which version is being used** (important if you have multiple versions!)
- **Where the DLLs are loaded from** (helps diagnose version conflicts)
- That HTTPS requests work end-to-end

## Common Issue: Wrong OpenSSL Version

If you see an older version being loaded (e.g., OpenSSL 1.1.1 when you installed 3.6.0), check the DLL path in the debug output:

```
[DEBUG] libssl loaded from: C:\WINDOWS\SYSTEM32\libssl-1_1-x64.dll
```

This means Windows found an older version in System32 before your new installation. See the [OpenSSL Version Selection Guide](../../docs/OPENSSL-VERSION-SELECTION.md) for solutions.

## Enabling DEBUG Mode in Your Own Code

Add this at the top of your program:

```pascal
{$DEFINE DEBUG}
```

Or compile with the `-dDEBUG` flag:

```bash
fpc -dDEBUG yourprogram.pas
```

## Troubleshooting

If you see an error instead of debug output, refer to the main [Troubleshooting Guide](../../README.md#-troubleshooting) for OpenSSL installation instructions.
