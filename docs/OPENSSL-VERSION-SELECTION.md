# OpenSSL version selection on Windows

## The FPC 3.2.2 filename problem

FPC 3.2.2's OpenSSL bindings can use the OpenSSL 3 API, but its Windows
loader does not try the standard OpenSSL 3 DLL names. On 64-bit Windows, its
newest default candidates are:

- `libssl-1_1-x64.dll`
- `libcrypto-1_1-x64.dll`

Installing OpenSSL 3 can therefore leave `InitSSLInterface` returning `False`
even when `openssl.exe version` works. The command-line program finding its
own libraries does not prove that an FPC application knows their filenames.

## Request-FP's v1.3.0 compatibility override

Request-FP selects the OpenSSL 3 names before initializing HTTPS:

```pascal
{$IFDEF CPU64}
DLLSSLName := 'libssl-3-x64.dll';
DLLUtilName := 'libcrypto-3-x64.dll';
{$ELSE}
DLLSSLName := 'libssl-3.dll';
DLLUtilName := 'libcrypto-3.dll';
{$ENDIF}

if not InitSSLInterface then
  raise ERequestError.Create('Could not initialize OpenSSL library');
```

Only FPC's primary candidates are replaced. Its existing OpenSSL 1.1
fallback candidates remain available, so older deployments continue to work.
Applications do not need to patch or rebuild FPC.

This was easy to miss on development machines that already had OpenSSL 1.1
DLLs in `C:\Windows\System32`: FPC silently loaded those instead. The clean
Windows CI runner had only OpenSSL 3, which exposed the filename mismatch.

## Required DLLs

| Executable | SSL DLL | Crypto DLL |
| --- | --- | --- |
| 64-bit | `libssl-3-x64.dll` | `libcrypto-3-x64.dll` |
| 32-bit | `libssl-3.dll` | `libcrypto-3.dll` |

Both DLLs must come from the same OpenSSL build, and their architecture must
match the executable—not merely the Windows installation.

## Recommended setup

Install a maintained OpenSSL 3 build, then use either of these approaches:

1. Put its `bin` directory on `PATH`.
2. Copy both matching DLLs from that `bin` directory beside your executable.

The executable directory is the most predictable option for a deployed
application:

```powershell
$opensslBin = 'C:\Program Files\OpenSSL\bin'
Copy-Item "$opensslBin\libssl-3-x64.dll" .\your-exe-directory
Copy-Item "$opensslBin\libcrypto-3-x64.dll" .\your-exe-directory
```

Adjust the path and filenames for your installation and architecture.

Do not rename or remove DLLs from `C:\Windows\System32`. That can break other
applications and is no longer needed by Request-FP.

## Diagnosing the loaded version

Build and run the debug example:

```powershell
cd examples\ssl_debug
lazbuild --build-mode=Debug ssl_debug.lpi
.\ssl_debug.exe
```

The debug build reports the OpenSSL version and loaded DLL paths:

```text
[DEBUG] OpenSSL version: OpenSSL 3.6.3 ...
[DEBUG] libssl loaded from: C:\Program Files\OpenSSL\bin\libssl-3-x64.dll
[DEBUG] libcrypto loaded from: C:\Program Files\OpenSSL\bin\libcrypto-3-x64.dll
```

You can also inspect candidates visible through `PATH`:

```powershell
where.exe libssl-3-x64.dll
where.exe libcrypto-3-x64.dll
where.exe libssl-1_1-x64.dll
where.exe libcrypto-1_1-x64.dll
```

## Troubleshooting checklist

1. Confirm both SSL and crypto DLLs are present.
2. Confirm both DLLs came from the same OpenSSL build.
3. Confirm the DLL and executable architectures match.
4. Restart Lazarus or the terminal after changing `PATH`.
5. Run `examples/ssl_debug` to see what was actually loaded.

## See also

- [SSL/HTTPS guide](SSL-HTTPS-GUIDE.md)
- [Technical details](TECHNICAL-DETAILS.md#windows-openssl-3-loading-with-fpc-322)
- [SSL debug example](../examples/ssl_debug/)
