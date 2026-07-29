# SSL/HTTPS configuration guide

## How HTTPS is selected

Request-FP uses HTTPS whenever the request URL starts with `https://`. There
is no global HTTPS switch: the URL scheme determines whether TLS is used.

```pascal
Response := Http.Get('https://api.example.com/data'); // Encrypted
Response := Http.Get('http://api.example.com/data');  // Not encrypted
```

For sessions, put HTTPS in the base URL:

```pascal
var
  Session: THttpSession;
begin
  Session.SetBaseURL('https://api.example.com');
  Response := Session.Get('/users');
end;
```

## Windows setup

HTTPS requires OpenSSL DLLs whose architecture matches the executable.

| Executable | OpenSSL 3 DLLs |
| --- | --- |
| 64-bit | `libssl-3-x64.dll`, `libcrypto-3-x64.dll` |
| 32-bit | `libssl-3.dll`, `libcrypto-3.dll` |

Put both DLLs beside the executable or add their installation directory to
`PATH`.

FPC 3.2.2 has an unusual Windows compatibility issue: its bindings support
OpenSSL 3, but its loader does not try the OpenSSL 3 filenames. Request-FP
v1.3.0 handles this automatically by selecting the OpenSSL 3 names first and
retaining FPC's OpenSSL 1.1 fallback. You do not need to modify FPC, rename
system DLLs, or add special code to your application.

See [OpenSSL version selection](OPENSSL-VERSION-SELECTION.md) for the exact
loading behavior and troubleshooting steps.

## Linux setup

Install the OpenSSL package supplied by the distribution. For example:

```bash
sudo apt-get install libssl-dev
```

Request-FP uses FPC's normal Unix library discovery on Linux.

## Enforcing HTTPS in an application

If your application must reject plain HTTP, validate URLs at its boundary:

```pascal
function SecureGet(const URL: string): TResponse;
begin
  if Pos('https://', LowerCase(URL)) <> 1 then
    raise ERequestError.Create('Only HTTPS URLs are allowed');
  Result := Http.Get(URL);
end;
```

This is an application policy; Request-FP does not silently rewrite URLs.

## Diagnostics

Build the included diagnostic example:

```powershell
cd examples\ssl_debug
lazbuild --build-mode=Debug ssl_debug.lpi
.\ssl_debug.exe
```

On Windows, debug output includes the executable architecture, OpenSSL
version, and actual DLL paths:

```text
[DEBUG] Initializing OpenSSL...
[DEBUG] OpenSSL initialized successfully (Windows)
[DEBUG] libssl loaded from: C:\Program Files\OpenSSL\bin\libssl-3-x64.dll
[DEBUG] libcrypto loaded from: C:\Program Files\OpenSSL\bin\libcrypto-3-x64.dll
[DEBUG] OpenSSL version: OpenSSL 3.6.3 ...
```

You can also check what is visible through `PATH`:

```powershell
where.exe libssl-3-x64.dll
where.exe libcrypto-3-x64.dll
```

## Common failures

### Could not initialize OpenSSL library

Check that:

- both DLLs are present;
- their architecture matches the executable;
- they came from the same OpenSSL build; and
- their directory is beside the executable or on `PATH`.

### OpenSSL works in a terminal but Request-FP cannot initialize it

`openssl.exe version` only proves that the command-line program can find its
own libraries. Run `examples/ssl_debug` to verify what the FPC application
loads.

### The wrong version is loaded

Windows searches the executable directory before most `PATH` locations.
Place the intended pair beside the executable, then use the debug example to
confirm the loaded paths. Do not rename or remove DLLs from `System32`.

## See also

- [OpenSSL version selection](OPENSSL-VERSION-SELECTION.md)
- [Technical details](TECHNICAL-DETAILS.md#windows-openssl-3-loading-with-fpc-322)
- [SSL debug example](../examples/ssl_debug/)
- [API reference](Request.md)
