# SSL/HTTPS Configuration Guide

## How HTTPS is Determined

Request-FP automatically uses HTTPS when:
1. The URL starts with `https://`
2. This triggers automatic SSL initialization before making the request

There is **no global switch** to force all requests to use HTTPS, as this is determined by the URL protocol.

## Ensuring HTTPS is Used

### Option 1: Always Use `https://` in URLs

The simplest approach - explicitly use HTTPS URLs:

```pascal
// Good - uses HTTPS
Response := Http.Get('https://api.example.com/data');

// Bad - uses HTTP (unencrypted)
Response := Http.Get('http://api.example.com/data');
```

### Option 2: Use Base URL with Sessions

For session-based requests, set the base URL with `https://`:

```pascal
var
  Session: THttpSession;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');  // Forces HTTPS for all session requests

  // All these use HTTPS because of the base URL:
  Response := Session.Get('/users');
  Response := Session.Post('/data', 'payload');
end;
```

### Option 3: Create a Helper Function

Wrap the HTTP functions to enforce HTTPS:

```pascal
function SecureGet(const URL: string): TResponse;
begin
  if Pos('https://', LowerCase(URL)) <> 1 then
    raise ERequestError.Create('Only HTTPS URLs are allowed');
  Result := Http.Get(URL);
end;

// Usage:
Response := SecureGet('https://api.example.com/data');  // OK
Response := SecureGet('http://api.example.com/data');   // Raises exception
```

### Option 4: URL Validation Function

Create a validation helper:

```pascal
function EnsureHTTPS(const URL: string): string;
begin
  if Pos('http://', LowerCase(URL)) = 1 then
    Result := 'https://' + Copy(URL, 8, Length(URL))  // Replace http:// with https://
  else if Pos('https://', LowerCase(URL)) = 1 then
    Result := URL  // Already HTTPS
  else
    Result := 'https://' + URL;  // Add https:// prefix
end;

// Usage:
Response := Http.Get(EnsureHTTPS('api.example.com/data'));       // Adds https://
Response := Http.Get(EnsureHTTPS('http://api.example.com'));    // Converts to https://
Response := Http.Get(EnsureHTTPS('https://api.example.com'));   // No change
```

## Debug Mode: See Where DLLs Are Loaded

### Enable Debug Output

Compile your program with DEBUG mode to see SSL initialization details:

```bash
fpc -dDEBUG yourprogram.pas
```

Or add to your source code:

```pascal
{$DEFINE DEBUG}
```

### What Debug Mode Shows

When DEBUG mode is enabled, you'll see output like:

**On Windows:**
```
[DEBUG] Initializing OpenSSL...
[DEBUG] OpenSSL initialized successfully (Windows)
[DEBUG] OpenSSL version: OpenSSL 3.0.11 19 Sep 2023
```

**On Linux:**
```
[DEBUG] Initializing OpenSSL...
[DEBUG] OpenSSL initialized successfully (Unix)
```

This shows:
- When SSL initialization begins
- Confirmation of successful initialization
- **OpenSSL version**: Exact version string from the loaded library (Windows only, as returned by the library)

### Finding DLL Locations on Windows

The debug handles don't show the file path, but you can find where DLLs are loaded from using:

```bash
# Check system PATH for OpenSSL DLLs
where libssl-3-x64.dll
where libcrypto-3-x64.dll

# Or for OpenSSL 1.1.x:
where libssl-1_1-x64.dll
where libcrypto-1_1-x64.dll
```

### Advanced DLL Path Detection (Windows)

For a more detailed approach, you can use Windows API to get the loaded DLL path:

```pascal
{$IFDEF WINDOWS}
uses
  Windows;

function GetLoadedDLLPath(Handle: THandle): string;
var
  Path: array[0..MAX_PATH] of Char;
begin
  if GetModuleFileName(Handle, Path, MAX_PATH) > 0 then
    Result := Path
  else
    Result := 'Unable to determine DLL path';
end;
{$ENDIF}

// Usage (in DEBUG mode after SSL is initialized):
WriteLn('SSL DLL path: ', GetLoadedDLLPath(SSLLibraryHandle));
WriteLn('Crypto DLL path: ', GetLoadedDLLPath(SSLUtilHandle));
```

## See Also

- [Troubleshooting Guide](../README.md#-troubleshooting) - OpenSSL installation help
- [SSL Debug Example](../examples/ssl_debug/) - Example program with debug output
- [API Reference](Request.md) - Full API documentation
