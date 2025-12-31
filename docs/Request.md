# Request Unit Documentation

A lightweight, memory-safe HTTP client for Free Pascal that uses advanced records for automatic cleanup. This module provides a simple, procedural interface for making HTTP requests with built-in memory management and JSON integration through the standard FPC fpjson unit.

## Table of Contents

- [Request Unit Documentation](#request-unit-documentation)
  - [Table of Contents](#table-of-contents)
  - [Features](#features)
  - [System Requirements](#system-requirements)
  - [Design Philosophy](#design-philosophy)
  - [Memory Safety](#memory-safety)
  - [Basic Usage](#basic-usage)
    - [Simple GET Request](#simple-get-request)
    - [Working with JSON](#working-with-json)
  - [Error Handling](#error-handling)
    - [Using Try-Pattern](#using-try-pattern)
    - [SSL/TLS Errors](#ssltls-errors)
  - [API Reference](#api-reference)
    - [TResponse Record](#tresponse-record)
    - [TRequestResult Record](#trequestresult-record)
    - [Global HTTP Functions](#global-http-functions)
  - [Advanced Usage Examples](#advanced-usage-examples)
    - [Multipart File Uploads](#multipart-file-uploads)
    - [Custom Headers and Query Parameters](#custom-headers-and-query-parameters)
    - [Checking Success and Saving to File](#checking-success-and-saving-to-file)
  - [Testing and Development](#testing-and-development)
  - [Best Practices](#best-practices)

## Features

- Zero-setup memory management using advanced records
- Simple procedural interface for expressive request building
- Support for common HTTP methods (GET, POST, PUT, DELETE)
- Seamless JSON integration with FPC's standard fpjson unit
- Error handling with result pattern
- Cross-platform support for Windows and Linux
- Automatic test environment detection and HTTP fallback
- **Custom headers and query parameters supported in all procedural/stateless HTTP methods**

## System Requirements

For basic HTTP functionality, no special requirements are needed. For HTTPS (SSL/TLS) support:

- **Windows**: Install OpenSSL via [Chocolatey](https://chocolatey.org/) (`choco install openssl`), [Scoop](https://scoop.sh/) (`scoop install openssl`), or download the [Win64 OpenSSL installer](https://slproweb.com/products/Win32OpenSSL.html). Copy the required DLLs (`libssl-*.dll` and `libcrypto-*.dll`) into your executable folder or add their location to PATH. See the [Troubleshooting](#troubleshooting) section for specific DLL names and detailed instructions.
- **Linux**:
  - Ubuntu/Debian: `sudo apt-get install libssl-dev`
  - Fedora/RHEL: `sudo dnf install openssl-devel`

The module will detect missing OpenSSL libraries and provide helpful error messages with installation instructions.

## Basic Usage

### Simple GET Request

```pascal
Response := Http.Get('https://api.example.com/data');
WriteLn(Response.Text);
```

### GET with custom headers and query parameters

```pascal
Response := Http.Get('https://api.example.com/data',
  [TKeyValue.Create('X-Api-Key', 'my-key')],
  [TKeyValue.Create('q', 'search')]);
WriteLn(Response.Text);
```

### Working with JSON

```pascal
if Response.StatusCode = 200 then
begin
  JsonObj := TJSONObject(Response.JSON);
  try
    WriteLn('Name: ', JsonObj.Get('name', ''));
  finally
    // Do NOT free JsonObj - it's owned by Response.JSON
  end;
end;
```

## Error Handling

### Using Try-Pattern

```pascal
Result := Http.TryGet('https://api.example.com/secure');
if Result.Success then
  WriteLn('Status: ', Result.Response.StatusCode)
else
  WriteLn('Error: ', Result.Error);
```

### Behavior Summary

- The procedural methods like `Http.Get/Post/Put/Delete/...` may raise `ERequestError` for transport issues (network/SSL/timeout). They still return non-2xx responses without raising.
- The Try* methods like `Http.TryGet/TryPost/TryPut/TryDelete` never raise exceptions. Inspect `Result.Success`, `Result.Error`, and `Result.Response`.
- Accessing `Response.JSON` parses the current `Response.Text`. If content is not valid JSON, an `ERequestError` is raised with prefix "JSON Parse Error".

### SSL/TLS Errors

If SSL libraries are missing, the library will provide a clear error message with installation instructions.

## API Reference

### TResponse Record

- `StatusCode: Integer` — HTTP status code
- `Text: string` — Response body as text
- `JSON: TJSONData` — Parsed JSON response (if applicable)
- `HeaderValue(const Name: string): string` — Get a response header value
- `IsSuccessStatus: Boolean` — True if `StatusCode` is 2xx
- `SaveToFile(const FilePath: string)` — Save the response body to a file (UTF-8)

Note: Response headers are captured for both the stateless (`Request`) and session (`Request.Session`) APIs, so `HeaderValue` works consistently in all cases.

### TRequestResult Record

- `Success: Boolean` — True if request succeeded
- `Response: TResponse` — The response (even on error)
- `Error: string` — Error message (if any)

### Global HTTP Functions

All procedural/stateless HTTP methods support optional custom headers and query parameters:

- `Http.Get(const URL: string; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TResponse`
- `Http.Post(const URL: string; const Data: string = ''; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TResponse`
- `Http.Put(const URL: string; const Data: string = ''; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TResponse`
- `Http.Delete(const URL: string; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TResponse`
- `Http.PostJSON(const URL: string; const JSON: string; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TResponse`
- `Http.PostMultipart(const URL: string; const Fields, Files: array of TKeyValue; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TResponse`
- `Http.TryGet(const URL: string; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TRequestResult`
- `Http.TryPost(const URL: string; const Data: string = ''; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TRequestResult`
- `Http.TryPut(const URL: string; const Data: string = ''; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TRequestResult`
- `Http.TryDelete(const URL: string; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TRequestResult`
- `Http.TryPostMultipart(const URL: string; const Fields, Files: array of TKeyValue; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TRequestResult`

## Advanced Usage Examples

### Multipart File Uploads

```pascal
Response := Http.PostMultipart('https://api.example.com/upload',
  [TKeyValue.Create('field1', 'value1')],
  [TKeyValue.Create('file1', 'myfile.txt')]);
```

### Multipart Uploads with Try-pattern

```pascal
var R: TRequestResult;
begin
  R := Http.TryPostMultipart('https://api.example.com/upload',
    [TKeyValue.Create('field1', 'value1')],
    [TKeyValue.Create('file1', 'myfile.txt')]);
  if R.Success and (R.Response.StatusCode = 200) then
    WriteLn('Uploaded OK')
  else
    WriteLn('Upload failed: ', R.Error);
end;
```

### Custom Headers and Query Parameters

```pascal
Response := Http.Get('https://api.example.com/data',
  [TKeyValue.Create('X-Api-Key', 'my-key'), TKeyValue.Create('Accept', 'application/json')],
  [TKeyValue.Create('lang', 'en')]);
WriteLn(Response.Text);
```

### Reading Response Headers

```pascal
var CT: string;
CT := Response.HeaderValue('Content-Type');
if Pos('application/json', LowerCase(CT)) > 0 then
  WriteLn('Looks like JSON');
```

### Checking Success and Saving to File

```pascal
if Response.IsSuccessStatus then
  Response.SaveToFile('output.json')
else
  WriteLn('HTTP error: ', Response.StatusCode);
```

## Troubleshooting

### OpenSSL Errors on Windows

If you encounter OpenSSL initialization errors on Windows (e.g., "OpenSSL initialization failed"), you need to install the OpenSSL DLLs:

**Required DLL Files:**

- **OpenSSL 1.1.x**: `libssl-1_1-x64.dll` and `libcrypto-1_1-x64.dll` (or `libssl-1_1.dll` / `libcrypto-1_1.dll` for 32-bit)
- **OpenSSL 3.x**: `libssl-3-x64.dll` and `libcrypto-3-x64.dll` (or `libssl-3.dll` / `libcrypto-3.dll` for 32-bit)

**Installation Options:**

1. **Via Package Manager (Recommended)**:
   - [Chocolatey](https://chocolatey.org/): `choco install openssl`
   - [Scoop](https://scoop.sh/): `scoop install openssl`

2. **Manual Installation**:
   - Download from [Shining Light Productions](https://slproweb.com/products/Win32OpenSSL.html)
   - Choose the appropriate installer for your architecture (Win64 or Win32)
   - Install to a location like `C:\OpenSSL-Win64\`

3. **Deploy DLLs**:
   - **Option A**: Copy the DLL files to the same folder as your executable
   - **Option B**: Add the OpenSSL `bin` directory to your system PATH environment variable

**Verifying Installation:**

```bash
# Check if OpenSSL DLLs are accessible
where libssl-3-x64.dll
where libcrypto-3-x64.dll
```

### OpenSSL Errors on Linux

If you encounter OpenSSL errors on Linux, install the development libraries:

**Ubuntu/Debian**:

```bash
sudo apt-get update
sudo apt-get install libssl-dev
```

**Fedora/RHEL**:

```bash
sudo dnf install openssl-devel
```

### Network and Certificate Errors

- **Certificate validation failures**: Ensure your system's CA certificates are up to date
- **Connection timeouts**: Check firewall settings and network connectivity
- **502 errors from httpbin**: Some test endpoints may intermittently return 502; this is a known httpbin upstream issue and does not indicate a library problem

---