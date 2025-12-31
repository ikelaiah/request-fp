# Request.Session User Manual

A robust, memory-safe HTTP session library for Free Pascal, providing persistent cookies, headers, and connection reuse. Built with advanced records for automatic memory management and a clear, session-oriented API. Inspired by Python's requests.Session, but with strong Pascal typing and safety.

## Table of Contents

- [Request.Session User Manual](#requestsession-user-manual)
  - [Table of Contents](#table-of-contents)
  - [Features](#features)
  - [System Requirements](#system-requirements)
  - [Design Philosophy](#design-philosophy)
    - [Session-Oriented API](#session-oriented-api)
  - [Memory Safety](#memory-safety)
  - [Basic Usage](#basic-usage)
    - [Session GET Request](#session-get-request)
    - [Session POST Request](#session-post-request)
    - [Working with Cookies and Headers](#working-with-cookies-and-headers)
    - [Working with Cookies, Headers, and JSON](#working-with-cookies-headers-and-json)
  - [Error Handling](#error-handling)
  - [API Reference](#api-reference)
    - [THttpSession Record](#thttpsession-record)
    - [TSimpleMap Record](#tsimplemap-record)
  - [Advanced Usage Examples](#advanced-usage-examples)
    - [Authenticated Session](#authenticated-session)
    - [Session with Custom Timeout](#session-with-custom-timeout)
  - [Testing and Development](#testing-and-development)
  - [Best Practices](#best-practices)

## Features

- Persistent cookies and headers across requests
- Connection reuse for performance
- Session configuration (base URL, user-agent, timeout)
- Memory-safe: no manual cleanup required
- Robust error handling with `ERequestError`
- Cross-platform (Windows, Linux)
- Simple, clear API for session-based HTTP

## System Requirements

- Free Pascal 3.2.2 or later
- For HTTPS:
  - **Windows**: Install OpenSSL via [Chocolatey](https://chocolatey.org/) (`choco install openssl`), [Scoop](https://scoop.sh/) (`scoop install openssl`), or download the [Win64 OpenSSL installer](https://slproweb.com/products/Win32OpenSSL.html). Copy the required DLLs (`libssl-*.dll` and `libcrypto-*.dll`) into your executable folder or add their location to PATH. See [Request.md Troubleshooting](Request.md#troubleshooting) for specific DLL names and detailed instructions.
  - **Linux**: `sudo apt-get install libssl-dev` (Ubuntu/Debian) or `sudo dnf install openssl-devel` (Fedora/RHEL)

## Design Philosophy

### Session-Oriented API

`Request.Session` provides a stateful HTTP client, similar to Python's `requests.Session`, but with Pascal's strong typing and memory safety. Sessions allow you to:
- Reuse cookies and headers
- Set a base URL for all requests
- Maintain connection pooling for efficiency

## Memory Safety

- Uses advanced records for automatic initialization and cleanup
- No manual memory management needed for sessions, headers, or cookies
- Safe to use in try-except blocks

## Basic Usage

### Session GET Request

```pascal
var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  Response := Session.Get('/data');
  if Response.StatusCode = 200 then
    WriteLn('Data: ', Response.Text);
end;
```

### Session POST Request

```pascal
var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  Session.SetHeader('Authorization', 'Bearer ...');
  Response := Session.Post('/submit', '{"name":"John"}', 'application/json');
  if Response.StatusCode = 201 then
    WriteLn('Created: ', Response.Text);
end;
```

### Working with Cookies and Headers

```pascal
var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  Session.SetCookie('sessionid', 'abc123');
  Session.SetHeader('X-API-Key', 'mykey');
  Response := Session.Get('/profile');
  WriteLn('Profile: ', Response.Text);
end;
```

### Working with Cookies, Headers, and JSON

```pascal
var
  Session: THttpSession;
  Response: TResponse;
  UserData, ResponseData: TJSONObject;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  Session.SetCookie('sessionid', 'abc123');
  Session.SetHeader('X-API-Key', 'mykey');

  // Send JSON data
  UserData := TJSONObject.Create;
  try
    UserData.Add('name', 'John');
    UserData.Add('email', 'john@example.com');
    Response := Session.Post('/profile', UserData.AsJSON, 'application/json');
    if Response.StatusCode = 200 then
    begin
      ResponseData := TJSONObject(Response.JSON);
      // No need to free ResponseData - it's owned by Response
      WriteLn('Profile: ', ResponseData.Get('name', ''));
    end;
  finally
    UserData.Free;
  end;
end;
```

> **Note:** JSON objects returned by `Response.JSON` are owned by the response. Do not free them manually.

## Error Handling

All session methods may raise `ERequestError` for transport failures and non-2xx HTTP statuses. Use try-except blocks for robust error handling:
```pascal
try
  Response := Session.Get('/data');
except
  on E: ERequestError do
    WriteLn('HTTP Error: ', E.Message);
end;
```

### JSON Parse Errors

Calling `Response.JSON` parses the current response body. If the content is not valid JSON, an `ERequestError` is raised with the prefix "JSON Parse Error". Access `Response.Text` for raw content if parsing is not desired.

## API Reference

### THttpSession Record

```pascal
THttpSession = record
  procedure Init;  // REQUIRED: Must be called before using the session
  function Get(const URL: string): TResponse;
  function Post(const URL: string; const Body: string = ''; const ContentType: string = 'application/x-www-form-urlencoded'): TResponse;
  function Put(const URL: string; const Body: string = ''; const ContentType: string = 'application/json'): TResponse;
  function Delete(const URL: string): TResponse;
  procedure SetHeader(const Name, Value: string);
  procedure SetCookie(const Name, Value: string);
  procedure SetBaseURL(const URL: string);
  procedure SetUserAgent(const UserAgent: string);
  procedure SetTimeout(Timeout: Integer);
  procedure ClearCookies;
  function GetCookie(const Name: string): string;
  procedure ClearHeaders;
  
  // Memory management (called automatically)
  class operator Initialize(var Session: THttpSession);
  class operator Finalize(var Session: THttpSession);
  class operator Copy(constref Source: THttpSession; var Dest: THttpSession);
end;
```

### TSimpleMap Record

Used internally for headers and cookies. No manual management needed.

```pascal
TSimpleMap = record
  procedure Add(const Key, Value: string);        // Alias for SetItem
  procedure SetItem(const Key, Value: string);
  function Get(const Key: string; const Default: string = ''): string;
  function ContainsKey(const Key: string): Boolean;
  procedure Remove(const Key: string);
  procedure Clear;
  function GetCount: Integer;
  function GetKey(Index: Integer): string;
  function GetValue(Index: Integer): string;
  function IndexOf(const Key: string): Integer;
end;
```

## Advanced Usage Examples

### Authenticated Session

```pascal
var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  Session.SetHeader('Authorization', 'Bearer mytoken');
  Response := Session.Get('/secure-data');
  if Response.StatusCode = 200 then
    WriteLn('Secure: ', Response.Text);
end;
```

### Session with Custom Timeout

```pascal
var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  Session.SetTimeout(10000); // 10 seconds
  Response := Session.Get('/slow-endpoint');
  if Response.StatusCode = 200 then
    WriteLn('Slow data: ', Response.Text);
end;
```

### Session Multipart Upload

While the session API does not expose a dedicated multipart helper, you can send multipart/form-data by constructing the body and content-type:

```pascal
var
  Session: THttpSession;
  Boundary, Body, CRLF: string;
  Response: TResponse;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  Boundary := '----RequestFP' + IntToStr(Random(100000));
  CRLF := #13#10;
  Body := '--' + Boundary + CRLF +
          'Content-Disposition: form-data; name="field1"' + CRLF + CRLF +
          'value1' + CRLF +
          '--' + Boundary + '--' + CRLF;
  Response := Session.Post('/upload', Body, 'multipart/form-data; boundary=' + Boundary);
  if Response.StatusCode = 200 then
    WriteLn('Uploaded');
end;
```

For an exception-free variant using the procedural API, see `Http.TryPostMultipart(...)` documented in `docs/Request.md`.

## Testing and Development

- Add the `src` directory to your project
- Use `Request.Session` in your `uses` clause
- Tests target `https://httpbin.org` and require outbound network access in CI
- Some httpbin upstreams can intermittently return 502; the test suite includes a minimal one-time retry on HTTP 502 to reduce flakiness. Core library behavior is unchanged.

## Best Practices

1. Always call `Session.Init` before use
2. Set base URL and headers/cookies as needed
3. Use try-except for error handling
4. Let the session manage memory and cleanup
5. Use `ClearCookies` and `ClearHeaders` to reset session state if needed
