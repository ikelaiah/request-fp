# Request-FP

A modern, memory-safe HTTP client library for Free Pascal, inspired by Python's requests library. Now includes both `Request.pas` (stateless, fluent API) and `Request.Session.pas` (session-based, connection-pooling API). Built with advanced records for automatic memory management and a fluent interface for expressive HTTP requests.

```pascal
// Simple GET request (stateless)
var
  Response: TResponse;
begin
  Response := Http.Get('https://api.example.com/data');
  if Response.StatusCode = 200 then
    WriteLn('Response: ', Response.Text);
end;

// Session-based GET request
var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  Response := Session.Get('/data');
  if Response.StatusCode = 200 then
    WriteLn('Session Response: ', Response.Text);
end;
```

## Features ✨

- **Fluent Interface** - Chain methods for clean, readable code
- **Session Support** - Persistent cookies, headers, and connection pooling via `THttpSession`
- **Automatic Memory Management** - Uses advanced records for hassle-free cleanup
- **JSON Support** - Built-in JSON parsing with FPC's standard `fpjson`
- **Memory Safe** - Clear ownership model prevents memory leaks and access violations
- **HTTPS/SSL** - Secure connections with OpenSSL
- **Cross-Platform** - Works on Windows and Linux
- **Tested** - Comprehensive test suite

## Library Structure

- `src/Request.pas` — Stateless, fluent HTTP API (like Python's `requests`)
- `src/Request.Session.pas` — Session-based API with persistent cookies, headers, and connection reuse

## How does Request-FP compare to Python's requests?

| Feature                | Python requests         | Request-FP (Pascal)           |
|------------------------|------------------------|-------------------------------|
| Stateless API          | Yes                    | Yes (`Request.pas`)           |
| Session API            | Yes (`requests.Session`)| Yes (`Request.Session.pas`)   |
| Automatic cleanup      | Yes (GC)               | Yes (advanced records)        |
| JSON support           | Yes                    | Yes (via `fpjson`)            |
| Cookie persistence     | Yes                    | Yes (in `THttpSession`)       |
| Connection pooling     | Yes                    | Yes (in `THttpSession`)       |
| Fluent interface       | Yes                    | Yes                           |
| Exception handling     | Yes                    | Yes (`ERequestError`)         |
| Platform support       | Cross-platform         | Cross-platform                |
| SSL/HTTPS              | Yes                    | Yes                           |
| Memory safety          | Yes (GC)               | Yes (no leaks, no AVs)        |
| Type safety            | Dynamic                | Strong static typing          |
| Language               | Python                 | Free Pascal                   |

- **Request-FP** provides both a stateless API (like `requests.get/post`) and a session API (like `requests.Session`).
- Memory is managed automatically (no manual `Free`/`Dispose`), similar to Python's garbage collection.
- Session cookies, headers, and connection reuse are supported via `THttpSession`.
- JSON parsing is built-in and safe.
- Exception handling is explicit via `ERequestError`.

## Dependencies

- Windows
  - No external dependencies required
- Linux
  - Ubuntu/Debian: `sudo apt-get install libssl-dev` (needed for HTTPS in `Request`)
  - Fedora/RHEL: `sudo dnf install openssl-devel` (needed for HTTPS in `Request`)
- Uses only standard Free Pascal RTL units

## Memory Management ℹ️

### Request and Response Objects
- `THttpRequest`, `THttpSession`, and `TResponse` are implemented as **advanced records** with automatic memory management
- No need to manually create or free these objects
- Memory is automatically managed when variables go out of scope

### JSON Data Handling
- JSON objects returned by `Response.JSON` are **owned by the TResponse object**
- **Do not free** JSON objects obtained via `FindPath` or similar methods
- The `TResponse` destructor will automatically free all associated JSON data

#### ✅ Correct Usage
```pascal
var
  Response: TResponse;
  UserData: TJSONObject;
begin
  Response := Http.Get('https://api.example.com/user/1');
  try
    UserData := TJSONObject(Response.JSON.FindPath('user'));
    // No need to free UserData - it's owned by Response
    if Assigned(UserData) then
      WriteLn('User: ', UserData.Get('name', 'Unknown'));
  finally
    // Response is automatically managed
  end;
end;
```

#### ❌ Incorrect Usage
```pascal
// WRONG: Don't free objects from FindPath
UserData := TJSONObject(Response.JSON.FindPath('user'));
try
  // Use UserData...
finally
  UserData.Free; // This will cause an access violation!
end;
```

### Best Practices
1. Always check if JSON objects are `nil` before accessing them
2. Never manually free JSON objects obtained from `TResponse`
3. Use `try/finally` blocks for request/response handling when needed
4. Let the `TResponse` record handle all memory cleanup

## Quick Start

### Installation
1. Add the `src` directory to your project's search path
2. Add `Request` and/or `Request.Session` to your uses clause
3. For HTTPS support, ensure OpenSSL libraries are installed

### Examples

#### Basic GET Request (stateless)
```pascal
var
  Response: TResponse;
begin
  Response := Http.Get('https://httpbin.org/get');
  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);
end;
```

#### Session-based GET Request
```pascal
var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.Init;
  Session.SetBaseURL('https://httpbin.org');
  Response := Session.Get('/get');
  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);
end;
```

#### POST with JSON
```pascal
var
  Response: TResponse;
  UserData: TJSONObject;
begin
  UserData := TJSONObject.Create;
  try
    UserData.Add('name', 'John');
    UserData.Add('email', 'john@example.com');
    Response := Http.PostJSON('https://httpbin.org/post', UserData.AsJSON);
    WriteLn('Status: ', Response.StatusCode);
    WriteLn('Response: ', Response.Text);
  finally
    UserData.Free;
  end;
end;
```

#### Using the Fluent Interface
```pascal
var
  Response: TResponse;
begin
  Response := THttpRequest
    .Create
    .Get
    .URL('https://httpbin.org/headers')
    .AddHeader('X-Custom-Header', 'test')
    .WithTimeout(5000)
    .Send;
  if Response.StatusCode = 200 then
    WriteLn('Response: ', Response.Text);
end;
```

## Documentation

For detailed documentation, see [docs/Request.md](docs/Request.md).

## Running Tests

1. Open the project in your favorite Free Pascal IDE
2. Add the `tests` directory to your test project
3. Run the test suite

## Requirements

- FreePascal 3.2.2 or later
- For HTTPS support:
  - Windows: OpenSSL libraries (included)
  - Linux: `libssl-dev` or equivalent

## Project Documentation

- [📖 Documentation](docs/Request.md) - Detailed API reference and usage examples
- [📋 Cheat Sheet](docs/cheat-sheet.md) - Quick reference for common operations
- [⚖️ Code of Conduct](CODE_OF_CONDUCT.md) - Community guidelines and standards
- [🤝 Contributing](CONTRIBUTING.md) - How to contribute to the project
- [🔒 Security Policy](SECURITY.md) - Reporting security vulnerabilities
- [📄 License](LICENSE.md) - MIT License terms and conditions

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Contributing

Contributions are welcome! Please read our [Contributing Guidelines](CONTRIBUTING.md) and [Code of Conduct](CODE_OF_CONDUCT.md) before making contributions.
