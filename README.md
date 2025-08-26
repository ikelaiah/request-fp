# Request-FP

[![License: MIT](https://img.shields.io/badge/License-MIT-1E3A8A.svg)](https://opensource.org/licenses/MIT)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2+-3B82F6.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0+-60A5FA.svg)](https://www.lazarus-ide.org/)
![Supports Windows](https://img.shields.io/badge/support-Windows-F59E0B?logo=Windows)
![Supports Linux](https://img.shields.io/badge/support-Linux-F59E0B?logo=Linux)
[![Version](https://img.shields.io/badge/version-0.6.0-8B5CF6.svg)](CHANGELOG.md)
![No Dependencies](https://img.shields.io/badge/dependencies-none-10B981.svg)

Zero‑memory‑leak, high‑level HTTP client for Free Pascal. Built on top of FPC's HTTP stack with a clean API and zero boilerplate.

## ❓ Why Request-FP?

Request-FP is a thin, high-level wrapper around the Free Pascal HTTP client. If you like the built-in client but want fewer lines of code and safer lifetimes, this library gives you:

- Less boilerplate with expressive, procedural helpers.
- RAII-style advanced records for automatic cleanup (no leaks).
- A consistent, predictable API for common tasks (headers, params, JSON, multipart).

Use it when you want the power of FPC's HTTP client without the repetitive setup and manual memory management.

## ✨ Features

- **Zero memory leaks:** RAII-style advanced records handle cleanup for you.
- **High-level API over FPC:** Built on the stock Free Pascal HTTP client—no extra runtime deps.
- **Stateless or session-based:** `Http.Get(...)` for quick calls, `THttpSession` for cookies/state.
- **Headers, params, JSON, multipart:** First-class helpers for common patterns.
- **Simple error handling:** Exceptions or try-pattern results—your choice.
- **Battle-tested:** Cross-platform with a comprehensive test suite.

## ⚡ Getting Started in 30 Seconds

```pascal
uses Request;
var
  R: TResponse;
begin
  R := Http.Get('https://httpbin.org/get');
  WriteLn(R.Text);
end;
```

> That's it! No manual memory management, no setup headaches.


## Usage Styles

Request-FP offers two ways to make HTTP requests:

| If you want...                        | Use this style         | Example                |
|---------------------------------------|------------------------|------------------------|
| The simplest, one-off requests        | Stateless API          | `Http.Get(...)`        |
| To keep cookies/headers across calls  | Session API            | `THttpSession`         |

- **Start with the stateless API** for quick scripts, demos, or simple tools.
- **Use the session API** if you need to log in, reuse cookies, or make many related requests.

> Most users only need the stateless API. The session API is there for advanced needs—use it when you need more control or state.

> Note: `THttpSession` is an advanced record—call `Session.Init` before using methods like `Session.Get(...)`.

## Examples

### Simple GET
```pascal
Response := Http.Get('https://api.example.com/data');
WriteLn(Response.Text);
```

### GET with custom headers and query parameters
```pascal
Response := Http.Get('https://api.example.com/data',
  [TKeyValue.Create('X-Api-Key', 'my-secret-key')],
  [TKeyValue.Create('search', 'pascal')]);
WriteLn(Response.Text);
```

### POST with data
```pascal
Response := Http.Post('https://api.example.com/data', 'foo=bar');
```

### POST with custom headers and params

```pascal
Response := Http.Post('https://api.example.com/data', 'foo=bar',
  [TKeyValue.Create('Authorization', 'Bearer token')],
  [TKeyValue.Create('debug', '1')]);
```

### POST JSON
```pascal
Response := Http.PostJSON('https://api.example.com/data', '{"foo": "bar"}');
```

### Multipart upload

```pascal
Response := Http.PostMultipart('https://api.example.com/upload',
  [TKeyValue.Create('field1', 'value1')],
  [TKeyValue.Create('file1', 'myfile.txt')]);
```

### Error handling
```pascal
try
  Response := Http.Get('https://api.example.com/secure');
except
  on E: ERequestError do
    WriteLn('HTTP Error: ', E.Message);
end;
```

### Try-pattern (no exceptions)
```pascal
Result := Http.TryGet('https://api.example.com/secure');
if Result.Success then
  WriteLn('Status: ', Result.Response.StatusCode)
else
  WriteLn('Error: ', Result.Error);
```

## 🧪 Testing

Request-FP includes a comprehensive test suite that ensures reliability and catches regressions.

First, compile the test suite using Lazarus IDE or `lazbuild`.

### Running Tests

```bash
# Navigate to the tests directory
cd tests

# Run all tests
./TestRunner.exe -a --format=plain

# On Linux
./TestRunner -a --format=plain
```

### Test Coverage

- ✅ Comprehensive tests for HTTP methods, headers/params, JSON, multipart, and error handling
- ✅ Cross-platform: Windows and Linux
- ✅ Memory-safe by construction: advanced records manage lifetimes; JSON parse errors raise `ERequestError`

## 📚 Documentation

### Quick References

- **[📋 Cheat Sheet](docs/Cheat-Sheet.md)** - Quick reference for common patterns
- **[📖 API Reference](docs/Request.md)** - Complete API documentation
- **[📖 Session API](docs/Request.Session.md)** - Session-based HTTP client guide
- **[🔧 Technical Details](docs/TECHNICAL-DETAILS.md)** - Implementation details

### Examples

Explore practical examples in the [`examples/`](examples/) directory:
- **[Basic GET](examples/easy_get/)** - Simple HTTP GET request
- **[Custom Headers](examples/custom_headers_params/)** - Headers and query parameters
- **[Authentication](examples/basic_auth/)** - Basic authentication
- **[JSON POST](examples/post_json/)** - POST requests with JSON
- **[File Upload](examples/multipart_upload/)** - Multipart file uploads
- **[File Download](examples/file_download/)** - Download files
- **[Error Handling](examples/retry_on_error/)** - Robust error handling
- **[Sessions](examples/session_easy_get/)** - Session-based requests

## 🚀 Installation

1. **Clone the repository**:
   ```bash
   git clone https://github.com/iwank/request-fp.git
   cd request-fp
   ```

2. **Add to your project**:
   - Copy `src/Request.pas` and `src/Request.Session.pas` to your project
   - Add the `src` directory to your unit search path
   - Include `uses Request;` in your code

3. **Dependencies**:
   - **Free Pascal 3.2.2+** or **Lazarus 4.0+**
   - **OpenSSL** (for HTTPS support)
     - Windows: Usually included
     - Linux: `sudo apt-get install libssl-dev` (Ubuntu/Debian)

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details.

### Development Setup

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/amazing-feature`
3. Make your changes
4. Run tests: `cd tests && ./TestRunner.exe -a --format=plain`
5. Commit your changes: `git commit -m 'Add amazing feature'`
6. Push to the branch: `git push origin feature/amazing-feature`
7. Open a Pull Request

## 📋 Requirements

- **Free Pascal 3.2.2+** or **Lazarus 4.0+**
- **OpenSSL** libraries (for HTTPS)
- **Windows** or **Linux** (cross-platform)

## 📄 License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.


## 🙏 Acknowledgments

- [Free Pascal Dev Team](https://www.freepascal.org/) for the Pascal compiler
- [Lazarus IDE Team](https://www.lazarus-ide.org/) for such an amazing IDE
- The kind and helpful individuals on various online platforms such as:
    - [Unofficial Free Pascal discord server](https://discord.com/channels/570025060312547359/570091337173696513)
    - [Free Pascal & Lazarus forum](https://forum.lazarus.freepascal.org/index.php)
    - [Tweaking4All Delphi, Lazarus, Free Pascal forum](https://www.tweaking4all.com/forum/delphi-lazarus-free-pascal/)
    - [Laz Planet - Blogspot](https://lazplanet.blogspot.com/) / [Laz Planet - GitLab](https://lazplanet.gitlab.io/)
    - [Delphi Basics](https://www.delphibasics.co.uk/index.html)
- All contributors who have helped improve this project