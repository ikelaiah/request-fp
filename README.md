# Request-FP

[![License: MIT](https://img.shields.io/badge/License-MIT-2dce89.svg)](LICENSE.md)
[![Version](https://img.shields.io/badge/version-0.5.0-2d8cf0.svg)](https://github.com/iwank/request-fp/releases/tag/v0.5.0)
[![Platform](https://img.shields.io/badge/platform-Windows%20%7C%20Linux-2d8cf0.svg)](https://www.freepascal.org/)
[![FPC Version](https://img.shields.io/badge/FPC-3.2.2-2d8cf0.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0-2dce89.svg)](https://www.lazarus-ide.org/)

---

## ❓ Why Request-FP?

> Want modern, readable HTTP code in Pascal? Request-FP gives you a clean, memory-safe, and expressive API for all your HTTP needs—no boilerplate, no leaks, just results. If you like fluent, easy-to-use libraries, you'll feel right at home.

**Perfect for anyone writing HTTP clients in Free Pascal, from hobbyists to professionals.**

---

## ✨ Features

- **Zero Memory Leaks:** Advanced records handle cleanup for you.
- **Fluent, Modern API:** Chain methods for clarity and power.
- **Stateless or Session:** Use what fits your workflow.
- **Battle-Tested:** 100% passing test suite, cross-platform.
- **Clear Error Handling:** Robust JSON support and explicit exceptions.

---

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

---

## 🙋 FAQ

**Q: Do I need to free anything?**  
A: Nope! All cleanup is automatic.

**Q: Does it work on Linux and Windows?**  
A: Yes, fully supported and tested.

**Q: Is it suitable for both simple and advanced use?**  
A: Yes! Use the stateless API for quick calls, or sessions for advanced scenarios.

**Q: How do I get help?**  
A: [Open an issue](https://github.com/iwank/request-fp/issues) or [start a discussion](https://github.com/iwank/request-fp/discussions) on GitHub.

---

**Request-FP** is a modern, memory-safe HTTP client library for Free Pascal, inspired by Python's legendary [requests](https://docs.python-requests.org/) library. Effortlessly make HTTP requests with a clean, fluent API—stateless or session-based—while enjoying automatic memory management and robust JSON support.

---

- 📄 [License (MIT)](LICENSE.md)
- 🤝 [Contributing Guide](CONTRIBUTING.md)
- ⚖️ [Code of Conduct](CODE_OF_CONDUCT.md)

---

## 🚀 Quick Example

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

---

## 🏗️ Library Structure

- [`src/Request.pas`](src/Request.pas) — Stateless, fluent HTTP API (like Python's `requests`)
- [`src/Request.Session.pas`](src/Request.Session.pas) — Session-based API with persistent cookies, headers, and connection reuse

---

## 🐍 How does Request-FP compare to Python's requests?

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
| Multipart upload       | Yes                    | Yes (stateless/session, memory-safe, dynamic arrays) |
| Platform support       | Cross-platform         | Cross-platform                |
| SSL/HTTPS              | Yes                    | Yes                           |
| Memory safety          | Yes (GC)               | Yes (no leaks, no AVs)        |
| Type safety            | Dynamic                | Strong static typing          |
| Language               | Python                 | Free Pascal                   |

---

## 🛠️ Installation

1. Add the `src` directory to your project's search path
2. Add `Request` and/or `Request.Session` to your uses clause
3. For HTTPS support, ensure OpenSSL libraries are installed

---

## 📦 Dependencies

- **Windows:** No external dependencies required
- **Linux:**
  - Ubuntu/Debian: `sudo apt-get install libssl-dev`
  - Fedora/RHEL: `sudo dnf install openssl-devel`
- Uses only standard Free Pascal RTL units

---

## 🧠 Memory Management & JSON Handling

- `THttpRequest`, `THttpSession`, and `TResponse` are **advanced records** with automatic memory management
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

---

## 🏁 Quick Start Examples

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

---

## 📤 Multipart File Uploads

Request-FP supports robust, memory-safe multipart file uploads using dynamic arrays, fully compatible with FPC 3.2.2 and both stateless and session APIs.

#### Fluent API Example

```pascal
var
  Request: THttpRequest;
  Response: TResponse;
  TempFile: string;
begin
  // Create a temporary file to upload
  TempFile := GetTempDir + 'test_upload.txt';
  // ... write to TempFile ...

  Request := Request.Post
    .URL('https://httpbin.org/post')
    .AddFile('file1', TempFile)
    .AddFormField('field1', 'value1');
  Response := Request.Send;

  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);
end;
```

#### Static API Example

```pascal
var
  Response: TResponse;
  Fields, FilesArr: array of TKeyValue;
begin
  SetLength(Fields, 1);
  Fields[0].Key := 'staticfield';
  Fields[0].Value := 'staticvalue';

  SetLength(FilesArr, 1);
  FilesArr[0].Key := 'file2';
  FilesArr[0].Value := 'path/to/file.txt';

  Response := Http.PostMultipart('https://httpbin.org/post', Fields, FilesArr);
  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);
end;
```

- No manual memory management is required for multipart fields/files.
- Multipart logic is only triggered if files or fields are added; all state is reset after each request.
- 100% passing test suite, including multipart upload tests.

---

## 📚 Documentation

- [📖 API Reference](docs/Request.md)
- [📋 Cheat Sheet](docs/cheat-sheet.md)
- [⚙️ Technical Details](docs/TECHNICAL-DETAILS.md)

---

## 🧪 Running Tests

1. Open the project in your favorite Free Pascal IDE
2. Add the `tests` directory to your test project
3. Run the test suite

---

## 📝 License

This project is licensed under the [MIT License](LICENSE.md).

---

## 🤝 Contributing

Contributions are welcome! Please read our [Contributing Guidelines](CONTRIBUTING.md) and [Code of Conduct](CODE_OF_CONDUCT.md) before making contributions.
