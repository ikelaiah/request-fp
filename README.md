# Request-FP

[![License: MIT](https://img.shields.io/badge/License-MIT-2dce89.svg)](LICENSE.md)
[![Version](https://img.shields.io/badge/version-0.6.0-2d8cf0.svg)](https://github.com/iwank/request-fp/releases/tag/v0.6.0)
[![Platform](https://img.shields.io/badge/platform-Windows%20%7C%20Linux-2d8cf0.svg)](https://www.freepascal.org/)
[![FPC Version](https://img.shields.io/badge/FPC-3.2.2-2d8cf0.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0-2dce89.svg)](https://www.lazarus-ide.org/)


## 🚧 Development Status: Stable Release

> [!Note] 
> Request-FP has been successfully refactored to use a clean procedural API (version 0.6.0). All tests pass and the API is production-ready. The library now offers excellent ergonomics with `TKeyValue.Create()` helpers and comprehensive method overloads.

## ❓ Why Request-FP?

> Want modern, readable HTTP code in Pascal? Request-FP gives you a clean, memory-safe, and expressive API for all your HTTP needs—no boilerplate, no leaks, just results.

**Perfect for anyone writing HTTP clients in Free Pascal, from hobbyists to professionals.**

## ✨ Features

- **Zero Memory Leaks:** Advanced records handle cleanup for you.
- **Clean Procedural API:** Simple, stateless methods with ergonomic overloads.
- **Easy Key-Value Creation:** Use `TKeyValue.Create('key', 'value')` for clean syntax.
- **Custom Headers & Query Parameters:** Easily add headers and params to any request.
- **Battle-Tested:** 100% passing test suite, cross-platform.
- **Clear Error Handling:** Robust JSON support and explicit exceptions.

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

---

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

---