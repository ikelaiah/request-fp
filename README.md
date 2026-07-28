# Request-FP

[![License: MIT](https://img.shields.io/badge/License-MIT-1E3A8A.svg)](https://opensource.org/licenses/MIT)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2+-3B82F6.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.8+-60A5FA.svg)](https://www.lazarus-ide.org/)
![Supports Windows](https://img.shields.io/badge/support-Windows-F59E0B?logo=Windows)
![Supports Linux](https://img.shields.io/badge/support-Linux-F59E0B?logo=Linux)
[![Version](https://img.shields.io/badge/version-1.3.0-8B5CF6.svg)](CHANGELOG.md)

An easy, memory-safe HTTP client for Free Pascal. Request-FP wraps FPC's HTTP
stack with a small API, automatic cleanup, and no application-level
dependencies.

## Start here

Add `src` to your unit search path and make a request:

```pascal
uses Request;

var
  Response: TResponse;
begin
  Response := Http.Get('https://api.example.com/users');
  Response.RaiseForStatus;
  WriteLn(Response.Text);
end.
```

There is no client to create or free. `TResponse` cleans itself up.

Use the stateless `Http` API for one-off requests. Use `THttpSession` when
requests need to share a base URL, headers, or cookies.

## Common requests

### Query parameters

`KV` is the short syntax for a header, query parameter, form field, or
multipart field:

```pascal
Response := Http.GetWithParams('https://api.example.com/search', [
  KV('q', 'free pascal'),
  KV('page', '2')
]);
```

Request-FP percent-encodes names and values as UTF-8.

### Headers and query parameters together

```pascal
Response := Http.Get('https://api.example.com/search',
  [KV('Authorization', 'Bearer ' + Token)],
  [KV('q', 'free pascal')]);
```

The second array is headers and the third is query parameters.
`TKeyValue.Create(...)` remains available for existing code.

### Form data

```pascal
Response := Http.PostForm('https://api.example.com/login', [
  KV('email', 'ada@example.com'),
  KV('password', Password)
]);
```

`PostForm` performs the form encoding and sets
`application/x-www-form-urlencoded`. Use `Http.Post` when the body is already
encoded or is another content type.

### JSON

Post a JSON string:

```pascal
Response := Http.PostJSON(
  'https://api.example.com/users',
  '{"name":"Ada"}'
);
```

Or pass an existing `TJSONData` value directly:

```pascal
uses Request, fpjson;

var
  Body: TJSONObject;
begin
  Body := TJSONObject.Create;
  try
    Body.Add('name', 'Ada');
    Response := Http.PostJSON('https://api.example.com/users', Body);
  finally
    Body.Free;
  end;
end;
```

Request-FP reads JSON responses lazily:

```pascal
WriteLn(Response.JSON.FindPath('user.name').AsString);
```

`Response` owns the parsed value returned by `Response.JSON`. Do not free that
value yourself. Invalid JSON raises `ERequestError` with a `JSON Parse Error`
message.

### Multipart file upload

```pascal
Response := Http.PostMultipart('https://api.example.com/upload',
  [KV('description', 'avatar')],
  [KV('file', 'avatar.png')]);
```

The key in the files array is the form field name; the value is the local file
path.

## Handling success and errors

Choose the style that fits your program.

### Exceptions for transport errors

Normal methods raise `ERequestError` for network, TLS, and request failures.
Inspect the response or opt in to raising for a non-2xx status:

```pascal
try
  Response := Http.Get('https://api.example.com/users/42');
  Response.RaiseForStatus;
  WriteLn(Response.Text);
except
  on E: ERequestError do
    WriteLn(E.Message);
end;
```

You can also branch without raising:

```pascal
if Response.OK then
  WriteLn(Response.Text)
else
  WriteLn('HTTP status: ', Response.StatusCode);
```

`Response.OK` and `Response.IsSuccessStatus` are true for status codes 200
through 299.

### No exceptions

Every `Try*` method catches request exceptions:

```pascal
Result := Http.TryGet('https://api.example.com/users/42');
if Result.OK then
  WriteLn(Result.Response.Text)
else if not Result.Success then
  WriteLn('Request failed: ', Result.Error)
else
  WriteLn('HTTP status: ', Result.Response.StatusCode);
```

- `Result.Success` means the HTTP exchange completed, even if the server
  returned 404 or 500.
- `Result.OK` means the exchange completed and the status is 2xx.
- `TryGet`, `TryGetWithParams`, `TryPost`, `TryPostForm`, `TryPostJSON`,
  `TryPut`, `TryDelete`, and `TryPostMultipart` do not raise request
  exceptions.

## Sessions

Add `Request.Session` when calls should share configuration or cookies:

```pascal
uses Request, Request.Session;

var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.SetBaseURL('https://api.example.com');
  Session.SetHeader('Authorization', 'Bearer ' + Token);

  Response := Session.Get('/profile');
  if Response.OK then
    WriteLn(Response.Text);
end.
```

Sessions initialize and clean themselves up automatically. You do not need to
call `Session.Init`; call it only when you want to reset an existing session to
its defaults.

The base URL works with or without a trailing slash, and paths work with or
without a leading slash.

Post JSON with the same convenience as the stateless API:

```pascal
Response := Session.PostJSON('/profile', '{"displayName":"Ada"}');
```

See the [session guide](docs/Request.Session.md) for cookies, timeouts, and the
complete API.

## Installation

1. Copy `src/Request.pas` and, if needed, `src/Request.Session.pas` into your
   project, or clone this repository.
2. Add the `src` directory to the project's unit search path.
3. Add `Request` to your `uses` clause.

Requirements:

- Free Pascal 3.2.2+ or Lazarus 4.8+
- Windows or Linux
- OpenSSL libraries for HTTPS

On Linux, install the distribution's OpenSSL development package. On Windows,
the OpenSSL DLL architecture must match the executable architecture. If HTTPS
setup fails, run the `examples/ssl_debug` project and follow the
[SSL/HTTPS guide](docs/SSL-HTTPS-GUIDE.md).

## API at a glance

```pascal
// Requests
Http.Get(URL)
Http.GetWithParams(URL, Params)
Http.Post(URL, Body)
Http.PostForm(URL, Fields)
Http.PostJSON(URL, JSON)
Http.Put(URL, Body)
Http.Delete(URL)
Http.PostMultipart(URL, Fields, Files)

// Response
Response.StatusCode
Response.Text
Response.JSON
Response.OK
Response.HeaderValue('Content-Type')
Response.RaiseForStatus
Response.SaveToFile('output.dat')
```

Most methods also have header/query-parameter overloads and exception-free
`Try*` counterparts. See the [API reference](docs/Request.md) for exact
signatures.

## Documentation and examples

- [Cheat sheet](docs/cheat-sheet.md)
- [Stateless API reference](docs/Request.md)
- [Session guide](docs/Request.Session.md)
- [SSL/HTTPS guide](docs/SSL-HTTPS-GUIDE.md)
- [Technical details](docs/TECHNICAL-DETAILS.md)
- [Examples](examples/)

## Testing

Build the suite with Lazarus or:

```bash
lazbuild --build-mode=Release tests/TestRunner.lpi
tests/TestRunner.exe -a --format=plain
```

On Linux, run `tests/TestRunner`. The integration tests use
`https://httpbin.org` and therefore require network access.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

Request-FP is available under the [MIT License](LICENSE.md).
