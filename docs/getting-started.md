# Getting started

From nothing to a working request, a session, and good error handling.

## 1. Add Request-FP to your project

1. Clone the repository, or copy `src/Request.pas` (and
   `src/Request.Session.pas` for sessions) into your project.
2. Add the `src` directory to the unit search path. In Lazarus: **Project →
   Project Options → Compiler Options → Paths → Other unit files**.
3. Add `Request` to your `uses` clause.

For HTTPS you also need OpenSSL:

| System | Install |
| --- | --- |
| Windows | Put `libssl-3-x64.dll` and `libcrypto-3-x64.dll` (same bitness as your program) next to the executable or on `PATH` |
| Debian, Ubuntu | `sudo apt-get install libssl3` |
| Fedora, RHEL | `sudo dnf install openssl-libs` |
| macOS | `brew install openssl@3` |

OpenSSL loads on the first HTTPS request, so HTTP-only programs do not need
it. If HTTPS fails, see [SSL/HTTPS setup](SSL-HTTPS-GUIDE.md).

## 2. Make your first request

```pascal
program hello_http;

{$mode objfpc}{$H+}

uses
  Request;

var
  Response: TResponse;
begin
  Response := Http.Get('https://httpbin.org/get');
  WriteLn('Status: ', Response.StatusCode);
  WriteLn(Response.Text);
end.
```

Every HTTP status comes back as a response; only transport problems raise.
Call `Response.RaiseForStatus` when a non-2xx status should raise too.

## 3. Send data

```pascal
// JSON
Response := Http.PostJSON('https://httpbin.org/post', '{"name":"Ada"}');
WriteLn(Response.JSON.FindPath('json.name').AsString);

// A form
Response := Http.PostForm('https://httpbin.org/post',
  [KV('user', 'ada'), KV('remember', 'yes')]);

// Headers and query parameters
Response := Http.Get('https://httpbin.org/get',
  [KVBearer(Token)],               // headers
  [KV('q', 'free pascal')]);       // query parameters
```

`KV('name', 'value')` is the short form used for headers, parameters, form
fields, and files.

## 4. Use a session for related calls

A session remembers the base URL, headers, and cookies:

```pascal
uses
  Request, Request.Session;

var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.SetBaseURL('https://api.example.com');
  Session.SetBearerToken(Token);

  Response := Session.PostJSON('/login', '{"user":"ada"}');
  Response := Session.Get('/profile');   // sends cookies from /login
end.
```

Like `TResponse`, a session needs no `Init` or `Free`.

## 5. Handle failures

Pick the style that suits your program. With exceptions, catch the most
specific type first:

```pascal
try
  Response := Http.Get(URL);
  Response.RaiseForStatus;
except
  on E: ETimeoutError do WriteLn('Too slow, try later');
  on E: ENetworkError do WriteLn('Network problem: ', E.Message);
  on E: ERequestError do WriteLn('Request failed: ', E.Message);
end;
```

Or without exceptions:

```pascal
Result := Http.TryGet(URL);
if Result.OK then
  WriteLn(Result.Response.Text)
else if Result.Success then
  WriteLn('Server said ', Result.Response.StatusCode)
else
  WriteLn('Could not connect: ', Result.Error);
```

GET, HEAD, PUT, and DELETE are retried automatically on network errors, 429,
and 503. See [Retries](Request.md#retries).

## Next steps

- Browse the [cheat sheet](cheat-sheet.md).
- Download large files: [Downloads and streaming](Request.md#downloads-and-streaming).
- Run the examples: `bash ./build-examples.sh`, then look in `example-bin/`.
