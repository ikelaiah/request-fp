# Session API guide

`Request.Session` provides a stateful HTTP client for calls that share a base
URL, headers, cookies, or a connection.

Use the stateless `Http` API for one-off calls. Use `THttpSession` for a login
flow or a group of calls to the same service.

## Quick start

```pascal
uses SysUtils, Request, Request.Session;

var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.SetBaseURL('https://api.example.com');
  Session.SetHeader('Authorization', 'Bearer ' + Token);

  Response := Session.Get('/profile');
  Response.RaiseForStatus;
  WriteLn(Response.Text);
end.
```

`THttpSession` is an advanced record. It initializes and cleans itself up
automatically, so there is no required constructor, `Init`, `Free`, or
`try..finally` block.

`Session.Init` is retained for compatibility and now means “reset this session
to its defaults.” It is optional for a newly declared session.

## Base URLs

Set a base URL once and pass relative paths:

```pascal
Session.SetBaseURL('https://api.example.com/v1');
Response := Session.Get('/users');
```

Slash handling is automatic. These combinations all produce the same URL:

```pascal
Session.SetBaseURL('https://api.example.com/v1');
Session.Get('users');

Session.SetBaseURL('https://api.example.com/v1/');
Session.Get('/users');
```

An absolute URL passed to a request method bypasses the base URL.

## Headers

Headers set on a session are sent on every request:

```pascal
Session.SetHeader('Authorization', 'Bearer ' + Token);
Session.SetHeader('Accept-Language', 'en-AU');
Response := Session.Get('/profile');
```

`ClearHeaders` removes custom headers and restores the default
`Accept: application/json` header.

The session uses `Request-FP/<version>` as its default user agent. Override it
with:

```pascal
Session.SetUserAgent('MyApp/1.0');
```

## JSON

Post a JSON string:

```pascal
Response := Session.PostJSON('/users', '{"name":"Ada"}');
```

Or pass a `TJSONData` value:

```pascal
uses Request, Request.Session, fpjson;

var
  Body: TJSONObject;
begin
  Body := TJSONObject.Create;
  try
    Body.Add('name', 'Ada');
    Response := Session.PostJSON('/users', Body);
  finally
    Body.Free;
  end;
end;
```

The caller retains ownership of a JSON value passed to `PostJSON`.

For other content types, use `Post` or `Put` directly:

```pascal
Response := Session.Post('/submit', 'name=Ada',
  'application/x-www-form-urlencoded');
```

## Cookies

Cookies received in `Set-Cookie` headers are stored and sent by later calls.
You can also manage them directly:

```pascal
Session.SetCookie('session_id', 'abc123');
WriteLn(Session.GetCookie('session_id'));
Session.ClearCookies;
```

## Timeouts

The default timeout is 30 seconds. Values are milliseconds:

```pascal
Session.SetTimeout(10000);
```

The new timeout applies to the current connection and later calls.

## Reading a response

Session methods return the same `TResponse` as the stateless API:

```pascal
if Response.OK then
begin
  WriteLn(Response.Text);
  WriteLn(Response.HeaderValue('Content-Type'));
end;
```

The response owns its parsed `Response.JSON` value; do not free it manually.
See the [Request API reference](Request.md#tresponse) for all response helpers.

## Error handling

Session methods raise exceptions when the underlying request fails. `Get`,
`Put`, and `Delete` normalize those failures to `ERequestError`; `Post` and
`PostJSON` currently allow the underlying FPC exception type to propagate.
Catch `Exception` when one handler must cover every session method:

```pascal
try
  Response := Session.Get('/profile');
except
  on E: Exception do
    WriteLn('Request failed: ', E.Message);
end;
```

The session API follows the accepted-status behavior of its underlying FPC
calls. `Post` and `Put` accept 200, 201, and 204; `Delete` accepts 200 and 204;
`Get` uses the FPC client's default accepted status. Other response statuses
may raise an exception. If you need the stateless API's transport/status
separation or an exception-free result, use `Http.Try*`.

## API reference

```pascal
THttpSession = record
  // Optional reset
  procedure Init;

  // Requests
  function Get(const URL: string): TResponse;
  function Post(const URL: string; const Body: string = '';
    const ContentType: string = 'application/x-www-form-urlencoded'): TResponse;
  function PostJSON(const URL: string; const JSON: string): TResponse;
  function PostJSON(const URL: string; const JSON: TJSONData): TResponse;
  function Put(const URL: string; const Body: string = '';
    const ContentType: string = 'application/json'): TResponse;
  function Delete(const URL: string): TResponse;

  // Configuration
  procedure SetHeader(const Name, Value: string);
  procedure SetCookie(const Name, Value: string);
  procedure SetBaseURL(const URL: string);
  procedure SetUserAgent(const UserAgent: string);
  procedure SetTimeout(Timeout: Integer);

  // State
  procedure ClearCookies;
  function GetCookie(const Name: string): string;
  procedure ClearHeaders;
end;
```

`TSimpleMap` is public for compatibility but is an implementation detail for
most users.

## Multipart requests

The session API does not yet provide a multipart helper. Prefer the stateless
helper:

```pascal
Result := Http.TryPostMultipart('https://api.example.com/upload',
  [KV('description', 'avatar')],
  [KV('file', 'avatar.png')]);
```

## HTTPS

Session requests use the same FPC/OpenSSL stack and automatic Windows
OpenSSL 3 filename selection as the stateless API. No session-specific TLS
configuration is required.

See the [SSL/HTTPS guide](SSL-HTTPS-GUIDE.md) if setup fails, or
[OpenSSL version selection](OPENSSL-VERSION-SELECTION.md) for the FPC 3.2.2
compatibility details.
