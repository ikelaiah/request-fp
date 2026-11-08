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

## Per-request headers and parameters

Session headers apply to every call. Add or override headers for one call
with `Get(URL, Headers)` or the general `Send`:

```pascal
Response := Session.Get('/report', [KV('Accept', 'text/csv')]);
Response := Session.GetWithParams('/search', [KV('q', 'pascal')]);

// Method, URL, Body, ContentType, Headers, Params
Response := Session.Send('PATCH', '/users/42', '{"active":false}',
  'application/json', [KV('If-Match', ETag)], []);
```

## Forms and uploads

```pascal
Response := Session.PostForm('/login', [
  KV('email', 'ada@example.com'),
  KV('password', Password)
]);

Response := Session.PostMultipart('/upload',
  [KV('description', 'avatar')],
  [KV('file', 'avatar.png')]);
```

## Cookies

Cookies received in `Set-Cookie` headers are stored and sent by later calls:

- Cookies set on redirect responses (common after a login POST) are kept.
- A cookie with `Max-Age=0` or a past `Expires` is removed.
- Session cookies are not sent when a redirect goes to a different host.
- `Domain` and `Path` attributes are not checked: every stored cookie is sent
  to the session's own requests.

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

The new timeout applies to the current connection and later calls. A request
that exceeds it raises `ETimeoutError`.

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

Session methods behave like the stateless API:

- Any HTTP status, including 4xx and 5xx, is returned in the response.
- Network, TLS, and request failures raise `ERequestError`
  (`ENetworkError`/`ETimeoutError` for transport failures).
- Call `Response.RaiseForStatus` to raise for non-2xx.
- `Try*` methods never raise; check `Result.Success` or `Result.OK`.

```pascal
Result := Session.TryGet('/profile');
if Result.OK then
  WriteLn(Result.Response.Text)
else if Result.Success then
  WriteLn('HTTP ', Result.Response.StatusCode)
else
  WriteLn('Failed: ', Result.Error);
```

```pascal
try
  Response := Session.Get('/profile');
  Response.RaiseForStatus;
except
  on E: ERequestError do
    WriteLn('Request failed: ', E.Message);
end;
```

Before v1.3.1, session methods raised for some non-error statuses (for
example `Get` on 201 or 404) and `Post` could raise FPC exception types.

## API reference

```pascal
THttpSession = record
  // Optional reset
  procedure Init;

  // Requests
  function Get(const URL: string): TResponse;
  function Get(const URL: string; const Headers: array of TKeyValue): TResponse;
  function GetWithParams(const URL: string; const Params: array of TKeyValue): TResponse;
  function Post(const URL: string; const Body: string = '';
    const ContentType: string = 'application/x-www-form-urlencoded'): TResponse;
  function PostJSON(const URL: string; const JSON: string): TResponse;
  function PostJSON(const URL: string; const JSON: TJSONData): TResponse;
  function PostForm(const URL: string; const Fields: array of TKeyValue): TResponse;
  function PostMultipart(const URL: string; const Fields, Files: array of TKeyValue): TResponse;
  function Put(const URL: string; const Body: string = '';
    const ContentType: string = 'application/json'): TResponse;
  function Patch(const URL: string; const Body: string = '';
    const ContentType: string = 'application/json'): TResponse;
  function Delete(const URL: string): TResponse;
  function Head(const URL: string): TResponse;
  function Send(const Method, URL: string; const Body: string;
    const ContentType: string; const Headers: array of TKeyValue;
    const Params: array of TKeyValue): TResponse;

  // Exception-free: TryGet, TryPost, TryPostJSON, TryPostForm,
  // TryPostMultipart, TryPut, TryPatch, TryDelete, TryHead, TrySend
  // (same parameters, return TRequestResult)

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
