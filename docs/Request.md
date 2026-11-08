# Request API reference

This unit provides the stateless Request-FP API. Add `Request` to the `uses`
clause and call methods on the global `Http` value.

For a task-oriented overview, start with the [README](../README.md). For shared
cookies and configuration, see the [session guide](Request.Session.md).

## First request

```pascal
uses Request;

var
  Response: TResponse;
begin
  Response := Http.Get('https://api.example.com/data');
  Response.RaiseForStatus;
  WriteLn(Response.Text);
end.
```

No request or response objects need to be freed.

## Key/value pairs

Headers, query parameters, forms, and multipart requests use `TKeyValue`.
`KV` is the concise constructor:

```pascal
KV('Authorization', 'Bearer ' + Token)
```

It is equivalent to:

```pascal
TKeyValue.Create('Authorization', 'Bearer ' + Token)
```

## THttp

### GET

```pascal
Http.Get(URL)
Http.Get(URL, Headers)
Http.Get(URL, Headers, Params)
Http.GetWithParams(URL, Params)
```

Examples:

```pascal
Response := Http.Get('https://api.example.com/users');

Response := Http.GetWithParams('https://api.example.com/search', [
  KV('q', 'free pascal'),
  KV('page', '2')
]);

Response := Http.Get('https://api.example.com/search',
  [KV('Authorization', 'Bearer ' + Token)],
  [KV('q', 'free pascal')]);
```

### POST

Use `Post` for a body that is already encoded:

```pascal
Http.Post(URL, Body)
Http.Post(URL, Body, Headers)
Http.Post(URL, Body, Headers, Params)
```

When `Body` is not empty and no content type was supplied, `Post` uses
`application/x-www-form-urlencoded`.

### Form POST

Use `PostForm` to let Request-FP encode form fields:

```pascal
Http.PostForm(URL, Fields)
Http.PostForm(URL, Fields, Headers)
Http.PostForm(URL, Fields, Headers, Params)
```

```pascal
Response := Http.PostForm('https://api.example.com/login', [
  KV('email', 'ada@example.com'),
  KV('password', Password)
]);
```

Names and values are percent-encoded as UTF-8.

### JSON POST

`PostJSON` accepts either a JSON string or `TJSONData` and sets
`Content-Type: application/json`.

```pascal
Http.PostJSON(URL, JSON)
Http.PostJSON(URL, JSON, Headers)
Http.PostJSON(URL, JSON, Headers, Params)
```

```pascal
Response := Http.PostJSON(
  'https://api.example.com/users',
  '{"name":"Ada"}'
);
```

Passing a JSON object does not transfer ownership. The caller remains
responsible for freeing it:

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

Passing `nil` in place of `TJSONData` raises `ERequestError`.

### PUT, PATCH, DELETE, and HEAD

```pascal
Http.Put(URL, Body)
Http.Put(URL, Body, Headers)
Http.Put(URL, Body, Headers, Params)

Http.Patch(URL, Body)
Http.Patch(URL, Body, Headers)
Http.Patch(URL, Body, Headers, Params)

Http.Delete(URL)
Http.Delete(URL, Headers)
Http.Delete(URL, Headers, Params)

Http.Head(URL)
Http.Head(URL, Headers)
Http.Head(URL, Headers, Params)
```

- `Put` and `Patch` send `application/x-www-form-urlencoded` for a non-empty
  body unless you pass a `Content-Type` header.
- `Head` returns status and headers; `Text` is empty.

For JSON, use `PutJSON` and `PatchJSON` (string or `TJSONData`, optional
`Headers`, plus `TryPutJSON` / `TryPatchJSON`):

```pascal
Http.PutJSON(URL, '{"name":"Ada"}');
Http.PatchJSON(URL, Body);  // TJSONData; you keep ownership
```

### Authentication

```pascal
Http.Get(URL, [KVBearer(Token)]);
Http.Get(URL, [KVBasic('user', 'secret')]);
```

`Authorization` is dropped when a redirect goes to a different host, so
credentials are not sent to another server.

### Proxy

```pascal
Http.SetProxy('proxy.local', 3128);                   // all Http requests
Http.SetProxy('proxy.local', 3128, 'user', 'secret'); // with proxy auth
Http.SetProxy('', 0);                                 // remove
```

The proxy is used for `http://` URLs only. FPC 3.2.2 cannot tunnel HTTPS
through a proxy, so an `https://` request raises `ERequestError` while a proxy
is set.

### Timeout

```pascal
Http.SetTimeout(10000); // milliseconds, all stateless requests
WriteLn(Http.GetTimeout);
```

The default is 30000. `0` waits forever. It is the I/O timeout.

The connect timeout is separate (default 3000 ms; `0` uses the system
default). Both raise `ETimeoutError`:

```pascal
Http.SetConnectTimeout(5000);
WriteLn(Http.GetConnectTimeout);
```

### Multipart POST

```pascal
Http.PostMultipart(URL, Fields, Files)
Http.PostMultipart(URL, Fields, Files, Headers)
Http.PostMultipart(URL, Fields, Files, Headers, Params)
```

Each item in `Files` maps a multipart field name to a local file path:

```pascal
Response := Http.PostMultipart('https://api.example.com/upload',
  [KV('description', 'profile photo')],
  [KV('file', 'avatar.png')]);
```

- Each file's `Content-Type` comes from its extension (for example
  `image/png`, `application/pdf`); unknown extensions use
  `application/octet-stream`.
- Quotes and line breaks in field names and filenames are percent-encoded.
- A file that cannot be opened raises `ERequestError`.
- Files are streamed from disk while sending, so large uploads use little
  memory. They are opened before the request starts and closed when it ends.

### Downloads and streaming

`Download` and `GetToStream` write the body as it arrives, so large files
never sit in memory:

```pascal
Http.Download(URL, FilePath)
Http.Download(URL, FilePath, OnProgress)
Http.Download(URL, FilePath, Headers, Params)
Http.Download(URL, FilePath, Headers, Params, OnProgress)

Http.GetToStream(URL, Stream)                 // same four forms
```

```pascal
procedure ShowProgress(Received, Total: Int64);
begin
  if Total > 0 then
    Write(#13, Received * 100 div Total, '%');
end;

Response := Http.Download('https://example.com/big.zip', 'big.zip',
  @ShowProgress);
if not Response.OK then
  WriteLn('Failed: ', Response.StatusCode, ' ', Response.Text);
```

- Only a 2xx body is saved. It goes to `FilePath + '.part'` and replaces
  `FilePath` once complete, so an existing file is never left half-written.
- Any other status leaves `FilePath` (or `Stream`) untouched and puts the
  error body in `Response.Text`. On a 2xx, `Response.Text` is empty.
- `OnProgress` is a plain procedure or a method (`of object`):
  `procedure(Received, Total: Int64)`. `Total` is `-1` when the server sends
  no `Content-Length`.
- Raise an exception in `OnProgress` to cancel; the partial file is deleted.
- A body shorter than `Content-Length` raises `ENetworkError`.
- `GetToStream` writes at the stream's current position. If a transfer fails,
  bytes already written stay in the stream.
- `TryDownload` and `TryGetToStream` take the same arguments and return
  `TRequestResult`.

## Exception-free methods

The `Try*` methods return `TRequestResult` rather than allowing request
exceptions to escape:

```pascal
Http.TryGet(URL)
Http.TryGet(URL, Headers)
Http.TryGet(URL, Headers, Params)
Http.TryGetWithParams(URL, Params)

Http.TryPost(URL, Body)
Http.TryPost(URL, Body, Headers)
Http.TryPost(URL, Body, Headers, Params)

Http.TryPostForm(URL, Fields)
Http.TryPostForm(URL, Fields, Headers)
Http.TryPostForm(URL, Fields, Headers, Params)

Http.TryPostJSON(URL, JSON)
Http.TryPostJSON(URL, JSON, Headers)
Http.TryPostJSON(URL, JSON, Headers, Params)

Http.TryPut(URL, Body)
Http.TryPut(URL, Body, Headers)
Http.TryPut(URL, Body, Headers, Params)

Http.TryPatch(URL, Body)
Http.TryPatch(URL, Body, Headers)
Http.TryPatch(URL, Body, Headers, Params)

Http.TryDelete(URL)
Http.TryDelete(URL, Headers)
Http.TryDelete(URL, Headers, Params)

Http.TryHead(URL)
Http.TryHead(URL, Headers)
Http.TryHead(URL, Headers, Params)

Http.TryPostMultipart(URL, Fields, Files)
Http.TryPostMultipart(URL, Fields, Files, Headers)
Http.TryPostMultipart(URL, Fields, Files, Headers, Params)
```

`TryPostJSON` accepts both JSON strings and `TJSONData`.

Example:

```pascal
Result := Http.TryGet('https://api.example.com/users/42');
if Result.OK then
  WriteLn(Result.Response.Text)
else if not Result.Success then
  WriteLn(Result.Error)
else
  WriteLn('HTTP status: ', Result.Response.StatusCode);
```

`Success` and `OK` answer different questions:

- `Result.Success` is true when the HTTP exchange completed. A 404 response is
  still a completed exchange.
- `Result.OK` is true when the exchange completed and the response status is
  in the 200..299 range.

## TResponse

### Members

```pascal
StatusCode: Integer
Text: string
JSON: TJSONData
OK: Boolean

function HeaderValue(const Name: string): string;
function HeaderValues(const Name: string): TStringArray;
function HeaderNames: TStringArray;
function IsSuccessStatus: Boolean;
procedure RaiseForStatus;
procedure SaveToFile(const FilePath: string);
procedure SaveToStream(Stream: TStream);
function Bytes: TBytes;
```

- `Text` is the response body decoded as UTF-8.
- `JSON` lazily parses `Text`. An empty body returns `nil`; a non-empty invalid
  body raises `ERequestError` with a `JSON Parse Error` prefix.
- `OK` and `IsSuccessStatus` are true for HTTP status 200 through 299.
- `HeaderValue` performs a case-insensitive response-header lookup and returns
  an empty string when the header is absent (first value if repeated).
- `HeaderValues` returns every value of a repeated header such as
  `Set-Cookie`; `HeaderNames` lists header names once each, in order.
- `RaiseForStatus` raises `ERequestError` for a status outside 200..299.
- `SaveToFile`, `SaveToStream`, and `Bytes` give the body bytes exactly as
  received, so they are safe for binary downloads.

The response owns the value returned by `JSON`. Do not free that value or a
child obtained with `FindPath`.

## TRequestResult

```pascal
Success: Boolean
Response: TResponse
Error: string
OK: Boolean
```

When `Success` is false, `Error` contains the caught exception message and
`Response.StatusCode` is zero.

## Error policy

Stateless methods separate transport success from HTTP status:

- Network, TLS, timeout, and request failures raise `ERequestError`:
  - `ETimeoutError` for a connect timeout or an I/O timeout;
  - `ENetworkError` (parent of `ETimeoutError`) for other connection, DNS,
    socket, or TLS failures;
  - plain `ERequestError` for other request errors.
- A server response such as 404 or 500 is returned as `TResponse`.
- Call `Response.RaiseForStatus` if your application wants non-2xx responses
  to raise.
- Use a `Try*` method when request exceptions should be captured in a result.
- A connection closed without any response raises `ENetworkError`.

## Retries

Requests are retried automatically, so short outages and rate limits often
go unnoticed:

- **Methods:** GET, HEAD, PUT, and DELETE only. POST and PATCH are never
  retried, because repeating them can duplicate an action.
- **When:** network errors (not timeouts), `429 Too Many Requests`, and
  `503 Service Unavailable`.
- **How often:** 2 retries; waits 250 ms, then 500 ms.
- **Retry-After:** honoured, in seconds or as a date. If it is longer than
  the timeout, the 429/503 response is returned without waiting.
- **Downloads:** not retried once body bytes have been written.
- After the last retry, the final response is returned (or the error raised).

```pascal
Http.SetRetry(5, 100);  // 5 retries: 100, 200, 400, 800, 1600 ms
Http.SetRetry(0, 0);    // disable
WriteLn(Http.GetRetryCount, ' retries, first delay ', Http.GetRetryDelay, ' ms');
```

Sessions have their own `Session.SetRetry` with the same defaults.

## HTTPS

Request-FP uses FPC's OpenSSL support. On Windows, it automatically works
around FPC 3.2.2's DLL-name issue by selecting the standard OpenSSL 3 names
first while retaining FPC's OpenSSL 1.1 fallback. The installed DLL
architecture must still match the executable.

OpenSSL loads on the first HTTPS request, so HTTP-only programs run without
it. Call `EnsureSSL` at startup to fail fast instead; it raises
`ERequestError` with setup hints if OpenSSL cannot be loaded.

If HTTPS setup fails, run `examples/ssl_debug` and follow the
[SSL/HTTPS guide](SSL-HTTPS-GUIDE.md). For the exact Windows loading order,
see [OpenSSL version selection](OPENSSL-VERSION-SELECTION.md).
