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

### PUT and DELETE

```pascal
Http.Put(URL, Body)
Http.Put(URL, Body, Headers)
Http.Put(URL, Body, Headers, Params)

Http.Delete(URL)
Http.Delete(URL, Headers)
Http.Delete(URL, Headers, Params)
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

Http.TryDelete(URL)
Http.TryDelete(URL, Headers)
Http.TryDelete(URL, Headers, Params)

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
function IsSuccessStatus: Boolean;
procedure RaiseForStatus;
procedure SaveToFile(const FilePath: string);
```

- `Text` is the response body decoded as UTF-8.
- `JSON` lazily parses `Text`. It raises `ERequestError` with a
  `JSON Parse Error` prefix when the body is invalid.
- `OK` and `IsSuccessStatus` are true for HTTP status 200 through 299.
- `HeaderValue` performs a case-insensitive response-header lookup and returns
  an empty string when the header is absent.
- `RaiseForStatus` raises `ERequestError` for a status outside 200..299.
- `SaveToFile` saves the body as its UTF-8 bytes.

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

- Network, TLS, timeout, and request failures raise `ERequestError`.
- A server response such as 404 or 500 is returned as `TResponse`.
- Call `Response.RaiseForStatus` if your application wants non-2xx responses
  to raise.
- Use a `Try*` method when request exceptions should be captured in a result.

## HTTPS

Request-FP uses FPC's OpenSSL support. If HTTPS setup fails, run the
`examples/ssl_debug` project and follow the
[SSL/HTTPS guide](SSL-HTTPS-GUIDE.md).
