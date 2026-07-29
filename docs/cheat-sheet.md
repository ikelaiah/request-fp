# Request-FP cheat sheet

For complete signatures and behavior, see the [stateless API reference](Request.md)
and [session guide](Request.Session.md).

## Setup

```pascal
uses Request, fpjson; // fpjson is needed only when constructing JSON values

var
  Response: TResponse;
  Result: TRequestResult;
```

## Requests

```pascal
// GET
Response := Http.Get('https://api.example.com/users');

// Query parameters (UTF-8 percent-encoded automatically)
Response := Http.GetWithParams('https://api.example.com/search', [
  KV('q', 'free pascal'),
  KV('page', '2')
]);

// Headers plus query parameters
Response := Http.Get('https://api.example.com/search',
  [KV('Authorization', 'Bearer ' + Token)],
  [KV('q', 'free pascal')]);

// Raw or already-encoded body
Response := Http.Post('https://api.example.com/events', Body);
Response := Http.Put('https://api.example.com/users/42', Body);

// Automatically encoded form
Response := Http.PostForm('https://api.example.com/login', [
  KV('email', 'ada@example.com'),
  KV('password', Password)
]);

// JSON string or an existing TJSONData value
Response := Http.PostJSON('https://api.example.com/users',
  '{"name":"Ada"}');
// The caller retains ownership of JsonObject.
Response := Http.PostJSON('https://api.example.com/users', JsonObject);

// DELETE
Response := Http.Delete('https://api.example.com/users/42');

// Multipart fields and files
Response := Http.PostMultipart('https://api.example.com/upload',
  [KV('description', 'avatar')],
  [KV('file', 'avatar.png')]);
```

`KV('name', 'value')` is shorthand for
`TKeyValue.Create('name', 'value')`.

## Responses

```pascal
WriteLn(Response.StatusCode);
WriteLn(Response.Text);
WriteLn(Response.HeaderValue('Content-Type'));

if Response.OK then
  WriteLn('2xx response');

Response.RaiseForStatus;       // raises for non-2xx
Response.SaveToFile('out.dat');

// Parsed JSON is owned by Response; do not free it.
WriteLn(Response.JSON.FindPath('user.name').AsString);
```

## Exception-free requests

Prefix the request name with `Try`:

```pascal
Result := Http.TryGetWithParams('https://api.example.com/search', [
  KV('q', 'free pascal')
]);

if Result.OK then
  WriteLn(Result.Response.Text)
else if not Result.Success then
  WriteLn('Transport error: ', Result.Error)
else
  WriteLn('HTTP status: ', Result.Response.StatusCode);
```

- `Result.Success`: the HTTP exchange completed, including 4xx/5xx responses.
- `Result.OK`: the exchange completed and the response status is 2xx.

Available variants:

```pascal
TryGet
TryGetWithParams
TryPost
TryPostForm
TryPostJSON
TryPut
TryDelete
TryPostMultipart
```

## Sessions

```pascal
uses Request, Request.Session;

var
  Session: THttpSession;
begin
  // No Init or Free needed.
  Session.SetBaseURL('https://api.example.com');
  Session.SetHeader('Authorization', 'Bearer ' + Token);
  Session.SetCookie('session_id', 'abc123');
  Session.SetTimeout(10000); // milliseconds

  Response := Session.Get('/profile');
  Response := Session.PostJSON('/profile', '{"displayName":"Ada"}');

  Session.ClearCookies;
  Session.ClearHeaders;
  Session.Init; // optional: reset the whole session
end;
```

## Error policy

```pascal
try
  Response := Http.Get('https://api.example.com/data');
  Response.RaiseForStatus;
except
  on E: ERequestError do
    WriteLn(E.Message);
end;
```

Normal stateless methods raise for request/transport failures and return HTTP
responses of any status. `RaiseForStatus` opts in to raising for non-2xx.
`Try*` methods capture request exceptions in `TRequestResult`.
