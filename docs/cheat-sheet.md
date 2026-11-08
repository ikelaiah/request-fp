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

// PATCH, DELETE, HEAD
Response := Http.Patch('https://api.example.com/users/42', Body);
Response := Http.Delete('https://api.example.com/users/42');
Response := Http.Head('https://api.example.com/files/report.pdf');

// I/O timeout for all stateless requests (milliseconds, default 30000)
Http.SetTimeout(10000);

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
Response.SaveToFile('out.dat'); // exact bytes, binary-safe
Bytes := Response.Bytes;        // TBytes copy of the body

// Parsed JSON is owned by Response; do not free it.
WriteLn(Response.JSON.FindPath('user.name').AsString);
```

## Auth, headers, and proxy

```pascal
Http.Get(URL, [KVBearer(Token)]);          // or KVBasic(User, Pass)
Http.PutJSON(URL, '{"a":1}');              // also PatchJSON
Values := Response.HeaderValues('Set-Cookie'); // every value
Names := Response.HeaderNames;
Http.SetConnectTimeout(5000);
Http.SetProxy('proxy.local', 3128);        // http:// only; ('', 0) removes
```

## Downloads

```pascal
procedure ShowProgress(Received, Total: Int64);  // Total = -1 if unknown
begin
  WriteLn(Received, ' / ', Total);
end;

Response := Http.Download(URL, 'big.zip');                // streamed to disk
Response := Http.Download(URL, 'big.zip', @ShowProgress);
Response := Http.GetToStream(URL, MyStream);
if not Response.OK then
  WriteLn(Response.Text);  // error body; the file is left untouched
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
TryPatch
TryDelete
TryHead
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
  Session.SetBearerToken(Token);      // or SetBasicAuth(User, Pass)
  Session.SetCookie('session_id', 'abc123');
  Session.SetTimeout(10000);          // milliseconds
  Session.SetConnectTimeout(5000);
  Session.SetProxy('proxy.local', 3128); // http:// only

  Response := Session.Get('/profile');
  Response := Session.Get('/report', [KV('Accept', 'text/csv')]);
  Response := Session.PostJSON('/profile', '{"displayName":"Ada"}');
  Response := Session.PostForm('/login', [KV('user', 'ada')]);
  Response := Session.PatchJSON('/profile', '{"theme":"dark"}');
  Response := Session.Send('GET', '/search', '', '', [], [KV('q', 'fp')]);
  Result := Session.TryGet('/health');

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

Normal stateless and session methods raise for request/transport failures and
return HTTP responses of any status. `RaiseForStatus` opts in to raising for
non-2xx. `Try*` methods capture request exceptions in `TRequestResult`.

## Retries

```pascal
// On by default: GET/HEAD/PUT/DELETE, network errors, 429, 503
Http.SetRetry(2, 250);    // default: 2 retries, 250 ms then 500 ms
Http.SetRetry(0, 0);      // disable
Session.SetRetry(0, 0);   // per session
```

```pascal
except
  on E: ETimeoutError do ...  // connect or I/O timeout
  on E: ENetworkError do ...  // other transport failure
  on E: ERequestError do ...  // anything else from Request-FP
end;
```
