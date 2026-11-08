# Request-FP v1.4.0 release notes

**Release date:** November 9, 2026

Feature release. No API removals.

## New

```pascal
Response := Http.Patch(URL, '{"active":false}', [KV('Content-Type', 'application/json')]);
Response := Http.Head(URL);
Http.SetTimeout(10000);

Data := Response.Bytes;
Response.SaveToStream(Stream);

Response := Session.PostForm('/login', [KV('user', 'ada'), KV('password', Pw)]);
Response := Session.PostMultipart('/upload', [], [KV('file', 'avatar.png')]);
Response := Session.Get('/report', [KV('Accept', 'text/csv')]);
Response := Session.Send('DELETE', '/items/1', '', '', [KV('If-Match', ETag)], []);
Result := Session.TryGet('/health');
```

- **Typed errors:** `ETimeoutError` < `ENetworkError` < `ERequestError`.
- **Sessions** get `Patch`, `Head`, `PostForm`, `PostMultipart`,
  per-request headers and params, and `Try*` variants.

## Fixes

- **Login flows:** cookies set on a redirect response are kept.
- **Cookie expiry:** `Max-Age=0` or a past `Expires` removes the cookie.
- **Redirect safety:** session cookies are not sent to a different host.
- **`ClearCookies`** no longer lets FPC resend the last response's cookies.
- **Multipart:** per-file content type, escaped names, random boundary;
  a missing file raises `ERequestError`.
- **`PostJSON`/`PostMultipart`** respect a `Content-Type` you pass.

## Behavior changes

- Stateless requests now time out after 30 seconds of I/O inactivity
  (previously never). `Http.SetTimeout(0)` restores the old behavior.
- Session transport error messages now start with `Network Error:`.

## Known limits

- Multipart files are read into memory before sending.
- Cookie `Domain` and `Path` attributes are ignored within a session.
- Timeouts during reads are detected by elapsed time, because FPC reports
  them as a generic socket read error.
