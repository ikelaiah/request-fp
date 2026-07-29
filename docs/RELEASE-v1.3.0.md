# Request-FP v1.3.0 release notes

**Release date:** July 29, 2026

v1.3.0 is the ease-of-use release. Its goal is that a new user can complete
the common HTTP tasks from the README without learning Request-FP's internal
types or writing encoding boilerplate.

## The v1.3.0 usability target

A common task should have:

- one obvious method name;
- no manual request or response cleanup;
- no empty placeholder arguments;
- correct encoding and content type by default;
- a clear choice between exceptions and result-based handling; and
- a copyable example near the top of the documentation.

## Highlights

### Short key/value syntax

```pascal
Response := Http.Get('https://api.example.com/search',
  [KV('Authorization', 'Bearer ' + Token)],
  [KV('q', 'free pascal')]);
```

`KV` replaces repeated `TKeyValue.Create` calls while remaining fully
compatible with them.

### Query-only GET requests

```pascal
Response := Http.GetWithParams('https://api.example.com/search', [
  KV('q', 'free pascal')
]);
```

There is no longer a need to pass `[]` for unused headers.

### Automatically encoded forms

```pascal
Response := Http.PostForm('https://api.example.com/login', [
  KV('email', 'ada@example.com'),
  KV('password', Password)
]);
```

`PostForm` encodes names and values over UTF-8 and sets the expected content
type.

### Direct JSON values

```pascal
Response := Http.PostJSON('https://api.example.com/users', JsonObject);
Response := Session.PostJSON('/users', JsonObject);
```

Both APIs accept `TJSONData` directly. The caller keeps ownership of the input
value.

### One-line success checks

```pascal
if Response.OK then
  WriteLn(Response.Text);

Response.RaiseForStatus;
```

For exception-free calls:

```pascal
Result := Http.TryGet(URL);
if Result.OK then
  WriteLn(Result.Response.Text);
```

`Result.Success` continues to mean that the HTTP exchange completed.
`Result.OK` additionally requires a 2xx status.

### Sessions with no setup ceremony

```pascal
var
  Session: THttpSession;
begin
  Session.SetBaseURL('https://api.example.com');
  Response := Session.Get('/profile');
end;
```

Session initialization and cleanup are automatic. `Session.Init` remains as an
optional reset method. Base URL/path joining now handles slash combinations
automatically.

### Build every example with one command

Windows PowerShell:

```powershell
.\build-examples.ps1
```

Linux or Git Bash:

```bash
bash ./build-examples.sh
```

Both scripts discover the Lazarus projects under `examples/`, skip backup
directories, clean the generated `example-bin/` directory, and compile all
examples in Release mode. `lazbuild` must be available on `PATH`.

### OpenSSL 3 on Windows with FPC 3.2.2

FPC 3.2.2's OpenSSL bindings support OpenSSL 3, but its Windows loader does
not try the standard OpenSSL 3 DLL filenames. Request-FP now selects
`libssl-3-x64.dll` and `libcrypto-3-x64.dll` first for 64-bit applications
(or the equivalent names without `-x64` for 32-bit applications).

FPC's existing OpenSSL 1.1 candidates remain as a compatibility fallback.
Users do not need to patch FPC or rename system DLLs. Request-FP also checks
the result of `InitSSLInterface`, so a failed load is no longer recorded as a
successful initialization.

See the [SSL/HTTPS guide](SSL-HTTPS-GUIDE.md) for setup and the
[OpenSSL version selection guide](OPENSSL-VERSION-SELECTION.md) for the
technical background.

## Documentation

The README, stateless API reference, session guide, cheat sheet, examples, and
SSL session example have been updated for the v1.3.0 API. The examples and
contributor guides now include the cross-platform bulk-build workflow.

The README now leads with everyday operations and links to detailed SSL
diagnostics instead of placing setup troubleshooting in the main learning
path. The dedicated SSL documents now explain Request-FP's automatic FPC
3.2.2/OpenSSL 3 compatibility handling and explicitly avoid unsafe
`System32` modifications.

## Compatibility

v1.3.0 is backward compatible with v1.2.0:

- existing `TKeyValue.Create` calls still work;
- all existing request overloads remain;
- existing `Session.Init` calls remain valid; and
- Windows OpenSSL 1.1 remains available as a fallback while OpenSSL 3 is
  preferred; and
- the error meaning of `TRequestResult.Success` is unchanged.

## Upgrade

Replace the source units with the v1.3.0 versions. No code migration is
required. Existing code can adopt the new helpers incrementally.

See [CHANGELOG.md](../CHANGELOG.md#130---2026-07-29) for the complete change list.
