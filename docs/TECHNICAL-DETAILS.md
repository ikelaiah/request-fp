# Technical details

## Windows OpenSSL 3 loading with FPC 3.2.2

FPC 3.2.2's `openssl` unit contains OpenSSL 3-compatible entry points, but
its Windows dynamic loader predates the standard OpenSSL 3 DLL filenames. A
64-bit process normally tries legacy names and
`libssl-1_1-x64.dll`/`libcrypto-1_1-x64.dll`; it does not try
`libssl-3-x64.dll`/`libcrypto-3-x64.dll`.

Request-FP configures OpenSSL 3 as the primary Windows candidate before the
first initialization:

```pascal
{$IFDEF CPU64}
DLLSSLName := 'libssl-3-x64.dll';
DLLUtilName := 'libcrypto-3-x64.dll';
{$ELSE}
DLLSSLName := 'libssl-3.dll';
DLLUtilName := 'libcrypto-3.dll';
{$ENDIF}

if not InitSSLInterface then
  raise ERequestError.Create('Could not initialize OpenSSL library');
```

FPC's remaining OpenSSL 1.1 candidate variables are left unchanged, providing
a compatibility fallback when OpenSSL 3 is unavailable. Request-FP also
checks the Boolean result from `InitSSLInterface`; a failed load is no longer
recorded internally as a successful initialization.

This is a filename-selection workaround, not a replacement TLS
implementation. HTTPS still uses FPC's `openssl` and `opensslsockets` units.
The Windows CI job validates this path with Lazarus 4.8, FPC 3.2.2, and the
OpenSSL 3 installation supplied by the GitHub-hosted runner.

See [OpenSSL version selection](OPENSSL-VERSION-SELECTION.md) for deployment
instructions and [the SSL/HTTPS guide](SSL-HTTPS-GUIDE.md) for user-facing
troubleshooting.

## JSON memory management

### The issue: access violations in tests

We were experiencing access violations in several test methods when working with JSON responses. The root cause was related to how JSON objects were being managed in memory.

The examples below use `TJSONObject` from FPC's `fpjson` unit:

```pascal
uses Request, fpjson;
```

### Problematic Pattern

```pascal
// Problematic code - causes access violations
var
  Headers: TJSONObject;
begin
  Headers := TJSONObject(Response.JSON.FindPath('headers'));
  try
    // Use Headers...
  finally
    Headers.Free; // This was the issue!
  end;
end;
```

### The solution

#### Root cause

1. **Ownership Issue**: The `FindPath` method returns a reference to an object that is owned by the parent `Response.JSON` object.
2. **Double Free**: When we called `Headers.Free`, we were trying to free memory that would later be freed by the `TResponse` finalizer.
3. **Use After Free**: This led to access violations when the parent
   `TResponse` record later finalized the already-freed JSON value.

#### Correct pattern

```pascal
// Correct way to handle JSON objects from TResponse
var
  Headers: TJSONObject;
begin
  Headers := TJSONObject(Response.JSON.FindPath('headers'));
  try
    AssertTrue('Headers should exist', Headers <> nil);
    // Use Headers...
  finally
    // Do NOT free Headers - it's owned by Response.JSON
  end;
end;
```

#### Key points

1. **No Manual Freeing**: Never free objects obtained via `FindPath` or similar methods from `TJSONData`.
2. **Ownership**: The `TResponse` record manages the lifetime of the JSON data structure.
3. **Null Safety**: Always check if the returned object is not nil before using it.
4. **Testing**: Added more robust assertions to catch issues earlier.

### Automatic JSON cleanup

The `TResponse` record automatically manages the lifecycle of its JSON data through its `Finalize` method. Here's how it works:

```pascal
// From Request.pas
TResponse = record
private
  FContent: string;
  FHeaders: string;
  FJSON: TJSONData;  // The JSON data is stored here
  
  // ... other methods ...
  
  // This method is automatically called when the record goes out of scope
  class operator Finalize(var Response: TResponse);
end;

class operator TResponse.Finalize(var Response: TResponse);
begin
  // This ensures the JSON data is properly freed
  if Assigned(Response.FJSON) then
    FreeAndNil(Response.FJSON);
end;
```

#### Example of automatic cleanup

```pascal
procedure Example;
var
  Response: TResponse;
  UserData: TJSONObject;
begin
  // Make an API request
  Response := Http.Get('https://api.example.com/user/1');
  
  // Access the JSON data
  UserData := TJSONObject(Response.JSON.FindPath('user'));
  if Assigned(UserData) then
    WriteLn('User: ', UserData.Get('name', 'Unknown'));
    
  // No need to free anything! The Response record will automatically
  // clean up the FJSON object when it goes out of scope
end; // <-- Automatic cleanup happens here
```

### Best practices

1. **For Consumers of TResponse**:
   - Treat all JSON objects obtained from `Response.JSON` as read-only.
   - Never free these objects manually - they are owned by `TResponse`.
   - Always check for nil before accessing properties.
   - The `TResponse` record will automatically free all JSON data when it goes out of scope.

2. **For Future Development**:
   - Document ownership semantics clearly in the code.
   - Consider using interfaces or smart pointers if the ownership model becomes complex.
   - Add more detailed error messages to aid debugging.

## Testing

The FPCUnit suite covers repeated JSON access, response copying, and automatic
cleanup. Build and run the current suite using the commands in
[CONTRIBUTING.md](../CONTRIBUTING.md#build-and-run-the-tests).
