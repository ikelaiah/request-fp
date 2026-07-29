# Technical Details: JSON Memory Management in Request-FP

## The Issue: Access Violations in Tests

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

## The Solution

### Root Cause

1. **Ownership Issue**: The `FindPath` method returns a reference to an object that is owned by the parent `Response.JSON` object.
2. **Double Free**: When we called `Headers.Free`, we were trying to free memory that would later be freed by the `TResponse` finalizer.
3. **Use After Free**: This led to access violations when the parent
   `TResponse` record later finalized the already-freed JSON value.

### Correct Pattern

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

### Key Points

1. **No Manual Freeing**: Never free objects obtained via `FindPath` or similar methods from `TJSONData`.
2. **Ownership**: The `TResponse` record manages the lifetime of the JSON data structure.
3. **Null Safety**: Always check if the returned object is not nil before using it.
4. **Testing**: Added more robust assertions to catch issues earlier.

## Automatic JSON Cleanup

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

### Example of Automatic Cleanup

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

## Best Practices

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
