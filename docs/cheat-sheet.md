# 📋 Request-FP Cheat Sheet

Quick reference for Request-FP features and usage examples.
 
## Table of Contents

- [📋 Cheat Sheet](#-cheat-sheet)
  - [Table of Contents](#table-of-contents)
  - [🌐 HTTP Request Operations](#-http-request-operations)
  - [🔗 HTTP Session Operations](#-http-session-operations)


## 🌐 HTTP Request Operations

```pascal
// Simple requests
Response := Http.Get('https://api.example.com/data');         // GET request
Response := Http.Post('https://api.example.com/data', 'text'); // POST with data
Response := Http.Put('https://api.example.com/data', 'text');  // PUT with data
Response := Http.Delete('https://api.example.com/data');       // DELETE request

// With custom headers and query parameters
Response := Http.Get('https://api.example.com/data',
  [TKeyValue.Create('X-Api-Key', 'my-key')],
  [TKeyValue.Create('q', 'search')]);

// POST with custom headers and params
Response := Http.Post('https://api.example.com/data', 'foo=bar',
  [TKeyValue.Create('Authorization', 'Bearer token')],
  [TKeyValue.Create('debug', '1')]);

// Status code and response handling
if Response.StatusCode = 200 then                             // Check status code
  WriteLn(Response.Text);                                     // Get response text
if Response.StatusCode = 200 then                             // Check status code
begin
  JsonObj := TJSONObject(Response.JSON);                       // Get JSON object
  try
    WriteLn('Name: ', JsonObj.Get('name', ''));
  finally
    // Do NOT free JsonObj - it's owned by Response.JSON
  end;
end;

// Error handling with try-except
try
  Response := Http.Get('https://api.example.com/secure');     // Request might fail
except
  on E: ERequestError do 
    WriteLn('HTTP Error: ', E.Message);                       // Handle HTTP errors
end;

// Error handling with Try-pattern (recommended)
Result := Http.TryGet('https://api.example.com/secure');      // Won't throw exceptions
if Result.Success then
  WriteLn('Status: ', Result.Response.StatusCode)             // Access successful response
else
  WriteLn('Error: ', Result.Error);                           // Get error message

// Try-pattern for multipart upload
var R: TRequestResult;
R := Http.TryPostMultipart('https://api.example.com/upload',
  [TKeyValue.Create('field1', 'value1')],
  [TKeyValue.Create('file1', 'myfile.txt')]);
if R.Success and (R.Response.StatusCode = 200) then
  WriteLn('Upload OK')
else
  WriteLn('Upload failed: ', R.Error);

// Behavior summary
// - Http.Get/Post/... may raise ERequestError on transport errors
// - Try* methods (TryGet/TryPost/TryPut/TryDelete/TryPostMultipart) never raise; inspect Result fields
// - Response.JSON raises ERequestError if body is not valid JSON

// Reading a response header
var CT: string;
CT := Response.HeaderValue("Content-Type");
if Pos('application/json', LowerCase(CT)) > 0 then
  WriteLn('Looks like JSON');

// Multipart upload
Response := Http.PostMultipart('https://api.example.com/upload',
  [TKeyValue.Create('field1', 'value1')],
  [TKeyValue.Create('file1', 'myfile.txt')]);

// Quick success check and save body to a file
if Response.IsSuccessStatus then
  Response.SaveToFile('output.txt');
```

## 🔗 HTTP Session Operations

```pascal
var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  Session.SetHeader('X-Token', 'abc123');
  Session.SetCookie('mycookie', 'cookievalue');
  Response := Session.Get('/data');
  // HeaderValue works for session responses too
  WriteLn('Content-Type: ', Response.HeaderValue('Content-Type'));
  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);
end;
```
