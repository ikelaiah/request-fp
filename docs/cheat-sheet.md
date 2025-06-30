# 📋 Cheat Sheet

A comprehensive reference of TidyKit's features and usage examples.
 
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

// Fluent interface for complex requests
var
  Request: THttpRequest;
begin
  Response := Request
    .Get                                                      // HTTP Method
    .URL('https://api.example.com/v2/data')                   // URL
    .AddHeader('X-API-Key', 'abc123')                         // Add header
    .AddHeader('Accept', 'application/json')                  // Add another header
    .AddParam('page', '1')                                    // Add query parameter
    .AddParam('limit', '10')                                  // Add another parameter
    .BasicAuth('username', 'password')                        // Add authentication
    .WithTimeout(5000)                                        // Set timeout in ms
    .Send;                                                    // Execute request
end;

// POST JSON data
var
  JsonObj: TJSONObject;
begin
  JsonObj := TJSONObject.Create;                              // Create JSON
  try
    JsonObj.Add('name', 'John');                             // Add properties
    JsonObj.Add('age', 30);
    Response := Http.PostJSON('https://api.example.com/users', // POST with JSON
      JsonObj.AsJSON);
  finally
    JsonObj.Free;
  end;
end;

// POST form data
var
  Request: THttpRequest;
begin
  Response := Request
    .Post                                                     // POST method
    .URL('https://api.example.com/form')                      // URL
    .WithData('name=John&age=30')                             // Form data
    .Send;                                                    // Execute
end;

// Multipart file uploads
var
  Request: THttpRequest;
begin
  Response := Request
    .Post
    .URL('https://api.example.com/upload')
    .AddFormField('description', 'My file upload')
    .AddFile('file', '/path/to/file.txt')
    .Send;
end;

// Session usage
var
  Session: THttpSession;
begin
  Session.Init;                                               // REQUIRED: Initialize session
  Session.SetBaseURL('https://api.example.com');
  Session.SetHeader('Authorization', 'Bearer token123');
  
  Response := Session.Get('/data');                           // GET with session
  Response := Session.Post('/submit', '{"key":"value"}', 'application/json'); // POST with session
end;

// SSL/TLS HTTPS errors
Result := Http.TryGet('https://secure.example.com');           // Try HTTPS request
if not Result.Success and
   (Pos('OpenSSL', Result.Error) > 0) then                     // Check for SSL error
begin
  // Handle SSL library missing
  WriteLn('Please install OpenSSL libraries for HTTPS support');
  // On Linux: sudo apt-get install libssl-dev
end;

// OpenSSL requirements
// Windows: Included with application
// Linux: 
//   - Ubuntu/Debian: sudo apt-get install libssl-dev
//   - Fedora/RHEL: sudo dnf install openssl-devel
```

## 🔗 HTTP Session Operations

```pascal
// Basic session setup
var
  Session: THttpSession;
begin
  Session.Init;                                               // REQUIRED: Must call before use
  Session.SetBaseURL('https://api.example.com');             // Set base URL for all requests
  Session.SetUserAgent('MyApp/1.0');                         // Set custom user agent
  Session.SetTimeout(10000);                                 // Set timeout (10 seconds)
end;

// Session with authentication headers
var
  Session: THttpSession;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  Session.SetHeader('Authorization', 'Bearer your-token');    // Auth header for all requests
  Session.SetHeader('X-API-Key', 'your-api-key');           // Custom headers
end;

// Session HTTP methods
var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  
  Response := Session.Get('/users');                          // GET request
  Response := Session.Post('/users', '{"name":"John"}');      // POST with form data
  Response := Session.Post('/users', '{"name":"John"}', 'application/json'); // POST with JSON
  Response := Session.Put('/users/1', '{"name":"Jane"}', 'application/json'); // PUT request
  Response := Session.Delete('/users/1');                    // DELETE request
end;

// Session cookie management
var
  Session: THttpSession;
begin
  Session.Init;
  Session.SetCookie('sessionid', 'abc123');                  // Set cookie for all requests
  Session.SetCookie('preference', 'dark-mode');              
  
  CookieValue := Session.GetCookie('sessionid');             // Get cookie value
  Session.ClearCookies;                                      // Clear all cookies
end;

// Session header management
var
  Session: THttpSession;
begin
  Session.Init;
  Session.SetHeader('Accept', 'application/json');           // Set header
  Session.SetHeader('Content-Type', 'application/json');     
  Session.ClearHeaders;                                      // Clear all headers (except Accept)
end;

// Persistent session with automatic cookie handling
var
  Session: THttpSession;
  LoginResponse, DataResponse: TResponse;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  
  // Login and get session cookie
  LoginResponse := Session.Post('/login', 'username=user&password=pass');
  // Session automatically stores cookies from server
  
  // Subsequent requests will include the session cookie
  DataResponse := Session.Get('/protected-data');            // Uses stored cookies
  
  if DataResponse.StatusCode = 200 then
    WriteLn('Protected data: ', DataResponse.Text);
end;

// Session error handling
var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  
  try
    Response := Session.Get('/protected');
    if Response.StatusCode = 200 then
      WriteLn('Success: ', Response.Text)
    else
      WriteLn('HTTP Error: ', Response.StatusCode);
  except
    on E: ERequestError do
      WriteLn('Request Error: ', E.Message);
  end;
end;

// Session with JSON response handling
var
  Session: THttpSession;
  Response: TResponse;
  UserData: TJSONObject;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com');
  Session.SetHeader('Accept', 'application/json');
  
  Response := Session.Get('/user/profile');
  if Response.StatusCode = 200 then
  begin
    UserData := TJSONObject(Response.JSON);                   // Gets reference to Response's internal JSON
    try
      WriteLn('User: ', UserData.Get('name', 'Unknown'));
      WriteLn('Email: ', UserData.Get('email', ''));
    finally
      // Do NOT free UserData - it's just a reference to Response.JSON
      // Response will automatically free its JSON when it goes out of scope
    end;
  end;
end; // <- Response.Finalize automatically called here, freeing the JSON

// Advanced session configuration
var
  Session: THttpSession;
begin
  Session.Init;
  Session.SetBaseURL('https://api.example.com/v2');
  Session.SetUserAgent('MyApp/2.1 (Windows; en-US)');
  Session.SetTimeout(30000);                                 // 30 second timeout
  Session.SetHeader('Accept-Language', 'en-US,en;q=0.9');
  Session.SetHeader('Cache-Control', 'no-cache');
  
  // All subsequent requests will use these settings
end;
```
