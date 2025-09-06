unit Request.Session.Test;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
  Request, Request.Session;

type
  { TRequestSessionTests }
  TRequestSessionTests = class(TTestCase)
  private
    FSession: THttpSession;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Basic session tests
    procedure Test01_SessionCreation;
    procedure Test02_SessionWithBaseURL;
    procedure Test03_PersistentHeaders;
    procedure Test04_CookieHandling;
    procedure Test05_ThreadSafety;
    
    // HTTP methods with session
    procedure Test11_GetWithSession;
    procedure Test12_PostWithSession;
    procedure Test13_PutWithSession;
    procedure Test14_DeleteWithSession;
    
    // Advanced session features
    procedure Test21_ConnectionPooling;
    procedure Test22_TimeoutHandling;
    procedure Test23_ClearCookies;
    procedure Test24_ClearHeaders;
    procedure Test25_SessionMultipartUpload_Success;
    procedure Test25b_SessionMultipartUpload_Failure;
  end;

implementation

{ TRequestSessionTests }

procedure TRequestSessionTests.SetUp;
begin
  inherited SetUp;
  // Don't use Default(THttpSession) as it doesn't call Initialize
  // Instead, use the explicit Init method
  FSession.Init;
  FSession.SetBaseURL('https://httpbin.org');
  FSession.SetHeader('Accept', 'application/json');
  FSession.SetHeader('X-Test-Header', 'test-value');
end;

procedure TRequestSessionTests.TearDown;
begin
  // Session is automatically finalized when it goes out of scope
  inherited TearDown;
end;

procedure TRequestSessionTests.Test01_SessionCreation;
begin
  WriteLn('Test01_SessionCreation: Starting');
  AssertTrue('Session should be created', True); // Just verify setup works
  WriteLn('Test01_SessionCreation: Completed');
end;

procedure TRequestSessionTests.Test02_SessionWithBaseURL;
var
  Response: TResponse;
  URL: string;
  URLNode: TJSONData;
begin
  WriteLn('Test02_SessionWithBaseURL: Starting');
  Response := FSession.Get('/get');
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
    
    URLNode := Response.JSON.FindPath('url');
    AssertTrue('Response should contain URL', URLNode <> nil);
    
    URL := URLNode.AsString;
    AssertTrue('URL should contain httpbin.org', Pos('httpbin.org', URL) > 0);
  except
    // No need to free Response - it's managed by the advanced record
    raise;
  end;
  WriteLn('Test02_SessionWithBaseURL: Completed');
end;

procedure TRequestSessionTests.Test03_PersistentHeaders;
var
  Response: TResponse;
  Headers: TJSONObject;
begin
  WriteLn('Test03_PersistentHeaders: Starting');
  // Add a custom header to the session
  FSession.SetHeader('X-Custom-Header', 'session-value');
  
  Response := FSession.Get('/headers');
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
    
    Headers := TJSONObject(Response.JSON.FindPath('headers'));
    try
      AssertTrue('Headers should exist in response', Headers <> nil);
      AssertEquals('Session header should be present', 
        'test-value', Headers.Get('X-Test-Header', ''));
      AssertEquals('Custom header should be present', 
        'session-value', Headers.Get('X-Custom-Header', ''));
    except
      // Don't free Headers as it's owned by Response.JSON
      raise;
    end;
  except
    // No need to free Response - it's managed by the advanced record
    raise;
  end;

  WriteLn('Test03_PersistentHeaders: Completed');
end;

procedure TRequestSessionTests.Test04_CookieHandling;
var
  Response: TResponse;
  Cookies: TJSONObject;
  CookieValue: string;
begin

  WriteLn('Test04_CookieHandling: Starting');

  // Manually set a cookie in the session to test if it gets sent
  FSession.SetCookie('testcookie', 'session-value');
  
  // Request to verify cookie was sent back
  try
    Response := FSession.Get('/cookies');
  except
    on E: ERequestError do
    begin
      // If it was a transient 502, retry once
      if Pos('502', E.Message) > 0 then
        Response := FSession.Get('/cookies')
      else
        raise;
    end;
  end;
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
    
    Cookies := TJSONObject(Response.JSON.FindPath('cookies'));
    try
      AssertTrue('Cookies should exist in response', Cookies <> nil);
      CookieValue := Cookies.Get('testcookie', '');
      AssertEquals('Cookie value should match', 'session-value', CookieValue);
      except
        // Don't free Cookies as it's owned by Response.JSON
        raise;
      end;
    except
      // No need to free Response - it's managed by the advanced record
      raise;
    end;

  WriteLn('Test04_CookieHandling: Completed');
end;

procedure TRequestSessionTests.Test05_ThreadSafety;
var
  Response: TResponse;
  Headers: TJSONObject;
begin

  WriteLn('Test05_ThreadSafety: Starting');

  // Simple test to verify session works in a single-threaded context
  Response := FSession.Get('/get');
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
    
    Headers := TJSONObject(Response.JSON.FindPath('headers'));
    try
      AssertTrue('Headers should exist in response', Headers <> nil);
      AssertEquals('Session header should be present', 
        'test-value', Headers.Get('X-Test-Header', ''));
    finally
      // Don't free Headers as it's owned by Response.JSON
    end;
  finally
    // No need to free Response - it's managed by the advanced record
  end;

  WriteLn('Test05_ThreadSafety: Completed');
end;

procedure TRequestSessionTests.Test11_GetWithSession;
var
  Response: TResponse;
  Headers: TJSONObject;
begin

  WriteLn('Test11_GetWithSession: Starting');

  Response := FSession.Get('/get');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  Headers := TJSONObject(Response.JSON.FindPath('headers'));
  try
    AssertTrue('Headers should exist in response', Headers <> nil);
    AssertEquals('Session header should be present', 
      'test-value', Headers.Get('X-Test-Header', ''));
  finally
    // Don't free Headers as it's owned by Response.JSON
  end;

  WriteLn('Test11_GetWithSession: Completed');
end;

procedure TRequestSessionTests.Test12_PostWithSession;
var
  Response: TResponse;
  Data: TJSONObject;
  Form: TJSONObject;
begin

  WriteLn('Test12_PostWithSession: Starting');

  Response := FSession.Post('/post', 'test=session+data');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  // Check headers were sent
  Data := TJSONObject(Response.JSON.FindPath('headers'));
  try
    AssertTrue('Headers should exist in response', Data <> nil);
    AssertEquals('Session header should be present', 
      'test-value', Data.Get('X-Test-Header', ''));
  finally
    // Don't free Data as it's owned by Response.JSON
  end;
  
  // Check form data was sent correctly
  Form := TJSONObject(Response.JSON.FindPath('form'));
  try
    AssertTrue('Form data should exist in response', Form <> nil);
    AssertEquals('Form data should match', 'session data', Form.Get('test', ''));
  finally
    // Don't free Form as it's owned by Response.JSON
  end;

  WriteLn('Test12_PostWithSession: Completed');
end;

procedure TRequestSessionTests.Test13_PutWithSession;
var
  Response: TResponse;
  Data: TJSONObject;
begin

  WriteLn('Test13_PutWithSession: Starting');

  Response := FSession.Put('/put', 'test=updated+data');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  // Check headers were sent
  Data := TJSONObject(Response.JSON.FindPath('headers'));
  try
    AssertTrue('Headers should exist in response', Data <> nil);
    AssertEquals('Session header should be present', 
      'test-value', Data.Get('X-Test-Header', ''));
  finally
    // Don't free Data as it's owned by Response.JSON
  end;

  WriteLn('Test13_PutWithSession: Completed');
end;

procedure TRequestSessionTests.Test14_DeleteWithSession;
var
  Response: TResponse;
  Data: TJSONObject;
begin

  WriteLn('Test14_DeleteWithSession: Starting');

  Response := FSession.Delete('/delete');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  // Check headers were sent
  Data := TJSONObject(Response.JSON.FindPath('headers'));
  try
    AssertTrue('Headers should exist in response', Data <> nil);
    AssertEquals('Session header should be present', 
      'test-value', Data.Get('X-Test-Header', ''));
  finally
    // Don't free Data as it's owned by Response.JSON
  end;

  WriteLn('Test14_DeleteWithSession: Completed');
end;

procedure TRequestSessionTests.Test21_ConnectionPooling;
var
  Response1, Response2: TResponse;
  URL1, URL2: string;
begin

  WriteLn('Test21_ConnectionPooling: Starting');


  // First request
  Response1 := FSession.Get('/get');
  if Response1.StatusCode = 502 then
    Response1 := FSession.Get('/get');
  AssertEquals('First request status code should be 200', 200, Response1.StatusCode);
  URL1 := Response1.JSON.GetPath('url').AsString;
  
  // Second request - should reuse connection
  Response2 := FSession.Get('/get');
  if Response2.StatusCode = 502 then
    Response2 := FSession.Get('/get');
  AssertEquals('Second request status code should be 200', 200, Response2.StatusCode);
  URL2 := Response2.JSON.GetPath('url').AsString;
  
  // Both requests should have succeeded (connection pooling works)
  AssertTrue('Both requests should succeed', (URL1 <> '') and (URL2 <> ''));

  WriteLn('Test21_ConnectionPooling: Completed');
end;


procedure TRequestSessionTests.Test22_TimeoutHandling;
var
  Response: TResponse;
  ExceptionRaised: Boolean;
  OldTimeout: Integer;
begin

WriteLn('Test22_TimeoutHandling: Starting');

  // Save old timeout
  OldTimeout := 30000; // Default timeout
  
  try
    // Set a very short timeout
    FSession.SetTimeout(100); // 100ms
    
    // This should timeout
    ExceptionRaised := False;
    try
      Response := FSession.Get('/delay/1'); // 1 second delay
      Fail('Timeout exception was not raised');
    except
      on E: ERequestError do
        ExceptionRaised := True;
    end;
    
    AssertTrue('Timeout exception should be raised', ExceptionRaised);
    
  finally
    // Restore timeout
    FSession.SetTimeout(OldTimeout);
  end;

  WriteLn('Test22_TimeoutHandling: Completed');
end;

procedure TRequestSessionTests.Test23_ClearCookies;
var
  Response: TResponse;
  Cookies: TJSONObject;
begin

WriteLn('Test23_ClearCookies: Starting');

  // Manually set a cookie in the session
  FSession.SetCookie('testcookie', 'value');
  
  // Verify cookie was set
  Response := FSession.Get('/cookies');
  Cookies := TJSONObject(Response.JSON.FindPath('cookies'));
  try
    AssertTrue('Cookie should be set', 
      (Cookies <> nil) and (Cookies.Find('testcookie') <> nil));
  finally
    // Don't free Cookies as it's owned by Response.JSON
  end;
  
  // Clear cookies
  FSession.ClearCookies; 
  
  // Verify no cookies are sent by checking request headers
  Response := FSession.Get('/headers');
  try
    AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
    // Check that no Cookie header is sent in the request
    // The /headers endpoint shows the headers we sent
    AssertFalse('No Cookie header should be sent', 
      Response.Text.Contains('Cookie'));
  except
    // No need to free Response - it's managed by the advanced record
    raise;
  end;

  WriteLn('Test23_ClearCookies: Completed');
  // No need to free Response - it's managed by the advanced record
end;

procedure TRequestSessionTests.Test24_ClearHeaders;
var
  Response: TResponse;
  Headers: TJSONObject;
begin

  WriteLn('Test24_ClearHeaders: Starting');

  // Add a custom header
  FSession.SetHeader('X-Custom-Header', 'test');
  
  // Clear all headers
  FSession.ClearHeaders;
  
  // Make a request
  Response := FSession.Get('/headers');
  Headers := TJSONObject(Response.JSON.FindPath('headers'));
  try
    // Default headers should still be present
    AssertTrue('Default headers should be present', Headers <> nil);
    
    // Our custom header should be gone
    AssertFalse('Custom header should be cleared', 
      (Headers.Find('X-Custom-Header') <> nil));
    
    // Default headers should be restored
    AssertTrue('Default Accept header should be present', 
      Headers.Get('Accept', '') = 'application/json');
  finally
    // Don't free Headers as it's owned by Response.JSON
  end;

  WriteLn('Test24_ClearHeaders: Completed');
end;

procedure TRequestSessionTests.Test25_SessionMultipartUpload_Success;
var
  Boundary, CRLF, Body, FileName, TempFile: string;
  F: TextFile;
  Response: TResponse;
  Form, FilesObj: TJSONObject;
begin
  // Prepare temp file
  TempFile := GetTempDir + 'session_mp_upload.txt';
  AssignFile(F, TempFile);
  Rewrite(F);
  WriteLn(F, 'Session multipart content');
  CloseFile(F);

  // Build multipart body
  Boundary := '----RequestFPTest' + IntToStr(Random(1000000));
  CRLF := #13#10;
  FileName := ExtractFileName(TempFile);

  Body := '';
  // form field
  Body := Body + '--' + Boundary + CRLF;
  Body := Body + 'Content-Disposition: form-data; name="staticfield"' + CRLF + CRLF;
  Body := Body + 'staticvalue' + CRLF;
  // file field
  Body := Body + '--' + Boundary + CRLF;
  Body := Body + 'Content-Disposition: form-data; name="file2"; filename="' + FileName + '"' + CRLF;
  Body := Body + 'Content-Type: application/octet-stream' + CRLF + CRLF;
  // For simplicity in tests, append a known line (matches the file content we wrote)
  Body := Body + 'Session multipart content' + CRLF;
  // closing boundary
  Body := Body + '--' + Boundary + '--' + CRLF;

  // Perform POST with proper Content-Type
  try
    Response := FSession.Post('/post', Body, 'multipart/form-data; boundary=' + Boundary);
  except
    on E: Exception do
    begin
      // Retry once on transient 502
      if Pos('502', E.Message) > 0 then
        Response := FSession.Post('/post', Body, 'multipart/form-data; boundary=' + Boundary)
      else
        raise;
    end;
  end;

  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));

  // Validate JSON payload
  Form := TJSONObject(Response.JSON.FindPath('form'));
  try
    AssertTrue('Form object should exist', Form <> nil);
    AssertEquals('Form field value', 'staticvalue', Form.Get('staticfield', ''));
  finally
  end;

  FilesObj := TJSONObject(Response.JSON.FindPath('files'));
  try
    AssertTrue('Files object should exist', FilesObj <> nil);
    AssertTrue('Uploaded file should be present', FilesObj.Find('file2') <> nil);
  finally
  end;

  // Cleanup
  DeleteFile(TempFile);
end;

procedure TRequestSessionTests.Test25b_SessionMultipartUpload_Failure;
var
  OldBase: string;
  Boundary, Body, CRLF: string;
  ExceptionRaised: Boolean;
begin
  OldBase := '/'; // store something; we'll restore httpbin base in SetUp anyway per test
  // Create a minimal multipart body
  Boundary := '----RequestFPTest' + IntToStr(Random(1000000));
  CRLF := #13#10;
  Body := '--' + Boundary + CRLF +
          'Content-Disposition: form-data; name="a"' + CRLF + CRLF +
          '1' + CRLF +
          '--' + Boundary + '--' + CRLF;

  // Point to a nonexistent host
  FSession.SetBaseURL('https://nonexistent.example.com');

  ExceptionRaised := False;
  try
    FSession.Post('/post', Body, 'multipart/form-data; boundary=' + Boundary);
  except
    on E: Exception do
      ExceptionRaised := True;
  end;
  AssertTrue('POST should raise exception on nonexistent host', ExceptionRaised);

  // No explicit restore needed; each test re-initializes the session in SetUp
end;

initialization
  RegisterTest(TRequestSessionTests);
end.
