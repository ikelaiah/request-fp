unit Request.Test;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
  Request;

type
  { TRequestSimpleTests }
  TRequestSimpleTests = class(TTestCase)
  published
    // Basic HTTP methods
    procedure Test01_SimpleGet;
    procedure Test02_SimplePost;
    procedure Test03_SimplePut;
    procedure Test04_SimpleDelete;
    
    // Content handling
    procedure Test09_JSONRequest;
    procedure Test10_FormDataRequest;
    procedure Test18_URLParamEncoding;
    procedure Test19_JSONAccessOnNonJSONRaises;
    
    // Error handling
    procedure Test11_TryGetSuccess;
    procedure Test12_TryGetFailure;
    procedure Test13_TryPostSuccess;
    procedure Test13b_TryPostFailure;
    procedure Test20_TryPolicy_4xx5xx;
    
    // Multipart upload tests
    procedure Test14_MultipartUpload_Static;
    
    // Custom headers and parameters
    procedure Test15_CustomHeadersAndParams;
    procedure Test16_TryPutSuccess;
    procedure Test16b_TryPutFailure;
    procedure Test17_TryDeleteSuccess;
    procedure Test17b_TryDeleteFailure;
    procedure Test21_HeaderValueExtraction;
    procedure Test22_TryPostMultipartSuccess;
    procedure Test22b_TryPostMultipartFailure;
  end;

implementation

procedure TRequestSimpleTests.Test01_SimpleGet;
var
  Response: TResponse;
begin
  Response := Http.Get('https://httpbin.org/get');
  // Retry once on transient upstream 502 from httpbin
  if Response.StatusCode = 502 then
    Response := Http.Get('https://httpbin.org/get');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response content should not be empty', Response.Text <> '');
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
end;

procedure TRequestSimpleTests.Test02_SimplePost;
var
  Response: TResponse;
  FormData: TJSONObject;
begin
  Response := Http.Post('https://httpbin.org/post', 'test=value');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response text should not be empty', Response.Text <> '');
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  FormData := TJSONObject(Response.JSON.FindPath('form'));
  try
    AssertTrue('Form data should exist in response', (FormData <> nil) and (FormData.Find('test') <> nil));
    AssertEquals('Form value should match', 'value', FormData.Get('test', ''));
  finally
    // Don't free FormData as it's owned by Response.JSON
  end;
end;

procedure TRequestSimpleTests.Test18_URLParamEncoding;
var
  Response: TResponse;
  JsonObj: TJSONObject;
  Q, U: string;
begin
  Q := 'hello world';
  U := 'üñîçødé & symbols?';
  Response := Http.Get('https://httpbin.org/get', [], [
    TKeyValue.Create('q', Q),
    TKeyValue.Create('u', U)
  ]);
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  JsonObj := TJSONObject(Response.JSON);
  try
    AssertEquals('Query q should be decoded correctly', Q, JsonObj.FindPath('args.q').AsString);
    AssertEquals('Query u should be decoded correctly', U, JsonObj.FindPath('args.u').AsString);
  finally
  end;
end;

procedure TRequestSimpleTests.Test19_JSONAccessOnNonJSONRaises;
var
  Response: TResponse;
  RaisedErr: Boolean;
begin
  Response := Http.Get('https://httpbin.org/html');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  RaisedErr := False;
  try
    if Assigned(Response.JSON) then; // force JSON parsing
  except
    on E: ERequestError do
      RaisedErr := Pos('JSON Parse Error', E.Message) = 1;
  end;
  AssertTrue('Accessing JSON on non-JSON should raise ERequestError', RaisedErr);
end;

procedure TRequestSimpleTests.Test03_SimplePut;
var
  Response: TResponse;
begin
  Response := Http.Put('https://httpbin.org/put', 'test=updated');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response text should not be empty', Response.Text <> '');
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  AssertTrue('Response should contain form data', Assigned(Response.JSON.FindPath('form')));
end;

procedure TRequestSimpleTests.Test04_SimpleDelete;
var
  Response: TResponse;
begin
  Response := Http.Delete('https://httpbin.org/delete');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response text should not be empty', Response.Text <> '');
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  AssertTrue('Response should contain URL', Assigned(Response.JSON.FindPath('url')));
end;

procedure TRequestSimpleTests.Test09_JSONRequest;
var
  Response: TResponse;
  JsonData: TJSONObject;
begin
  Response := Http.PostJSON('https://httpbin.org/post',
    '{"name": "John", "age": 30}');
    
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  JsonData := TJSONObject(Response.JSON.FindPath('json'));
  try
    AssertTrue('JSON data should exist in response', JsonData <> nil);
    AssertEquals('Name should match', 'John', JsonData.Get('name', ''));
    AssertEquals('Age should match', 30, JsonData.Get('age', 0));
  finally
    // Don't free JsonData as it's owned by Response.JSON
  end;
end;

procedure TRequestSimpleTests.Test10_FormDataRequest;
var
  Response: TResponse;
  FormData: TJSONObject;
begin
  Response := Http.Post('https://httpbin.org/post', 'name=John&age=30');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  FormData := TJSONObject(Response.JSON.FindPath('form'));
  try
    AssertTrue('Form data should exist in response', FormData <> nil);
    AssertEquals('Name should match', 'John', FormData.Get('name', ''));
    AssertEquals('Age should match', '30', FormData.Get('age', ''));
  finally
    // Don't free FormData as it's owned by Response.JSON
  end;
end;

procedure TRequestSimpleTests.Test11_TryGetSuccess;
var
  Result: TRequestResult;
begin
  Result := Http.TryGet('https://httpbin.org/get');
  AssertTrue('Request should succeed', Result.Success);
  AssertEquals('Status code should be 200', 200, Result.Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Result.Response.JSON));
  AssertEquals('No error message expected', '', Result.Error);
end;

procedure TRequestSimpleTests.Test12_TryGetFailure;
var
  Result: TRequestResult;
begin
  Result := Http.TryGet('https://nonexistent.example.com');
  AssertFalse('Request should fail', Result.Success);
  AssertTrue('Error message should not be empty', Result.Error <> '');
end;

procedure TRequestSimpleTests.Test13_TryPostSuccess;
var
  R: TRequestResult;
begin
  R := Http.TryPost('https://httpbin.org/post', 'x=1&y=2');
  AssertTrue('TryPost should succeed', R.Success);
  AssertEquals('Status code should be 200', 200, R.Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(R.Response.JSON));
  AssertEquals('No error message expected', '', R.Error);
end;

procedure TRequestSimpleTests.Test13b_TryPostFailure;
var
  R: TRequestResult;
begin
  R := Http.TryPost('https://nonexistent.example.com', 'x=1');
  AssertFalse('TryPost should fail', R.Success);
  AssertTrue('Error should be populated', R.Error <> '');
end;

procedure TRequestSimpleTests.Test20_TryPolicy_4xx5xx;
var
  R4, R5: TRequestResult;
begin
  // 404
  R4 := Http.TryGet('https://httpbin.org/status/404');
  AssertTrue('TryGet should not treat 404 as transport failure', R4.Success);
  AssertEquals('Status code should be 404', 404, R4.Response.StatusCode);
  AssertEquals('No transport error expected', '', R4.Error);

  // 500
  R5 := Http.TryGet('https://httpbin.org/status/500');
  AssertTrue('TryGet should not treat 500 as transport failure', R5.Success);
  AssertEquals('Status code should be 500', 500, R5.Response.StatusCode);
  AssertEquals('No transport error expected', '', R5.Error);
end;

procedure TRequestSimpleTests.Test14_MultipartUpload_Static;
var
  Response: TResponse;
  TempFile: string;
  F: TextFile;
  Form, FilesObj: TJSONObject;
begin
  // Create a temporary file to upload
  TempFile := GetTempDir + 'test_upload2.txt';
  AssignFile(F, TempFile);
  Rewrite(F);
  WriteLn(F, 'Static multipart test!');
  CloseFile(F);

  Response := Http.PostMultipart('https://httpbin.org/post', 
    [TKeyValue.Create('staticfield', 'staticvalue')],
    [TKeyValue.Create('file2', TempFile)]);
  // Retry once on transient upstream 502 from httpbin
  if Response.StatusCode = 502 then
    Response := Http.PostMultipart('https://httpbin.org/post', 
      [TKeyValue.Create('staticfield', 'staticvalue')],
      [TKeyValue.Create('file2', TempFile)]);

  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));

  // Check form field
  Form := TJSONObject(Response.JSON.FindPath('form'));
  try
    AssertTrue('Form field should exist', Form <> nil);
    AssertEquals('Form field value', 'staticvalue', Form.Get('staticfield', ''));
  finally
    // Don't free Form as it's owned by Response.JSON
  end;

  // Check file field
  FilesObj := TJSONObject(Response.JSON.FindPath('files'));
  try
    AssertTrue('Files should exist in response', FilesObj <> nil);
    AssertTrue('Uploaded file should be present', FilesObj.Find('file2') <> nil);
  finally
    // Don't free FilesObj as it's owned by Response.JSON
  end;

  // Clean up
  DeleteFile(TempFile);
end;

procedure TRequestSimpleTests.Test15_CustomHeadersAndParams;
var
  Response: TResponse;
  JsonObj: TJSONObject;
begin
  Response := Http.Get('https://httpbin.org/get', 
    [TKeyValue.Create('X-Test-Header', 'HeaderValue')],
    [TKeyValue.Create('foo', 'bar'), TKeyValue.Create('baz', 'qux')]);
  // Retry once on transient upstream 502 from httpbin
  if Response.StatusCode = 502 then
    Response := Http.Get('https://httpbin.org/get', 
      [TKeyValue.Create('X-Test-Header', 'HeaderValue')],
      [TKeyValue.Create('foo', 'bar'), TKeyValue.Create('baz', 'qux')]);
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  JsonObj := TJSONObject(Response.JSON);
  try
    AssertEquals('Header should be present', 'HeaderValue', JsonObj.FindPath('headers.X-Test-Header').AsString);
    AssertEquals('Query param foo', 'bar', JsonObj.FindPath('args.foo').AsString);
    AssertEquals('Query param baz', 'qux', JsonObj.FindPath('args.baz').AsString);
  finally
    // Do not free JsonObj
  end;
end;

procedure TRequestSimpleTests.Test16_TryPutSuccess;
var
  R: TRequestResult;
begin
  R := Http.TryPut('https://httpbin.org/put', 'a=updated');
  AssertTrue('TryPut should succeed', R.Success);
  AssertEquals('Status code should be 200', 200, R.Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(R.Response.JSON));
  AssertEquals('No error message expected', '', R.Error);
end;

procedure TRequestSimpleTests.Test16b_TryPutFailure;
var
  R: TRequestResult;
begin
  R := Http.TryPut('https://nonexistent.example.com', 'a=b');
  AssertFalse('TryPut should fail', R.Success);
  AssertTrue('Error should be populated', R.Error <> '');
end;

procedure TRequestSimpleTests.Test17_TryDeleteSuccess;
var
  R: TRequestResult;
begin
  R := Http.TryDelete('https://httpbin.org/delete');
  AssertTrue('TryDelete should succeed', R.Success);
  AssertEquals('Status code should be 200', 200, R.Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(R.Response.JSON));
  AssertEquals('No error message expected', '', R.Error);
end;

procedure TRequestSimpleTests.Test17b_TryDeleteFailure;
var
  R: TRequestResult;
begin
  R := Http.TryDelete('https://nonexistent.example.com');
  AssertFalse('TryDelete should fail', R.Success);
  AssertTrue('Error should be populated', R.Error <> '');
end;

procedure TRequestSimpleTests.Test21_HeaderValueExtraction;
var
  Response: TResponse;
  CT: string;
begin
  Response := Http.Get('https://httpbin.org/get');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  CT := Response.HeaderValue('Content-Type');
  AssertTrue('Content-Type header should exist', CT <> '');
  AssertTrue('Content-Type should indicate JSON', Pos('application/json', LowerCase(CT)) > 0);
end;

procedure TRequestSimpleTests.Test22_TryPostMultipartSuccess;
var
  R: TRequestResult;
  TempFile: string;
  F: TextFile;
  Form, FilesObj: TJSONObject;
begin
  // Prepare a temp file to upload
  TempFile := GetTempDir + 'test_upload_try_mp.txt';
  AssignFile(F, TempFile);
  Rewrite(F);
  WriteLn(F, 'TryPostMultipart content');
  CloseFile(F);

  R := Http.TryPostMultipart('https://httpbin.org/post',
    [TKeyValue.Create('staticfield', 'staticvalue')],
    [TKeyValue.Create('file2', TempFile)]);
  // Retry once on transient upstream 502 from httpbin
  if R.Success and (R.Response.StatusCode = 502) then
    R := Http.TryPostMultipart('https://httpbin.org/post',
      [TKeyValue.Create('staticfield', 'staticvalue')],
      [TKeyValue.Create('file2', TempFile)]);

  AssertTrue('TryPostMultipart should succeed (transport)', R.Success);
  AssertEquals('Status code should be 200', 200, R.Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(R.Response.JSON));

  // Validate form and files in response JSON
  Form := TJSONObject(R.Response.JSON.FindPath('form'));
  try
    AssertTrue('Form object should exist', Form <> nil);
    AssertEquals('Form field value', 'staticvalue', Form.Get('staticfield', ''));
  finally
  end;

  FilesObj := TJSONObject(R.Response.JSON.FindPath('files'));
  try
    AssertTrue('Files object should exist', FilesObj <> nil);
    AssertTrue('Uploaded file should be present', FilesObj.Find('file2') <> nil);
  finally
  end;

  // Cleanup
  DeleteFile(TempFile);
end;

procedure TRequestSimpleTests.Test22b_TryPostMultipartFailure;
var
  R: TRequestResult;
begin
  R := Http.TryPostMultipart('https://nonexistent.example.com',
    [TKeyValue.Create('a', '1')], []);
  AssertFalse('TryPostMultipart should fail on nonexistent host', R.Success);
  AssertTrue('Error should be populated', R.Error <> '');
end;

initialization
  RegisterTest(TRequestSimpleTests);
end.
