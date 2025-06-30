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
    
    // Error handling
    procedure Test11_TryGetSuccess;
    procedure Test12_TryGetFailure;
    
    // Multipart upload tests
    procedure Test14_MultipartUpload_Static;
    
    // Custom headers and parameters
    procedure Test15_CustomHeadersAndParams;
  end;

implementation

procedure TRequestSimpleTests.Test01_SimpleGet;
var
  Response: TResponse;
begin
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

initialization
  RegisterTest(TRequestSimpleTests);
end.
