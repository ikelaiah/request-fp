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
    
    // Builder pattern tests
    procedure Test05_BuilderWithHeaders;
    procedure Test06_BuilderWithParams;
    procedure Test07_BuilderWithTimeout;
    procedure Test08_BuilderWithAuth;
    
    // Content handling
    procedure Test09_JSONRequest;
    procedure Test10_FormDataRequest;
    
    // Error handling
    procedure Test11_TryGetSuccess;
    procedure Test12_TryGetFailure;
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

procedure TRequestSimpleTests.Test05_BuilderWithHeaders;
var
  Response: TResponse;
  Request: THttpRequest;  // Initialize is called automatically
  Headers: TJSONObject;
begin
  Response := Request
    .Get
    .URL('https://httpbin.org/headers')
    .AddHeader('X-Custom-Header', 'test')
    .AddHeader('User-Agent', 'TidyKit-Test')
    .Send;
  
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  Headers := TJSONObject(Response.JSON.FindPath('headers'));
  try
    AssertTrue('Headers should exist in response', Headers <> nil);
    AssertEquals('Custom header should be echoed back', 'test', Headers.Get('X-Custom-Header', ''));
  finally
    // Don't free Headers as it's owned by Response.JSON
  end;
end;

procedure TRequestSimpleTests.Test06_BuilderWithParams;
var
  Response: TResponse;
  Request: THttpRequest;  // Initialize is called automatically
  Args: TJSONObject;
begin
  Response := Request
    .Get
    .URL('https://httpbin.org/get')
    .AddParam('page', '1')
    .AddParam('limit', '10')
    .Send;
  
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  Args := TJSONObject(Response.JSON.FindPath('args'));
  try
    AssertTrue('Args should exist in response', Args <> nil);
    AssertEquals('Page param should be present', '1', Args.Get('page', ''));
    AssertEquals('Limit param should be present', '10', Args.Get('limit', ''));
  finally
    // Don't free Args as it's owned by Response.JSON
  end;
end;

procedure TRequestSimpleTests.Test07_BuilderWithTimeout;
var
  Response: TResponse;
  Request: THttpRequest;  // Initialize is called automatically
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  
  try
    Response := Request
      .Get
      .URL('https://httpbin.org/delay/2')
      .WithTimeout(1)
      .Send;
  except
    on E: ERequestError do
      ExceptionRaised := True;
  end;
  
  AssertTrue('Timeout exception should be raised', ExceptionRaised);
end;

procedure TRequestSimpleTests.Test08_BuilderWithAuth;
var
  Response: TResponse;
  Request: THttpRequest;  // Initialize is called automatically
  Headers: TJSONObject;
begin
  Response := Request
    .Get
    .URL('https://httpbin.org/basic-auth/username/password')
    .BasicAuth('username', 'password')
    .Send;
  
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response text should not be empty', Response.Text <> '');
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  AssertTrue('Response should contain authenticated user', 
    (Response.JSON.FindPath('authenticated') <> nil) and 
    (Response.JSON.FindPath('user') <> nil));
  
  Headers := TJSONObject(Response.JSON.FindPath('headers'));
  try
    if Headers <> nil then
      AssertTrue('Authorization header should exist', Headers.Find('Authorization') <> nil);
  finally
    Headers.Free;
  end;
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
  Request: THttpRequest;  // Initialize is called automatically
  FormData: TJSONObject;
begin
  Response := Request
    .Post
    .URL('https://httpbin.org/post')
    .WithData('name=John&age=30')
    .Send;
    
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

initialization
  RegisterTest(TRequestSimpleTests);
end.
