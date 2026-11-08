unit Request.Test;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
  openssl, Request, Test.Support;

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
    // New helper coverage
    procedure Test23_IsSuccessStatus;
    procedure Test24_SaveToFile;

    // SSL/OpenSSL initialization test (optional, not run in -a mode)
    procedure Test25_SSLInitialization;
    procedure Test26_ConvenienceHelpers;
    procedure Test27_PostForm;
    procedure Test28_SaveToFileBinary;
    procedure Test29_PatchAndHead;
    procedure Test30_NetworkErrorIsTyped;
    procedure Test31_TimeoutRaisesTimeoutError;
    procedure Test32_ResponseBytesAndStream;
    procedure Test33_MultipartEscapingAndContentType;
    procedure Test34_MultipartMissingFileRaisesRequestError;
    procedure Test35_PostJSONKeepsCustomContentType;
  end;

implementation

procedure TRequestSimpleTests.Test01_SimpleGet;
var
  Response: TResponse;
begin
  Response := Http.Get(TestURL('/get'));
  // Retry once when a developer uses the public fallback service.
  if Response.StatusCode = 502 then
    Response := Http.Get(TestURL('/get'));
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response content should not be empty', Response.Text <> '');
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
end;

procedure TRequestSimpleTests.Test02_SimplePost;
var
  Response: TResponse;
  FormData: TJSONObject;
begin
  Response := Http.Post(TestURL('/post'), 'test=value');
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
  Response := Http.GetWithParams(TestURL('/get'), [
    KV('q', Q),
    KV('u', U)
  ]);
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  JsonObj := TJSONObject(Response.JSON);
  try
    AssertEquals('Query q should be decoded correctly', Q, JsonObj.FindPath('args.q').AsString);
    AssertEquals('Query u should be decoded correctly', U, JsonObj.FindPath('args.u').AsString);
  finally
  end;
end;

procedure TRequestSimpleTests.Test26_ConvenienceHelpers;
var
  Pair: TKeyValue;
  Response: TResponse;
  RequestResult: TRequestResult;
  RaisedError: Boolean;
begin
  Pair := KV('X-Test', 'value');
  AssertEquals('KV should set the key', 'X-Test', Pair.Key);
  AssertEquals('KV should set the value', 'value', Pair.Value);

  Response.StatusCode := 204;
  AssertTrue('OK should be true for 2xx status', Response.OK);
  Response.RaiseForStatus;

  RequestResult.Success := True;
  RequestResult.Response.StatusCode := 200;
  AssertTrue('Result.OK should include transport and HTTP status',
    RequestResult.OK);
  RequestResult.Response.StatusCode := 404;
  AssertFalse('Result.OK should reject non-2xx status', RequestResult.OK);

  Response.StatusCode := 404;
  RaisedError := False;
  try
    Response.RaiseForStatus;
  except
    on E: ERequestError do
      RaisedError := Pos('404', E.Message) > 0;
  end;
  AssertTrue('RaiseForStatus should report non-2xx status', RaisedError);

  RequestResult := Http.TryPostJSON(TestFailureURL,
    TJSONData(nil));
  AssertFalse('TryPostJSON should reject nil without raising',
    RequestResult.Success);
  AssertTrue('TryPostJSON should explain the nil value',
    Pos('cannot be nil', RequestResult.Error) > 0);
end;

procedure TRequestSimpleTests.Test27_PostForm;
var
  Response: TResponse;
  FormData: TJSONObject;
begin
  Response := Http.PostForm(TestURL('/post'), [
    KV('name', 'Ada Lovelace'),
    KV('language', 'Pascal & friends')
  ]);
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  FormData := TJSONObject(Response.JSON.FindPath('form'));
  AssertTrue('Form data should exist', FormData <> nil);
  AssertEquals('Spaces should be encoded and decoded', 'Ada Lovelace',
    FormData.Get('name', ''));
  AssertEquals('Symbols should be encoded and decoded', 'Pascal & friends',
    FormData.Get('language', ''));
end;

procedure TRequestSimpleTests.Test19_JSONAccessOnNonJSONRaises;
var
  Response: TResponse;
  RaisedErr: Boolean;
begin
  Response := Http.Get(TestURL('/html'));
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
  Response := Http.Put(TestURL('/put'), 'test=updated');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response text should not be empty', Response.Text <> '');
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  AssertTrue('Response should contain form data', Assigned(Response.JSON.FindPath('form')));
end;

procedure TRequestSimpleTests.Test04_SimpleDelete;
var
  Response: TResponse;
begin
  Response := Http.Delete(TestURL('/delete'));
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response text should not be empty', Response.Text <> '');
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  AssertTrue('Response should contain URL', Assigned(Response.JSON.FindPath('url')));
end;

procedure TRequestSimpleTests.Test09_JSONRequest;
var
  Response: TResponse;
  JsonData, RequestBody: TJSONObject;
  RequestResult: TRequestResult;
begin
  RequestBody := TJSONObject.Create;
  try
    RequestBody.Add('name', 'John');
    RequestBody.Add('age', 30);
    RequestResult := Http.TryPostJSON(TestURL('/post'), RequestBody);
  finally
    RequestBody.Free;
  end;

  AssertTrue('TryPostJSON should succeed', RequestResult.Success);
  Response := RequestResult.Response;
    
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
  Response := Http.Post(TestURL('/post'), 'name=John&age=30');
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
  Result := Http.TryGet(TestURL('/get'));
  AssertTrue('Request should succeed', Result.Success);
  AssertEquals('Status code should be 200', 200, Result.Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Result.Response.JSON));
  AssertEquals('No error message expected', '', Result.Error);
end;

procedure TRequestSimpleTests.Test12_TryGetFailure;
var
  Result: TRequestResult;
begin
  Result := Http.TryGet(TestFailureURL);
  AssertFalse('Request should fail', Result.Success);
  AssertTrue('Error message should not be empty', Result.Error <> '');
end;

procedure TRequestSimpleTests.Test13_TryPostSuccess;
var
  R: TRequestResult;
begin
  R := Http.TryPost(TestURL('/post'), 'x=1&y=2');
  AssertTrue('TryPost should succeed', R.Success);
  AssertEquals('Status code should be 200', 200, R.Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(R.Response.JSON));
  AssertEquals('No error message expected', '', R.Error);
end;

procedure TRequestSimpleTests.Test13b_TryPostFailure;
var
  R: TRequestResult;
begin
  R := Http.TryPost(TestFailureURL, 'x=1');
  AssertFalse('TryPost should fail', R.Success);
  AssertTrue('Error should be populated', R.Error <> '');
end;

procedure TRequestSimpleTests.Test20_TryPolicy_4xx5xx;
var
  R4, R5: TRequestResult;
begin
  // 404
  R4 := Http.TryGet(TestURL('/status/404'));
  AssertTrue('TryGet should not treat 404 as transport failure', R4.Success);
  AssertEquals('Status code should be 404', 404, R4.Response.StatusCode);
  AssertEquals('No transport error expected', '', R4.Error);

  // 500
  R5 := Http.TryGet(TestURL('/status/500'));
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

  Response := Http.PostMultipart(TestURL('/post'),
    [TKeyValue.Create('staticfield', 'staticvalue')],
    [TKeyValue.Create('file2', TempFile)]);
  // Retry once when a developer uses the public fallback service.
  if Response.StatusCode = 502 then
    Response := Http.PostMultipart(TestURL('/post'),
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
  Response := Http.Get(TestURL('/get'),
    [TKeyValue.Create('X-Test-Header', 'HeaderValue')],
    [TKeyValue.Create('foo', 'bar'), TKeyValue.Create('baz', 'qux')]);
  // Retry once when a developer uses the public fallback service.
  if Response.StatusCode = 502 then
    Response := Http.Get(TestURL('/get'),
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
  R := Http.TryPut(TestURL('/put'), 'a=updated');
  AssertTrue('TryPut should succeed', R.Success);
  AssertEquals('Status code should be 200', 200, R.Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(R.Response.JSON));
  AssertEquals('No error message expected', '', R.Error);
end;

procedure TRequestSimpleTests.Test16b_TryPutFailure;
var
  R: TRequestResult;
begin
  R := Http.TryPut(TestFailureURL, 'a=b');
  AssertFalse('TryPut should fail', R.Success);
  AssertTrue('Error should be populated', R.Error <> '');
end;

procedure TRequestSimpleTests.Test17_TryDeleteSuccess;
var
  R: TRequestResult;
begin
  R := Http.TryDelete(TestURL('/delete'));
  AssertTrue('TryDelete should succeed', R.Success);
  AssertEquals('Status code should be 200', 200, R.Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(R.Response.JSON));
  AssertEquals('No error message expected', '', R.Error);
end;

procedure TRequestSimpleTests.Test17b_TryDeleteFailure;
var
  R: TRequestResult;
begin
  R := Http.TryDelete(TestFailureURL);
  AssertFalse('TryDelete should fail', R.Success);
  AssertTrue('Error should be populated', R.Error <> '');
end;

procedure TRequestSimpleTests.Test21_HeaderValueExtraction;
var
  Response: TResponse;
  CT: string;
begin
  Response := Http.Get(TestURL('/get'));
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

  R := Http.TryPostMultipart(TestURL('/post'),
    [TKeyValue.Create('staticfield', 'staticvalue')],
    [TKeyValue.Create('file2', TempFile)]);
  // Retry once when a developer uses the public fallback service.
  if R.Success and (R.Response.StatusCode = 502) then
    R := Http.TryPostMultipart(TestURL('/post'),
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
  R := Http.TryPostMultipart(TestFailureURL,
    [TKeyValue.Create('a', '1')], []);
  AssertFalse('TryPostMultipart should fail on connection failure', R.Success);
  AssertTrue('Error should be populated', R.Error <> '');
end;

procedure TRequestSimpleTests.Test23_IsSuccessStatus;
var
  R200, R404: TResponse;
begin
  R200 := Http.Get(TestURL('/get'));
  // Retry once when a developer uses the public fallback service.
  if R200.StatusCode = 502 then
    R200 := Http.Get(TestURL('/get'));
  AssertTrue('200 should be success', R200.IsSuccessStatus);

  R404 := Http.Get(TestURL('/status/404'));
  AssertFalse('404 should not be success', R404.IsSuccessStatus);
end;

procedure TRequestSimpleTests.Test24_SaveToFile;
var
  R: TResponse;
  TempFile: string;
  Info: TSearchRec;
  HasFile: Boolean;
begin
  R := Http.PostJSON(TestURL('/post'), '{"a":1}');
  AssertEquals('Status code should be 200', 200, R.StatusCode);
  TempFile := GetTempDir + 'request_fp_save_test.txt';
  try
    R.SaveToFile(TempFile);
    HasFile := FindFirst(TempFile, faAnyFile, Info) = 0;
    try
      AssertTrue('File should exist after SaveToFile', HasFile);
      AssertTrue('File size should be > 0', Info.Size > 0);
    finally
      if HasFile then FindClose(Info);
    end;
  finally
    // Cleanup regardless of assertions
    if FileExists(TempFile) then DeleteFile(TempFile);
  end;
end;

procedure TRequestSimpleTests.Test28_SaveToFileBinary;
const
  ByteCount = 512;
var
  R: TResponse;
  TempFile: string;
  FS: TFileStream;
  Saved: array of Byte;
  I: Integer;
begin
  R := Http.Get(TestURL('/bytes/' + IntToStr(ByteCount)));
  AssertEquals('Status code should be 200', 200, R.StatusCode);
  AssertEquals('Body length should match', ByteCount, Length(R.Text));

  TempFile := GetTempDir + 'request_fp_save_binary_test.bin';
  try
    R.SaveToFile(TempFile);
    FS := TFileStream.Create(TempFile, fmOpenRead or fmShareDenyWrite);
    try
      AssertEquals('Saved size should match', ByteCount, FS.Size);
      SetLength(Saved, FS.Size);
      if FS.Size > 0 then
        FS.ReadBuffer(Saved[0], FS.Size);
    finally
      FS.Free;
    end;
    for I := 0 to ByteCount - 1 do
      AssertEquals('Byte ' + IntToStr(I) + ' should match', I mod 256,
        Saved[I]);
  finally
    if FileExists(TempFile) then DeleteFile(TempFile);
  end;
end;

procedure TRequestSimpleTests.Test29_PatchAndHead;
var
  R: TResponse;
  Res: TRequestResult;
begin
  R := Http.Patch(TestURL('/patch'), 'name=Ada');
  AssertEquals('PATCH status', 200, R.StatusCode);
  AssertEquals('PATCH form echoed', 'Ada',
    R.JSON.FindPath('form.name').AsString);

  R := Http.Head(TestURL('/get'));
  AssertEquals('HEAD status', 200, R.StatusCode);
  AssertEquals('HEAD has no body', '', R.Text);
  AssertTrue('HEAD returns headers', R.HeaderValue('Content-Type') <> '');

  R := Http.Head(TestURL('/status/404'));
  AssertEquals('HEAD 404 returned', 404, R.StatusCode);

  Res := Http.TryPatch(TestURL('/status/409'), '{}');
  AssertTrue('TryPatch transport succeeded', Res.Success);
  AssertEquals('TryPatch status', 409, Res.Response.StatusCode);

  Res := Http.TryHead(TestFailureURL);
  AssertFalse('TryHead failure reported', Res.Success);
end;

procedure TRequestSimpleTests.Test30_NetworkErrorIsTyped;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    Http.Get(TestFailureURL);
  except
    on E: ENetworkError do
      Raised := True;
  end;
  AssertTrue('Connection failure should raise ENetworkError', Raised);
end;

procedure TRequestSimpleTests.Test31_TimeoutRaisesTimeoutError;
var
  OldTimeout: Integer;
  Raised: Boolean;
begin
  OldTimeout := Http.GetTimeout;
  AssertEquals('Default timeout', 30000, OldTimeout);
  Http.SetTimeout(300);
  try
    Raised := False;
    try
      Http.Get(TestURL('/delay/2'));
    except
      on E: ETimeoutError do
        Raised := True;
    end;
    AssertTrue('Slow response should raise ETimeoutError', Raised);
  finally
    Http.SetTimeout(OldTimeout);
  end;
end;

procedure TRequestSimpleTests.Test32_ResponseBytesAndStream;
const
  ByteCount = 300;
var
  R: TResponse;
  Data: TBytes;
  Stream: TMemoryStream;
  I: Integer;
begin
  R := Http.Get(TestURL('/bytes/' + IntToStr(ByteCount)));
  Data := R.Bytes;
  AssertEquals('Bytes length', ByteCount, Length(Data));
  for I := 0 to ByteCount - 1 do
    AssertEquals('Byte ' + IntToStr(I), I mod 256, Data[I]);

  Stream := TMemoryStream.Create;
  try
    R.SaveToStream(Stream);
    AssertEquals('Stream size', ByteCount, Stream.Size);
    AssertTrue('Stream bytes match',
      CompareMem(Stream.Memory, @Data[0], ByteCount));
  finally
    Stream.Free;
  end;
end;

procedure TRequestSimpleTests.Test33_MultipartEscapingAndContentType;
var
  R: TResponse;
  TempFile, TypeA, TypeB: string;
  FS: TFileStream;
  BodyA, BodyB: TStream;
const
  Content = 'hello';
begin
  TempFile := GetTempDir + 'request_fp_note.txt';
  FS := TFileStream.Create(TempFile, fmCreate);
  try
    FS.WriteBuffer(Content[1], Length(Content));
  finally
    FS.Free;
  end;
  try
    R := Http.PostMultipart(TestURL('/post'),
      [KV('a"b', '1')], [KV('doc', TempFile)]);
    AssertEquals('Status', 200, R.StatusCode);
    AssertEquals('Quote in field name is escaped', '1',
      TJSONObject(R.JSON.FindPath('form')).Get('a%22b', ''));
    AssertEquals('File content uploaded', Content,
      R.JSON.FindPath('files.doc').AsString);
    AssertEquals('File content type from extension', 'text/plain',
      R.JSON.FindPath('file_types.doc').AsString);
  finally
    DeleteFile(TempFile);
  end;

  BodyA := BuildMultipartBody([KV('x', '1')], [], TypeA);
  BodyB := BuildMultipartBody([KV('x', '1')], [], TypeB);
  try
    AssertTrue('Each body gets a new boundary', TypeA <> TypeB);
  finally
    BodyA.Free;
    BodyB.Free;
  end;
end;

procedure TRequestSimpleTests.Test34_MultipartMissingFileRaisesRequestError;
var
  Raised: Boolean;
  Res: TRequestResult;
begin
  Raised := False;
  try
    Http.PostMultipart(TestURL('/post'), [],
      [KV('doc', GetTempDir + 'request_fp_missing_file.bin')]);
  except
    on E: ERequestError do
      Raised := Pos('Cannot read upload file', E.Message) > 0;
  end;
  AssertTrue('Missing upload file should raise ERequestError', Raised);

  Res := Http.TryPostMultipart(TestURL('/post'), [],
    [KV('doc', GetTempDir + 'request_fp_missing_file.bin')]);
  AssertFalse('TryPostMultipart reports missing file', Res.Success);
end;

procedure TRequestSimpleTests.Test35_PostJSONKeepsCustomContentType;
var
  R: TResponse;
begin
  R := Http.PostJSON(TestURL('/post'), '{"a":1}',
    [KV('Content-Type', 'application/vnd.api+json')]);
  AssertEquals('Status', 200, R.StatusCode);
  AssertEquals('Custom Content-Type is sent unchanged',
    'application/vnd.api+json',
    R.JSON.FindPath('headers.Content-Type').AsString);
end;

procedure TRequestSimpleTests.Test25_SSLInitialization;
var
  FailureMessage: string;
begin
  { Verify FPC can load and initialize OpenSSL without depending on an
    external HTTPS service. }

  FailureMessage := '';
  try
    AssertTrue('OpenSSL should initialize', InitSSLInterface);
    AssertTrue('OpenSSL should report as loaded', IsSSLloaded);
    WriteLn('SSL initialization test PASSED - OpenSSL is working correctly');
  except
    on E: Exception do
    begin
      FailureMessage := 'SSL initialization test FAILED: ' + E.Message;
      WriteLn(FailureMessage);
      Fail(FailureMessage);
    end;
  end;
end;

initialization
  RegisterTest(TRequestSimpleTests);
end.
