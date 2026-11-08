unit Request.Test;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
  openssl, md5, Request, Test.Support;

type
  { TRequestSimpleTests }
  TRequestSimpleTests = class(TTestCase)
  private
    FProgressCalls: Integer;
    FLastReceived: Int64;
    FLastTotal: Int64;
    FCancelAfter: Integer;
    FFlakyCounter: Integer;
    procedure TrackProgress(Received, Total: Int64);
    function FlakyKey: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
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
    // Streaming downloads
    procedure Test36_DownloadToFileWithProgress;
    procedure Test37_GetToStreamChunkedUnknownTotal;
    procedure Test38_DownloadErrorStatusKeepsFile;
    procedure Test39_DownloadCancelKeepsOldFile;
    procedure Test40_TruncatedDownloadRaises;
    procedure Test41_GetToStreamRedirectAndPlainProc;
    // Streaming uploads
    procedure Test42_MultipartStreamsLargeFile;
    procedure Test43_MultipartBodyRewindsAndReadsFully;
    // Retries
    procedure Test44_RetryOn503And429;
    procedure Test45_RetryStopsAfterMax;
    procedure Test46_RetryAfterHonouredOrReturned;
    procedure Test47_OnlyIdempotentMethodsRetry;
    procedure Test48_RetryOnDroppedConnection;
    procedure Test49_TimeoutIsNotRetried;
    procedure Test50_RetryDefaultsAndDownload;
    // Helpers
    procedure Test51_AuthHeaderHelpers;
    procedure Test52_PutAndPatchJSON;
    procedure Test53_HeaderValuesAndNames;
    procedure Test54_ConnectTimeoutSetting;
    procedure Test55_ProxyForHTTP;
    procedure Test56_CrossHostRedirectDropsAuth;
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

{ Streaming downloads }

var
  GPlainProgressCalls: Integer;
  GPlainLastReceived: Int64;

procedure PlainProgress(Received, Total: Int64);
begin
  Inc(GPlainProgressCalls);
  GPlainLastReceived := Received;
end;

function ReadFileText(const Path: string): RawByteString;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, FS.Size);
    if FS.Size > 0 then
      FS.ReadBuffer(Result[1], FS.Size);
  finally
    FS.Free;
  end;
end;

procedure WriteFileText(const Path: string; const Data: RawByteString);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(Path, fmCreate);
  try
    if Data <> '' then
      FS.WriteBuffer(Data[1], Length(Data));
  finally
    FS.Free;
  end;
end;

function StreamText(Stream: TMemoryStream): RawByteString;
begin
  SetLength(Result, Stream.Size);
  if Stream.Size > 0 then
    Move(Stream.Memory^, Result[1], Stream.Size);
end;

function IsPatternBytes(const Data: RawByteString; Count: Integer): Boolean;
var
  I: Integer;
begin
  Result := Length(Data) = Count;
  if Result then
    for I := 1 to Count do
      if Ord(Data[I]) <> (I - 1) mod 256 then
        Exit(False);
end;

procedure TRequestSimpleTests.TrackProgress(Received, Total: Int64);
begin
  Inc(FProgressCalls);
  FLastReceived := Received;
  FLastTotal := Total;
  if (FCancelAfter > 0) and (FProgressCalls >= FCancelAfter) then
    raise ERequestError.Create('Cancelled by test');
end;

procedure TRequestSimpleTests.Test36_DownloadToFileWithProgress;
const
  ByteCount = 200000;
var
  R: TResponse;
  Path: string;
begin
  Path := GetTempDir + 'request_fp_download.bin';
  DeleteFile(Path);
  FProgressCalls := 0;
  FCancelAfter := 0;
  try
    R := Http.Download(TestURL('/bytes/' + IntToStr(ByteCount)), Path,
      @TrackProgress);
    AssertEquals('Status', 200, R.StatusCode);
    AssertEquals('Body is not kept in memory', '', R.Text);
    AssertEquals('Headers are kept', IntToStr(ByteCount),
      R.HeaderValue('Content-Length'));
    AssertTrue('File holds the exact bytes',
      IsPatternBytes(ReadFileText(Path), ByteCount));
    AssertFalse('No partial file left', FileExists(Path + '.part'));
    AssertTrue('Progress was reported', FProgressCalls > 0);
    AssertEquals('Last progress = all bytes', ByteCount, FLastReceived);
    AssertEquals('Total from Content-Length', ByteCount, FLastTotal);
  finally
    DeleteFile(Path);
  end;
end;

procedure TRequestSimpleTests.Test37_GetToStreamChunkedUnknownTotal;
const
  ByteCount = 5500;
var
  R: TResponse;
  Stream: TMemoryStream;
begin
  FProgressCalls := 0;
  FCancelAfter := 0;
  Stream := TMemoryStream.Create;
  try
    R := Http.GetToStream(TestURL('/stream-bytes/' + IntToStr(ByteCount)),
      Stream, @TrackProgress);
    AssertEquals('Status', 200, R.StatusCode);
    AssertTrue('Chunked body arrives intact',
      IsPatternBytes(StreamText(Stream), ByteCount));
    AssertEquals('Last progress = all bytes', ByteCount, FLastReceived);
    AssertEquals('Total unknown without Content-Length', -1, FLastTotal);
  finally
    Stream.Free;
  end;
end;

procedure TRequestSimpleTests.Test38_DownloadErrorStatusKeepsFile;
var
  R: TResponse;
  Res: TRequestResult;
  Path: string;
begin
  Path := GetTempDir + 'request_fp_keep.txt';
  WriteFileText(Path, 'old');
  try
    R := Http.Download(TestURL('/status/404'), Path);
    AssertEquals('Status', 404, R.StatusCode);
    AssertTrue('Error body is in Text', Pos('404', R.Text) > 0);
    AssertEquals('Existing file untouched', 'old', ReadFileText(Path));
    AssertFalse('No partial file left', FileExists(Path + '.part'));

    Res := Http.TryDownload(TestURL('/status/500'), Path);
    AssertTrue('Transport succeeded', Res.Success);
    AssertFalse('Not OK', Res.OK);
    AssertEquals('Existing file still untouched', 'old', ReadFileText(Path));
  finally
    DeleteFile(Path);
  end;
end;

procedure TRequestSimpleTests.Test39_DownloadCancelKeepsOldFile;
var
  Res: TRequestResult;
  Path: string;
  Raised: Boolean;
begin
  Path := GetTempDir + 'request_fp_cancel.bin';
  WriteFileText(Path, 'old');
  try
    FProgressCalls := 0;
    FCancelAfter := 1;
    Raised := False;
    try
      Http.Download(TestURL('/bytes/300000'), Path, @TrackProgress);
    except
      on E: ERequestError do
        Raised := Pos('Cancelled by test', E.Message) > 0;
    end;
    AssertTrue('Exception from the callback cancels', Raised);
    AssertEquals('Existing file untouched', 'old', ReadFileText(Path));
    AssertFalse('Partial file deleted', FileExists(Path + '.part'));

    FProgressCalls := 0;
    Res := Http.TryDownload(TestURL('/bytes/300000'), Path, @TrackProgress);
    AssertFalse('TryDownload reports the cancel', Res.Success);
    AssertFalse('Partial file deleted', FileExists(Path + '.part'));
  finally
    FCancelAfter := 0;
    DeleteFile(Path);
  end;
end;

procedure TRequestSimpleTests.Test40_TruncatedDownloadRaises;
var
  Path: string;
  Raised: Boolean;
begin
  Path := GetTempDir + 'request_fp_truncated.bin';
  DeleteFile(Path);
  Raised := False;
  try
    Http.Download(TestURL('/truncated-bytes/4000'), Path);
  except
    on E: ENetworkError do
      Raised := Pos('closed after 2000 of 4000 bytes', E.Message) > 0;
  end;
  AssertTrue('Short body raises ENetworkError', Raised);
  AssertFalse('No file created', FileExists(Path));
  AssertFalse('Partial file deleted', FileExists(Path + '.part'));
end;

procedure TRequestSimpleTests.Test41_GetToStreamRedirectAndPlainProc;
const
  ByteCount = 3000;
var
  R: TResponse;
  Stream: TMemoryStream;
  Raised: Boolean;
begin
  GPlainProgressCalls := 0;
  Stream := TMemoryStream.Create;
  try
    R := Http.GetToStream(TestURL('/redirect-to'), Stream,
      [KV('X-Test', '1')], [KV('url', '/bytes/' + IntToStr(ByteCount))],
      @PlainProgress);
    AssertEquals('Status after redirect', 200, R.StatusCode);
    AssertTrue('Only the final body is written',
      IsPatternBytes(StreamText(Stream), ByteCount));
    AssertTrue('Plain procedure progress', GPlainProgressCalls > 0);
    AssertEquals('Plain procedure saw all bytes', ByteCount, GPlainLastReceived);
  finally
    Stream.Free;
  end;

  Raised := False;
  try
    Http.GetToStream(TestURL('/get'), nil);
  except
    on E: ERequestError do
      Raised := True;
  end;
  AssertTrue('nil stream raises ERequestError', Raised);
end;

{ Streaming uploads }

procedure WritePatternFile(const Path: string; Count: Int64);
var
  FS: TFileStream;
  Chunk: array[0..65535] of Byte;
  I: Integer;
  N: Int64;
begin
  for I := 0 to High(Chunk) do
    Chunk[I] := I mod 251;
  FS := TFileStream.Create(Path, fmCreate);
  try
    while Count > 0 do
    begin
      N := Count;
      if N > SizeOf(Chunk) then
        N := SizeOf(Chunk);
      FS.WriteBuffer(Chunk, N);
      Dec(Count, N);
    end;
  finally
    FS.Free;
  end;
end;

procedure TRequestSimpleTests.Test42_MultipartStreamsLargeFile;
const
  FileSize = 8 * 1024 * 1024;
var
  Path, ContentType: string;
  Body: TStream;
  HeapBefore, HeapGrowth: PtrUInt;
  R: TResponse;
begin
  Path := GetTempDir + 'request_fp_upload_large.bin';
  WritePatternFile(Path, FileSize);
  try
    HeapBefore := GetFPCHeapStatus.CurrHeapUsed;
    Body := BuildMultipartBody([KV('note', 'big')], [KV('blob', Path)],
      ContentType);
    try
      HeapGrowth := GetFPCHeapStatus.CurrHeapUsed - HeapBefore;
      AssertTrue('File is not loaded into memory (grew ' +
        IntToStr(HeapGrowth) + ' bytes)', HeapGrowth < 1024 * 1024);
      AssertTrue('Body size includes the file', Body.Size > FileSize);
    finally
      Body.Free;
    end;

    R := Http.PostMultipart(TestURL('/upload-summary'),
      [KV('note', 'big')], [KV('blob', Path)]);
    AssertEquals('Status', 200, R.StatusCode);
    AssertEquals('Field', 'big', R.JSON.FindPath('form.note').AsString);
    AssertEquals('Uploaded size', FileSize,
      R.JSON.FindPath('files.blob.size').AsInt64);
    AssertEquals('Uploaded bytes match', MD5Print(MD5File(Path)),
      R.JSON.FindPath('files.blob.md5').AsString);
    AssertEquals('Sent with Content-Length, not chunked', '',
      R.JSON.FindPath('transfer_encoding').AsString);
  finally
    DeleteFile(Path);
  end;
end;

procedure TRequestSimpleTests.Test43_MultipartBodyRewindsAndReadsFully;
var
  PathA, PathB, ContentType: string;
  Body: TStream;
  First, Second: TMemoryStream;
  Text: RawByteString;
begin
  PathA := GetTempDir + 'request_fp_part_a.txt';
  PathB := GetTempDir + 'request_fp_part_empty.bin';
  WriteFileText(PathA, 'alpha');
  WriteFileText(PathB, '');
  First := TMemoryStream.Create;
  Second := TMemoryStream.Create;
  try
    Body := BuildMultipartBody([KV('x', '1')],
      [KV('a', PathA), KV('empty', PathB)], ContentType);
    try
      First.CopyFrom(Body, 0);
      Body.Position := 0;
      Second.CopyFrom(Body, 0);
      AssertEquals('Size matches bytes read', Body.Size, First.Size);
      AssertEquals('Rewound read is identical', First.Size, Second.Size);
      AssertTrue('Rewound bytes are identical',
        CompareMem(First.Memory, Second.Memory, First.Size));
    finally
      Body.Free;
    end;
    Text := StreamText(First);
    AssertTrue('File content present', Pos(#13#10#13#10'alpha'#13#10, Text) > 0);
    AssertTrue('Empty file part present',
      Pos('filename="request_fp_part_empty.bin"', Text) > 0);
    AssertTrue('Closing boundary last',
      Copy(Text, Length(Text) - 3, 4) = '--'#13#10);
    AssertTrue('Files are closed when the body is freed',
      DeleteFile(PathA) and DeleteFile(PathB));
  finally
    First.Free;
    Second.Free;
    DeleteFile(PathA);
    DeleteFile(PathB);
  end;
end;

{ Retries }

procedure TRequestSimpleTests.SetUp;
begin
  inherited SetUp;
  // Keep transport-failure tests fast; retry tests enable retries themselves.
  Http.SetRetry(0, 0);
end;

procedure TRequestSimpleTests.TearDown;
begin
  Http.SetRetry(DEFAULT_RETRY_COUNT, DEFAULT_RETRY_DELAY_MS);
  inherited TearDown;
end;

function TRequestSimpleTests.FlakyKey: string;
begin
  Inc(FFlakyCounter);
  Result := TestName + '-' + IntToStr(GetTickCount64) + '-' +
    IntToStr(FFlakyCounter);
end;

function FlakyURL(const Key, Query: string): string;
begin
  Result := TestURL('/flaky?key=' + Key + '&' + Query);
end;

function FlakyCount(const Key: string): Integer;
var
  R: TResponse;
begin
  R := Http.Get(TestURL('/flaky-count?key=' + Key));
  Result := R.JSON.FindPath('count').AsInteger;
end;

procedure TRequestSimpleTests.Test44_RetryOn503And429;
var
  R: TResponse;
  Key: string;
begin
  Http.SetRetry(2, 10);
  Key := FlakyKey;
  R := Http.Get(FlakyURL(Key, 'fail=2&status=503'));
  AssertEquals('Succeeds after two 503s', 200, R.StatusCode);
  AssertEquals('Third attempt', 3, R.JSON.FindPath('attempt').AsInteger);

  Key := FlakyKey;
  R := Http.Get(FlakyURL(Key, 'fail=1&status=429'));
  AssertEquals('429 is retried', 200, R.StatusCode);

  Key := FlakyKey;
  R := Http.Get(FlakyURL(Key, 'fail=1&status=500'));
  AssertEquals('500 is not retried', 500, R.StatusCode);
  AssertEquals('One attempt for 500', 1, FlakyCount(Key));
end;

procedure TRequestSimpleTests.Test45_RetryStopsAfterMax;
var
  R: TResponse;
  Key: string;
begin
  Http.SetRetry(2, 10);
  Key := FlakyKey;
  R := Http.Get(FlakyURL(Key, 'fail=5'));
  AssertEquals('Last response returned', 503, R.StatusCode);
  AssertEquals('One try plus two retries', 3, FlakyCount(Key));

  Http.SetRetry(0, 0);
  Key := FlakyKey;
  R := Http.Get(FlakyURL(Key, 'fail=1'));
  AssertEquals('SetRetry(0, 0) disables retries', 503, R.StatusCode);
  AssertEquals('Single attempt', 1, FlakyCount(Key));
end;

procedure TRequestSimpleTests.Test46_RetryAfterHonouredOrReturned;
var
  R: TResponse;
  Key: string;
  Start: QWord;
begin
  Http.SetRetry(2, 10);
  Key := FlakyKey;
  Start := GetTickCount64;
  R := Http.Get(FlakyURL(Key, 'fail=1&retry_after=1'));
  AssertEquals('Retried after Retry-After', 200, R.StatusCode);
  AssertTrue('Waited about one second', GetTickCount64 - Start >= 900);

  Key := FlakyKey;
  Start := GetTickCount64;
  R := Http.Get(FlakyURL(Key, 'fail=1&retry_after=3600'));
  AssertEquals('Long Retry-After is returned, not waited for', 503,
    R.StatusCode);
  AssertEquals('Retry-After visible to the caller', '3600',
    R.HeaderValue('Retry-After'));
  AssertEquals('No retry', 1, FlakyCount(Key));
  AssertTrue('Returned promptly', GetTickCount64 - Start < 2000);
end;

procedure TRequestSimpleTests.Test47_OnlyIdempotentMethodsRetry;
var
  R: TResponse;
  Key: string;
begin
  Http.SetRetry(2, 10);
  Key := FlakyKey;
  R := Http.Post(FlakyURL(Key, 'fail=1'), 'a=1');
  AssertEquals('POST not retried', 503, R.StatusCode);
  AssertEquals('POST single attempt', 1, FlakyCount(Key));

  Key := FlakyKey;
  R := Http.Patch(FlakyURL(Key, 'fail=1'), '{}');
  AssertEquals('PATCH not retried', 503, R.StatusCode);
  AssertEquals('PATCH single attempt', 1, FlakyCount(Key));

  Key := FlakyKey;
  R := Http.Put(FlakyURL(Key, 'fail=1'), 'a=1');
  AssertEquals('PUT retried', 200, R.StatusCode);

  Key := FlakyKey;
  R := Http.Delete(FlakyURL(Key, 'fail=1'));
  AssertEquals('DELETE retried', 200, R.StatusCode);

  Key := FlakyKey;
  R := Http.Head(FlakyURL(Key, 'fail=1'));
  AssertEquals('HEAD retried', 200, R.StatusCode);
end;

procedure TRequestSimpleTests.Test48_RetryOnDroppedConnection;
var
  R: TResponse;
  Key, Msg: string;
begin
  Key := FlakyKey;
  Msg := '';
  try
    Http.Get(FlakyURL(Key, 'fail=1&mode=drop'));
  except
    on E: ENetworkError do
      Msg := E.Message;
  end;
  AssertTrue('Empty response raises ENetworkError: ' + Msg,
    Pos('without a response', Msg) > 0);

  Http.SetRetry(2, 10);
  Key := FlakyKey;
  R := Http.Get(FlakyURL(Key, 'fail=1&mode=drop'));
  AssertEquals('Retried after a dropped connection', 200, R.StatusCode);
  AssertEquals('Second attempt', 2, R.JSON.FindPath('attempt').AsInteger);
end;

procedure TRequestSimpleTests.Test49_TimeoutIsNotRetried;
var
  Key: string;
  Raised: Boolean;
  OldTimeout: Integer;
begin
  Http.SetRetry(2, 10);
  OldTimeout := Http.GetTimeout;
  Http.SetTimeout(200);
  try
    Key := FlakyKey;
    Raised := False;
    try
      // A retry would succeed, so an ETimeoutError proves there was none.
      Http.Get(FlakyURL(Key, 'fail=1&delay=1'));
    except
      on E: ETimeoutError do
        Raised := True;
    end;
    AssertTrue('Timeout raised without retry', Raised);
  finally
    Http.SetTimeout(OldTimeout);
  end;
end;

procedure TRequestSimpleTests.Test50_RetryDefaultsAndDownload;
var
  R: TResponse;
  Key, Path: string;
  Start: QWord;
begin
  Http.SetRetry(DEFAULT_RETRY_COUNT, DEFAULT_RETRY_DELAY_MS);
  AssertEquals('Default retry count', 2, Http.GetRetryCount);
  AssertEquals('Default first delay', 250, Http.GetRetryDelay);

  Key := FlakyKey;
  Start := GetTickCount64;
  R := Http.Get(FlakyURL(Key, 'fail=1'));
  AssertEquals('Retries are on by default', 200, R.StatusCode);
  AssertTrue('Waited the first delay', GetTickCount64 - Start >= 230);

  Path := GetTempDir + 'request_fp_retry_download.json';
  Key := FlakyKey;
  try
    R := Http.Download(FlakyURL(Key, 'fail=1'), Path);
    AssertEquals('Download retried', 200, R.StatusCode);
    AssertTrue('Saved the successful body',
      Pos('"attempt": 2', ReadFileText(Path)) > 0);
  finally
    DeleteFile(Path);
  end;
end;

{ Helpers }

procedure TRequestSimpleTests.Test51_AuthHeaderHelpers;
var
  R: TResponse;
begin
  AssertEquals('Basic header name', 'Authorization', KVBasic('user', 'pass').Key);
  AssertEquals('Basic value', 'Basic dXNlcjpwYXNz', KVBasic('user', 'pass').Value);
  AssertEquals('Bearer value', 'Bearer abc', KVBearer('abc').Value);
  R := Http.Get(TestURL('/headers'), [KVBearer('abc')]);
  AssertEquals('Bearer sent', 'Bearer abc',
    R.JSON.FindPath('headers.Authorization').AsString);
end;

procedure TRequestSimpleTests.Test52_PutAndPatchJSON;
var
  R: TResponse;
  Obj: TJSONObject;
  Res: TRequestResult;
begin
  R := Http.PutJSON(TestURL('/put'), '{"a":1}');
  AssertEquals('PUT status', 200, R.StatusCode);
  AssertEquals('PUT JSON body', 1, R.JSON.FindPath('json.a').AsInteger);
  AssertEquals('PUT content type', 'application/json',
    R.JSON.FindPath('headers.Content-Type').AsString);

  Obj := TJSONObject.Create(['b', 2]);
  try
    R := Http.PatchJSON(TestURL('/patch'), Obj);
  finally
    Obj.Free;
  end;
  AssertEquals('PATCH JSON body', 2, R.JSON.FindPath('json.b').AsInteger);

  R := Http.PutJSON(TestURL('/put'), '{}',
    [KV('Content-Type', 'application/merge-patch+json')]);
  AssertEquals('Custom content type wins', 'application/merge-patch+json',
    R.JSON.FindPath('headers.Content-Type').AsString);

  Res := Http.TryPatchJSON(TestURL('/patch'), TJSONData(nil));
  AssertFalse('nil JSON is reported', Res.Success);
  AssertTrue('nil JSON message', Pos('cannot be nil', Res.Error) > 0);

  Res := Http.TryPutJSON(TestFailureURL + '/put', '{}');
  AssertFalse('Transport failure reported', Res.Success);
end;

procedure TRequestSimpleTests.Test53_HeaderValuesAndNames;
var
  R: TResponse;
  Values, Names: TStringArray;
  I, SetCookieCount: Integer;
begin
  R := Http.Get(TestURL('/set-cookie?a=1&b=2'));
  Values := R.HeaderValues('set-cookie');
  AssertEquals('Both Set-Cookie values', 2, Length(Values));
  AssertEquals('First value in order', 'a=1; Path=/', Values[0]);
  AssertEquals('Second value in order', 'b=2; Path=/', Values[1]);
  AssertEquals('Missing header gives no values', 0,
    Length(R.HeaderValues('X-Missing')));

  Names := R.HeaderNames;
  SetCookieCount := 0;
  for I := 0 to High(Names) do
    if SameText(Names[I], 'Set-Cookie') then
      Inc(SetCookieCount);
  AssertEquals('Names have no duplicates', 1, SetCookieCount);
  AssertTrue('Content-Type listed',
    (Length(Names) > 1) and (R.HeaderValue('Content-Type') <> ''));
end;

procedure TRequestSimpleTests.Test54_ConnectTimeoutSetting;
var
  Raised: Boolean;
begin
  AssertEquals('Default connect timeout', 3000, Http.GetConnectTimeout);
  Http.SetConnectTimeout(-5);
  AssertEquals('Negative becomes 0', 0, Http.GetConnectTimeout);
  Http.SetConnectTimeout(1500);
  try
    AssertEquals('Value kept', 1500, Http.GetConnectTimeout);
    Raised := False;
    try
      Http.Get(TestFailureURL);
    except
      on E: ENetworkError do
        Raised := True;
    end;
    AssertTrue('Failure still raises ENetworkError', Raised);
  finally
    Http.SetConnectTimeout(3000);
  end;
end;

procedure TRequestSimpleTests.Test55_ProxyForHTTP;
var
  R: TResponse;
  Res: TRequestResult;
  Host: string;
  Port: Integer;
  Msg: string;
begin
  if not LocalFixtureHostPort(Host, Port) then
    Exit; // The local test server doubles as the proxy.
  Http.SetProxy(Host, Port, 'proxyuser', 'proxypass');
  try
    R := Http.Get('http://example.invalid/proxy-echo');
    AssertEquals('Status via proxy', 200, R.StatusCode);
    AssertEquals('Absolute URL sent to the proxy',
      'http://example.invalid/proxy-echo', R.JSON.FindPath('target').AsString);
    AssertEquals('Host header is the target', 'example.invalid',
      R.JSON.FindPath('host').AsString);
    AssertEquals('Proxy credentials', KVBasic('proxyuser', 'proxypass').Value,
      R.JSON.FindPath('proxy_authorization').AsString);

    Msg := '';
    try
      Http.Get('https://example.invalid/');
    except
      on E: ERequestError do
        Msg := E.Message;
    end;
    AssertTrue('HTTPS through a proxy is refused clearly: ' + Msg,
      Pos('HTTPS through a proxy is not supported', Msg) > 0);

    Res := Http.TryGet('https://example.invalid/');
    AssertFalse('TryGet reports it', Res.Success);
  finally
    Http.SetProxy('', 0);
  end;
  R := Http.Get(TestURL('/get'));
  AssertEquals('Proxy removed', 200, R.StatusCode);
end;

procedure TRequestSimpleTests.Test56_CrossHostRedirectDropsAuth;
var
  R: TResponse;
  OtherHost: string;
begin
  R := Http.Get(TestURL('/redirect-to'), [KVBearer('secret')],
    [KV('url', '/headers')]);
  AssertEquals('Same-host redirect keeps Authorization', 'Bearer secret',
    R.JSON.FindPath('headers.Authorization').AsString);

  if Pos('127.0.0.1', TestBaseURL) = 0 then
    Exit; // Needs the local server, reachable as both 127.0.0.1 and localhost.
  OtherHost := StringReplace(TestBaseURL, '127.0.0.1', 'localhost', []) +
    '/headers';
  R := Http.Get(TestURL('/redirect-to'), [KVBearer('secret')],
    [KV('url', OtherHost)]);
  AssertEquals('Final status', 200, R.StatusCode);
  AssertTrue('Authorization not sent to another host',
    R.JSON.FindPath('headers.Authorization') = nil);
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
