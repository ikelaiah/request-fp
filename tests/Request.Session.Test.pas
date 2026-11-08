unit Request.Session.Test;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Sockets, fpcunit, testregistry, fpjson, jsonparser,
  Request, Request.Session, Test.Support;

type
  { TDelayedResponseServer }
  TDelayedResponseServer = class(TThread)
  private
    FListenSocket: LongInt;
    FPort: Word;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Port: Word read FPort;
  end;

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
    procedure Test05_SingleThreadedReuse;
    
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
    procedure Test26_Session_ResponseHeaderValue;
    procedure Test27_Session_PostJSON;
    procedure Test28_Session_NonSuccessStatusReturned;
    procedure Test29_Session_TransportErrorRaisesRequestError;
    procedure Test30_Session_PatchHeadSend;
    procedure Test31_Session_TryVariants;
    procedure Test32_Session_PostFormAndMultipart;
    procedure Test33_Session_ServerCookiesAndExpiry;
    procedure Test34_Session_RedirectKeepsCookies;
    procedure Test35_Session_CrossHostRedirectDropsCookies;
    procedure Test36_Session_DownloadUsesCookiesAndHeaders;
    procedure Test37_Session_GetToStreamAndTryVariants;
    procedure Test38_Session_MultipartStreamsFile;
    procedure Test39_Session_Retry;
    procedure Test40_Session_AuthAndJSONHelpers;
    procedure Test41_Session_Proxy;
  end;

implementation

{ TDelayedResponseServer }

constructor TDelayedResponseServer.Create;
var
  Address: TInetSockAddr;
  AddressLength: TSockLen;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FListenSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if FListenSocket < 0 then
    raise Exception.CreateFmt(
      'Could not create timeout test socket (error %d)', [SocketError]);

  FillChar(Address, SizeOf(Address), 0);
  Address.sin_family := AF_INET;
  Address.sin_addr := StrToNetAddr('127.0.0.1');
  Address.sin_port := htons(0);

  if fpBind(FListenSocket, @Address, SizeOf(Address)) <> 0 then
  begin
    CloseSocket(FListenSocket);
    FListenSocket := -1;
    raise Exception.CreateFmt(
      'Could not bind timeout test socket (error %d)', [SocketError]);
  end;

  if fpListen(FListenSocket, 1) <> 0 then
  begin
    CloseSocket(FListenSocket);
    FListenSocket := -1;
    raise Exception.CreateFmt(
      'Could not listen on timeout test socket (error %d)', [SocketError]);
  end;

  AddressLength := SizeOf(Address);
  if fpGetSockName(FListenSocket, @Address, @AddressLength) <> 0 then
  begin
    CloseSocket(FListenSocket);
    FListenSocket := -1;
    raise Exception.CreateFmt(
      'Could not read timeout test port (error %d)', [SocketError]);
  end;

  FPort := ntohs(Address.sin_port);
  Start;
end;

destructor TDelayedResponseServer.Destroy;
var
  Address: TInetSockAddr;
  WakeSocket: LongInt;
begin
  Terminate;

  // Wake a thread that is still blocked in accept.
  if FListenSocket >= 0 then
  begin
    WakeSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
    if WakeSocket >= 0 then
    begin
      FillChar(Address, SizeOf(Address), 0);
      Address.sin_family := AF_INET;
      Address.sin_addr := StrToNetAddr('127.0.0.1');
      Address.sin_port := htons(FPort);
      fpConnect(WakeSocket, @Address, SizeOf(Address));
      CloseSocket(WakeSocket);
    end;
  end;

  WaitFor;
  inherited Destroy;
end;

procedure TDelayedResponseServer.Execute;
var
  Buffer: array[0..1023] of Byte;
  ClientSocket: LongInt;
  I: Integer;
begin
  ClientSocket := -1;
  try
    ClientSocket := fpAccept(FListenSocket, nil, nil);
    if ClientSocket < 0 then
      Exit;

    // Read the request, then deliberately withhold the response for two
    // seconds. The client timeout should fire well before the socket closes.
    fpRecv(ClientSocket, @Buffer[0], SizeOf(Buffer), 0);
    for I := 1 to 200 do
    begin
      if Terminated then
        Exit;
      Sleep(10);
    end;
  finally
    if ClientSocket >= 0 then
    begin
      fpShutdown(ClientSocket, SHUT_RDWR);
      CloseSocket(ClientSocket);
    end;
    if FListenSocket >= 0 then
    begin
      CloseSocket(FListenSocket);
      FListenSocket := -1;
    end;
  end;
end;

{ TRequestSessionTests }

procedure TRequestSessionTests.SetUp;
begin
  inherited SetUp;
  // THttpSession is initialized automatically; Init is only needed to reset it.
  FSession.SetBaseURL(TestBaseURL);
  FSession.SetHeader('Accept', 'application/json');
  FSession.SetHeader('X-Test-Header', 'test-value');
  // Keep transport-failure tests fast; retry tests enable retries themselves.
  FSession.SetRetry(0, 0);
end;

procedure TRequestSessionTests.Test27_Session_PostJSON;
var
  Response: TResponse;
  Body, Echoed: TJSONObject;
begin
  Body := TJSONObject.Create;
  try
    Body.Add('name', 'Ada');
    Response := FSession.PostJSON('/post', Body);
  finally
    Body.Free;
  end;

  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  Echoed := TJSONObject(Response.JSON.FindPath('json'));
  AssertTrue('JSON body should be echoed', Echoed <> nil);
  AssertEquals('JSON value should match', 'Ada', Echoed.Get('name', ''));
end;

procedure TRequestSessionTests.Test28_Session_NonSuccessStatusReturned;
var
  Response: TResponse;
begin
  // Sessions return the response for any HTTP status, like the Http API.
  Response := FSession.Get('/status/404');
  AssertEquals('GET 404 should be returned', 404, Response.StatusCode);
  AssertFalse('GET 404 should not be OK', Response.OK);

  Response := FSession.Get('/status/204');
  AssertEquals('GET 204 should be returned', 204, Response.StatusCode);
  AssertTrue('GET 204 should be OK', Response.OK);

  Response := FSession.Post('/status/201', 'a=1');
  AssertEquals('POST 201 should be returned', 201, Response.StatusCode);

  Response := FSession.Post('/status/500', 'a=1');
  AssertEquals('POST 500 should be returned', 500, Response.StatusCode);

  Response := FSession.PostJSON('/status/422', '{"a":1}');
  AssertEquals('PostJSON 422 should be returned', 422, Response.StatusCode);

  Response := FSession.Put('/status/409', '{"a":1}');
  AssertEquals('PUT 409 should be returned', 409, Response.StatusCode);

  Response := FSession.Delete('/status/202');
  AssertEquals('DELETE 202 should be returned', 202, Response.StatusCode);

  Response := FSession.Delete('/status/404');
  AssertEquals('DELETE 404 should be returned', 404, Response.StatusCode);
end;

procedure TRequestSessionTests.Test29_Session_TransportErrorRaisesRequestError;

  procedure ExpectRequestError(const Method: string);
  var
    Raised: Boolean;
  begin
    Raised := False;
    try
      if Method = 'GET' then
        FSession.Get('/get')
      else if Method = 'POST' then
        FSession.Post('/post', 'a=1')
      else if Method = 'PUT' then
        FSession.Put('/put', '{}')
      else
        FSession.Delete('/delete');
    except
      on E: ERequestError do
        Raised := True;
    end;
    AssertTrue(Method + ' connection failure should raise ERequestError',
      Raised);
  end;

begin
  FSession.SetBaseURL(TestFailureURL);
  ExpectRequestError('GET');
  ExpectRequestError('POST');
  ExpectRequestError('PUT');
  ExpectRequestError('DELETE');
end;

procedure TRequestSessionTests.Test30_Session_PatchHeadSend;
var
  Response: TResponse;
begin
  Response := FSession.Patch('/patch', '{"name":"Ada"}');
  AssertEquals('PATCH status', 200, Response.StatusCode);
  AssertEquals('PATCH JSON echoed', 'Ada',
    Response.JSON.FindPath('json.name').AsString);

  Response := FSession.Head('/get');
  AssertEquals('HEAD status', 200, Response.StatusCode);
  AssertEquals('HEAD has no body', '', Response.Text);
  AssertEquals('HEAD headers returned', 'yes',
    Response.HeaderValue('X-Fixture-Head'));

  Response := FSession.Send('get', '/get', '', '',
    [KV('X-Test-Header', 'per-request'), KV('X-One', '1')], [KV('q', 'a b')]);
  AssertEquals('Send status', 200, Response.StatusCode);
  AssertEquals('Per-request header sent', '1',
    Response.JSON.FindPath('headers.X-One').AsString);
  AssertEquals('Per-request header overrides session header', 'per-request',
    Response.JSON.FindPath('headers.X-Test-Header').AsString);
  AssertEquals('Query param sent', 'a b',
    Response.JSON.FindPath('args.q').AsString);

  Response := FSession.GetWithParams('/get', [KV('page', '2')]);
  AssertEquals('GetWithParams param sent', '2',
    Response.JSON.FindPath('args.page').AsString);

  Response := FSession.Get('/get');
  AssertEquals('Session header restored after Send', 'test-value',
    Response.JSON.FindPath('headers.X-Test-Header').AsString);
end;

procedure TRequestSessionTests.Test31_Session_TryVariants;
var
  Res: TRequestResult;
begin
  Res := FSession.TryGet('/status/500');
  AssertTrue('TryGet transport succeeded', Res.Success);
  AssertEquals('TryGet status', 500, Res.Response.StatusCode);
  AssertFalse('TryGet 500 is not OK', Res.OK);

  Res := FSession.TryPostJSON('/post', '{"a":1}');
  AssertTrue('TryPostJSON OK', Res.OK);

  Res := FSession.TryHead('/get');
  AssertTrue('TryHead OK', Res.OK);

  FSession.SetBaseURL(TestFailureURL);
  Res := FSession.TryGet('/get');
  AssertFalse('TryGet failure reported', Res.Success);
  AssertTrue('TryGet error message set', Res.Error <> '');
  AssertEquals('TryGet failure status', 0, Res.Response.StatusCode);

  Res := FSession.TryDelete('/delete');
  AssertFalse('TryDelete failure reported', Res.Success);
end;

procedure TRequestSessionTests.Test32_Session_PostFormAndMultipart;
var
  Response: TResponse;
  TempFile: string;
  FS: TFileStream;
const
  Content = 'session upload';
begin
  Response := FSession.PostForm('/post', [KV('user', 'ada lovelace')]);
  AssertEquals('PostForm status', 200, Response.StatusCode);
  AssertEquals('PostForm field echoed', 'ada lovelace',
    Response.JSON.FindPath('form.user').AsString);

  TempFile := GetTempDir + 'request_fp_session_upload.json';
  FS := TFileStream.Create(TempFile, fmCreate);
  try
    FS.WriteBuffer(Content[1], Length(Content));
  finally
    FS.Free;
  end;
  try
    Response := FSession.PostMultipart('/post', [KV('kind', 'doc')],
      [KV('file', TempFile)]);
    AssertEquals('PostMultipart status', 200, Response.StatusCode);
    AssertEquals('Multipart field echoed', 'doc',
      Response.JSON.FindPath('form.kind').AsString);
    AssertEquals('Multipart file echoed', Content,
      Response.JSON.FindPath('files.file').AsString);
    AssertEquals('Multipart file type', 'application/json',
      Response.JSON.FindPath('file_types.file').AsString);
  finally
    DeleteFile(TempFile);
  end;
end;

procedure TRequestSessionTests.Test33_Session_ServerCookiesAndExpiry;
var
  Response: TResponse;
begin
  FSession.Get('/set-cookie?sid=abc&theme=dark');
  AssertEquals('Server cookie stored', 'abc', FSession.GetCookie('sid'));
  AssertEquals('Second server cookie stored', 'dark',
    FSession.GetCookie('theme'));

  FSession.Get('/expire-cookie?name=sid&mode=max-age');
  AssertEquals('Max-Age=0 removes cookie', '', FSession.GetCookie('sid'));
  AssertEquals('Other cookie kept', 'dark', FSession.GetCookie('theme'));

  FSession.Get('/expire-cookie?name=theme&mode=expires');
  AssertEquals('Past Expires removes cookie', '', FSession.GetCookie('theme'));

  // Cleared cookies must not be resent by FPC's own cookie list either.
  FSession.Get('/set-cookie?temp=1');
  FSession.ClearCookies;
  Response := FSession.Get('/cookies');
  AssertEquals('No cookies after ClearCookies', 0,
    TJSONObject(Response.JSON.FindPath('cookies')).Count);
end;

procedure TRequestSessionTests.Test34_Session_RedirectKeepsCookies;
var
  Response: TResponse;
  Cookies: TJSONObject;
begin
  FSession.SetCookie('existing', 'yes');
  Response := FSession.Get(
    '/redirect-set-cookie?name=login&value=ok&to=/cookies');
  AssertEquals('Final status', 200, Response.StatusCode);
  Cookies := TJSONObject(Response.JSON.FindPath('cookies'));
  AssertEquals('Redirect cookie sent on next hop', 'ok',
    Cookies.Get('login', ''));
  AssertEquals('Existing cookie still sent', 'yes',
    Cookies.Get('existing', ''));
  AssertEquals('Redirect cookie stored in session', 'ok',
    FSession.GetCookie('login'));
end;

procedure TRequestSessionTests.Test35_Session_CrossHostRedirectDropsCookies;
var
  Response: TResponse;
  OtherHostURL: string;
begin
  if Pos('127.0.0.1', TestBaseURL) = 0 then
    Exit; // Needs the local fixture, reachable as both 127.0.0.1 and localhost.
  OtherHostURL := StringReplace(TestBaseURL, '127.0.0.1', 'localhost', []) +
    '/cookies';
  FSession.SetCookie('secret', 'token');
  Response := FSession.Get('/redirect-to?url=' + OtherHostURL);
  AssertEquals('Final status', 200, Response.StatusCode);
  AssertEquals('Cookies not sent to another host', 0,
    TJSONObject(Response.JSON.FindPath('cookies')).Count);
  AssertEquals('Session keeps its cookie', 'token',
    FSession.GetCookie('secret'));
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
    AssertTrue('URL should contain the requested path', Pos('/get', URL) > 0);
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
  FSession.SetBaseURL(TestBaseURL + '/');
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

procedure TRequestSessionTests.Test05_SingleThreadedReuse;
var
  Response: TResponse;
  Headers: TJSONObject;
begin

  WriteLn('Test05_SingleThreadedReuse: Starting');

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

  WriteLn('Test05_SingleThreadedReuse: Completed');
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
  Server: TDelayedResponseServer;
  ExceptionRaised: Boolean;
  StartedAt, ElapsedMS: QWord;
  OldTimeout: Integer;
begin

WriteLn('Test22_TimeoutHandling: Starting');

  OldTimeout := 30000;
  ElapsedMS := 0;
  Server := TDelayedResponseServer.Create;
  try
    FSession.SetBaseURL(
      'http://127.0.0.1:' + IntToStr(Server.Port));
    FSession.SetTimeout(100);

    ExceptionRaised := False;
    StartedAt := GetTickCount64;
    try
      Response := FSession.Get('/');
      Fail('Timeout exception was not raised');
    except
      on E: ERequestError do
      begin
        ExceptionRaised := E is ETimeoutError;
        ElapsedMS := GetTickCount64 - StartedAt;
      end;
    end;

    AssertTrue('ETimeoutError should be raised', ExceptionRaised);
    AssertTrue(
      Format(
        'Timeout should occur before the server closes the connection (%dms)',
        [ElapsedMS]),
      ElapsedMS < 1500);
  finally
    FSession.SetTimeout(OldTimeout);
    Server.Free;
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
      // Retry once when a developer uses the public fallback service.
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
  Boundary, Body, CRLF: string;
  ExceptionRaised: Boolean;
begin
  // Create a minimal multipart body
  Boundary := '----RequestFPTest' + IntToStr(Random(1000000));
  CRLF := #13#10;
  Body := '--' + Boundary + CRLF +
          'Content-Disposition: form-data; name="a"' + CRLF + CRLF +
          '1' + CRLF +
          '--' + Boundary + '--' + CRLF;

  // Point to a local port that should reject the connection.
  FSession.SetBaseURL(TestFailureURL);

  ExceptionRaised := False;
  try
    FSession.Post('/post', Body, 'multipart/form-data; boundary=' + Boundary);
  except
    on E: Exception do
      ExceptionRaised := True;
  end;
  AssertTrue('POST should raise exception on connection failure',
    ExceptionRaised);

  // No explicit restore needed; each test re-initializes the session in SetUp
end;

procedure TRequestSessionTests.Test26_Session_ResponseHeaderValue;
var
  Response: TResponse;
  CT: string;
begin
  Response := FSession.Get('/get');
  // Retry once when a developer uses the public fallback service.
  if Response.StatusCode = 502 then
    Response := FSession.Get('/get');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  CT := Response.HeaderValue('Content-Type');
  AssertTrue('Content-Type header should be present', CT <> '');
  AssertTrue('Content-Type should indicate JSON', Pos('application/json', LowerCase(CT)) > 0);
end;

procedure TRequestSessionTests.Test36_Session_DownloadUsesCookiesAndHeaders;
var
  R: TResponse;
  Path, Saved: string;
  FS: TFileStream;
  Data: TJSONData;
begin
  Path := GetTempDir + 'request_fp_session_download.json';
  DeleteFile(Path);
  FSession.SetCookie('token', 'abc');
  try
    R := FSession.Download('/cookies', Path);
    AssertEquals('Status', 200, R.StatusCode);
    AssertEquals('Body is not kept in memory', '', R.Text);
    FS := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(Saved, FS.Size);
      if FS.Size > 0 then
        FS.ReadBuffer(Saved[1], FS.Size);
    finally
      FS.Free;
    end;
    Data := GetJSON(Saved);
    try
      AssertEquals('Session cookie was sent', 'abc',
        Data.FindPath('cookies.token').AsString);
    finally
      Data.Free;
    end;

    R := FSession.Download('/set-cookie?streamed=yes', Path);
    AssertEquals('Set-Cookie applied after a download', 'yes',
      FSession.GetCookie('streamed'));
  finally
    DeleteFile(Path);
  end;
end;

procedure TRequestSessionTests.Test37_Session_GetToStreamAndTryVariants;
var
  R: TResponse;
  Res: TRequestResult;
  Stream: TMemoryStream;
  Path: string;
begin
  Stream := TMemoryStream.Create;
  try
    R := FSession.GetToStream('/bytes/1024', Stream);
    AssertEquals('Status', 200, R.StatusCode);
    AssertEquals('Stream size', 1024, Stream.Size);

    Res := FSession.TryGetToStream('/status/404', Stream);
    AssertTrue('Transport succeeded', Res.Success);
    AssertEquals('Status', 404, Res.Response.StatusCode);
    AssertEquals('Error body not written to stream', 1024, Stream.Size);
  finally
    Stream.Free;
  end;

  Path := GetTempDir + 'request_fp_session_fail.bin';
  FSession.SetBaseURL(TestFailureURL);
  Res := FSession.TryDownload('/bytes/10', Path);
  AssertFalse('Connection failure reported', Res.Success);
  AssertFalse('No file created', FileExists(Path));
  AssertFalse('No partial file left', FileExists(Path + '.part'));
end;

procedure TRequestSessionTests.Test38_Session_MultipartStreamsFile;
const
  FileSize = 1024 * 1024 + 7;
var
  Path: string;
  FS: TFileStream;
  Data: RawByteString;
  I: Integer;
  R: TResponse;
begin
  Path := GetTempDir + 'request_fp_session_upload.bin';
  SetLength(Data, FileSize);
  for I := 1 to FileSize do
    Data[I] := Chr(I mod 256);
  FS := TFileStream.Create(Path, fmCreate);
  try
    FS.WriteBuffer(Data[1], FileSize);
  finally
    FS.Free;
  end;
  try
    R := FSession.PostMultipart('/upload-summary', [KV('kind', 'bin')],
      [KV('file', Path)]);
    AssertEquals('Status', 200, R.StatusCode);
    AssertEquals('Uploaded size', FileSize,
      R.JSON.FindPath('files.file.size').AsInt64);
    AssertEquals('File name', 'request_fp_session_upload.bin',
      R.JSON.FindPath('files.file.filename').AsString);
  finally
    DeleteFile(Path);
  end;
end;

procedure TRequestSessionTests.Test39_Session_Retry;
var
  R: TResponse;
  Fresh: THttpSession;
  Key: string;
begin
  Key := 'session-retry-' + IntToStr(GetTickCount64);
  FSession.SetRetry(2, 10);
  R := FSession.Get('/flaky?key=' + Key + '&fail=1');
  AssertEquals('Session GET retried', 200, R.StatusCode);

  R := FSession.Post('/flaky?key=' + Key + '-post&fail=1', 'a=1');
  AssertEquals('Session POST not retried', 503, R.StatusCode);

  FSession.SetRetry(0, 0);
  R := FSession.Get('/flaky?key=' + Key + '-off&fail=1');
  AssertEquals('Session retries disabled', 503, R.StatusCode);

  Fresh.SetBaseURL(TestBaseURL);
  R := Fresh.Get('/flaky?key=' + Key + '-fresh&fail=1');
  AssertEquals('New sessions retry by default', 200, R.StatusCode);
end;

procedure TRequestSessionTests.Test40_Session_AuthAndJSONHelpers;
var
  R: TResponse;
  Obj: TJSONObject;
  OtherHost: string;
begin
  FSession.SetBearerToken('abc');
  R := FSession.Get('/headers');
  AssertEquals('Bearer sent', 'Bearer abc',
    R.JSON.FindPath('headers.Authorization').AsString);

  FSession.SetBasicAuth('user', 'pass');
  R := FSession.Get('/headers');
  AssertEquals('Basic replaces bearer', 'Basic dXNlcjpwYXNz',
    R.JSON.FindPath('headers.Authorization').AsString);

  R := FSession.PutJSON('/put', '{"x":2}');
  AssertEquals('PUT JSON', 2, R.JSON.FindPath('json.x').AsInteger);
  AssertEquals('PUT content type', 'application/json',
    R.JSON.FindPath('headers.Content-Type').AsString);
  Obj := TJSONObject.Create(['y', 3]);
  try
    R := FSession.PatchJSON('/patch', Obj);
  finally
    Obj.Free;
  end;
  AssertEquals('PATCH JSON', 3, R.JSON.FindPath('json.y').AsInteger);
  AssertTrue('TryPatchJSON', FSession.TryPatchJSON('/patch', '{}').OK);

  if Pos('127.0.0.1', TestBaseURL) = 0 then
    Exit;
  OtherHost := StringReplace(TestBaseURL, '127.0.0.1', 'localhost', []) +
    '/headers';
  R := FSession.Get('/redirect-to?url=' + OtherHost);
  AssertTrue('Authorization not sent to another host',
    R.JSON.FindPath('headers.Authorization') = nil);
  R := FSession.Get('/headers');
  AssertEquals('Session keeps its credentials', 'Basic dXNlcjpwYXNz',
    R.JSON.FindPath('headers.Authorization').AsString);
end;

procedure TRequestSessionTests.Test41_Session_Proxy;
var
  R: TResponse;
  Host, Msg: string;
  Port: Integer;
begin
  if not LocalFixtureHostPort(Host, Port) then
    Exit;
  FSession.SetConnectTimeout(2000);
  FSession.SetProxy(Host, Port);
  R := FSession.Get('http://example.invalid/proxy-echo');
  AssertEquals('Absolute URL sent to the proxy',
    'http://example.invalid/proxy-echo', R.JSON.FindPath('target').AsString);
  AssertEquals('No proxy credentials', '',
    R.JSON.FindPath('proxy_authorization').AsString);

  Msg := '';
  try
    FSession.Get('https://example.invalid/');
  except
    on E: ERequestError do
      Msg := E.Message;
  end;
  AssertTrue('HTTPS through a proxy is refused clearly',
    Pos('not supported', Msg) > 0);

  FSession.SetProxy('', 0);
  R := FSession.Get('/get');
  AssertEquals('Proxy removed', 200, R.StatusCode);
end;

initialization
  RegisterTest(TRequestSessionTests);
end.
