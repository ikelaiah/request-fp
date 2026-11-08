unit Request.Session;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson, jsonparser,
  Request;

type


  { TSimpleMap }
  {
    @description Simple key-value map for string pairs, used for headers and cookies.

    @usage Use for storing and retrieving HTTP headers or cookies in a session.

    @example

      var
        Map: TSimpleMap;

      Map.SetItem('Key', 'Value');
      if Map.ContainsKey('Key') then ...
      Map.Remove('Key');

    @note No manual memory management required; dynamic array is managed by Pascal.
  }
  TSimpleMap = record
  private
    FItems: array of TKeyValue;
    function FindIndex(const Key: string): Integer;
  public
    { Adds or updates a key-value pair (alias for SetItem) }
    procedure Add(const Key, Value: string);
    { Removes all items }
    procedure Clear;
    { Returns True if the key exists }
    function ContainsKey(const Key: string): Boolean;
    { Gets the value for a key, or returns Default if not found }
    function Get(const Key: string; const Default: string = ''): string;
    { Returns the number of items }
    function GetCount: Integer;
    { Gets the key at the given index }
    function GetKey(Index: Integer): string;
    { Gets the value at the given index }
    function GetValue(Index: Integer): string;
    { Returns the index of a key, or -1 if not found }
    function IndexOf(const Key: string): Integer;
    { Removes a key-value pair by key }
    procedure Remove(const Key: string);
    { Adds or updates a key-value pair }
    procedure SetItem(const Key, Value: string);
  end;

  { THttpSession }
  {
    @description HTTP session with connection reuse, shared headers, and cookies.

    @usage Use for multiple requests that share a base URL, headers, or cookies.
           Every method returns the response for any HTTP status and raises
           ERequestError (or ENetworkError/ETimeoutError) for transport failures.
           Try* methods never raise.

    @example

      var
        Session: THttpSession;

      Session.SetBaseURL('https://api.example.com');
      Session.SetHeader('Authorization', 'Bearer ...');
      Response := Session.Get('/endpoint');
      WriteLn('Status: ', Response.StatusCode);

    @note Advanced record: uses Initialize, Finalize, and Copy for safe resource management.
          No manual cleanup needed; memory is managed automatically.
  }
  THttpSession = record
  private
    FClient: TFPHTTPClient;
    FHeaders: TSimpleMap;
    FCookies: TSimpleMap;
    FBaseURL: string;
    FUserAgent: string;
    FTimeout: Integer;
    FRetryCount: Integer;
    FRetryDelay: Integer;

    function GetClient: TFPHTTPClient;
    function GetFullURL(const URL: string): string;
    procedure SetupRequest(Client: TFPHTTPClient);
    { Target nil keeps the body in memory; otherwise a 2xx body is streamed
      into Target. }
    function DoRequest(const Method, URL: string; Body: TStream;
      const ContentType: string; const Headers: array of TKeyValue;
      const Params: array of TKeyValue; Target: TStream = nil;
      OnProgress: TProgressProc = nil;
      OnProgressEvent: TProgressEvent = nil): TResponse;
    function DoStreamGet(const URL: string; Target: TStream;
      OnProgress: TProgressProc; OnProgressEvent: TProgressEvent): TResponse;
    function DoDownload(const URL, FilePath: string;
      OnProgress: TProgressProc; OnProgressEvent: TProgressEvent): TResponse;
    function DoTextRequest(const Method, URL, Body: string; HasBody: Boolean;
      const ContentType: string; const Headers: array of TKeyValue;
      const Params: array of TKeyValue): TResponse;

  public
    { Advanced record management: see Initialize, Finalize, and Copy operators below. }
    class operator Initialize(var Session: THttpSession);
    class operator Finalize(var Session: THttpSession);
    class operator Copy(constref Source: THttpSession; var Dest: THttpSession);

    { Resets the session to its defaults. Initialization is automatic. }
    procedure Init;

    { Performs a HTTP GET request }
    function Get(const URL: string): TResponse; overload;
    { GET with extra headers for this request only }
    function Get(const URL: string; const Headers: array of TKeyValue): TResponse; overload;
    { GET with URL-encoded query parameters }
    function GetWithParams(const URL: string; const Params: array of TKeyValue): TResponse;
    { Performs a HTTP POST request }
    function Post(const URL: string; const Body: string = '';
                 const ContentType: string = 'application/x-www-form-urlencoded'): TResponse;
    { Posts a JSON string or TJSONData value with the correct content type. }
    function PostJSON(const URL: string; const JSON: string): TResponse; overload;
    function PostJSON(const URL: string; const JSON: TJSONData): TResponse; overload;
    { Posts Fields as application/x-www-form-urlencoded. }
    function PostForm(const URL: string; const Fields: array of TKeyValue): TResponse;
    { Posts multipart/form-data. Files values are file paths. }
    function PostMultipart(const URL: string; const Fields, Files: array of TKeyValue): TResponse;
    { Performs a HTTP PUT request }
    function Put(const URL: string; const Body: string = '';
                const ContentType: string = 'application/json'): TResponse;
    { Performs a HTTP PATCH request }
    function Patch(const URL: string; const Body: string = '';
                  const ContentType: string = 'application/json'): TResponse;
    { Performs a HTTP DELETE request }
    function Delete(const URL: string): TResponse;
    { Performs a HTTP HEAD request; Text is empty }
    function Head(const URL: string): TResponse;
    { Sends any request. Headers apply to this request only and override
      session headers with the same name. Use '' for no body. }
    function Send(const Method, URL: string; const Body: string;
      const ContentType: string; const Headers: array of TKeyValue;
      const Params: array of TKeyValue): TResponse;

    { Exception-free variants. Success is False only for transport failures. }
    function TryGet(const URL: string): TRequestResult;
    function TryPost(const URL: string; const Body: string = '';
      const ContentType: string = 'application/x-www-form-urlencoded'): TRequestResult;
    function TryPostJSON(const URL: string; const JSON: string): TRequestResult;
    function TryPostForm(const URL: string; const Fields: array of TKeyValue): TRequestResult;
    function TryPostMultipart(const URL: string; const Fields, Files: array of TKeyValue): TRequestResult;
    function TryPut(const URL: string; const Body: string = '';
      const ContentType: string = 'application/json'): TRequestResult;
    function TryPatch(const URL: string; const Body: string = '';
      const ContentType: string = 'application/json'): TRequestResult;
    function TryDelete(const URL: string): TRequestResult;
    function TryHead(const URL: string): TRequestResult;
    function TrySend(const Method, URL: string; const Body: string;
      const ContentType: string; const Headers: array of TKeyValue;
      const Params: array of TKeyValue): TRequestResult;

    { Streams a GET response to FilePath with session headers and cookies.
      Same rules as THttp.Download: FilePath is replaced only on a 2xx. }
    function Download(const URL, FilePath: string): TResponse; overload;
    function Download(const URL, FilePath: string; OnProgress: TProgressProc): TResponse; overload;
    function Download(const URL, FilePath: string; OnProgress: TProgressEvent): TResponse; overload;
    function TryDownload(const URL, FilePath: string): TRequestResult; overload;
    function TryDownload(const URL, FilePath: string; OnProgress: TProgressProc): TRequestResult; overload;
    function TryDownload(const URL, FilePath: string; OnProgress: TProgressEvent): TRequestResult; overload;
    { Streams a GET response into Stream; see THttp.GetToStream. }
    function GetToStream(const URL: string; Stream: TStream): TResponse; overload;
    function GetToStream(const URL: string; Stream: TStream; OnProgress: TProgressProc): TResponse; overload;
    function GetToStream(const URL: string; Stream: TStream; OnProgress: TProgressEvent): TResponse; overload;
    function TryGetToStream(const URL: string; Stream: TStream): TRequestResult; overload;
    function TryGetToStream(const URL: string; Stream: TStream; OnProgress: TProgressProc): TRequestResult; overload;
    function TryGetToStream(const URL: string; Stream: TStream; OnProgress: TProgressEvent): TRequestResult; overload;

    { Sets a header for all requests }
    procedure SetHeader(const Name, Value: string);
    { Sets a cookie for all requests }
    procedure SetCookie(const Name, Value: string);
    { Sets the base URL for relative requests }
    procedure SetBaseURL(const URL: string);
    { Sets the User-Agent header }
    procedure SetUserAgent(const UserAgent: string);
    { Sets the timeout in milliseconds }
    procedure SetTimeout(Timeout: Integer);
    { Retries for this session; same rules as THttp.SetRetry (default 2
      retries, 250 ms first delay). SetRetry(0, 0) disables. }
    procedure SetRetry(MaxRetries, BaseDelayMS: Integer);

    { Clears all cookies }
    procedure ClearCookies;
    { Gets a cookie value by name }
    function GetCookie(const Name: string): string;

    { Clears all headers (except Accept) }
    procedure ClearHeaders;
  end;

implementation

uses
  DateUtils, URIParser;

type
  PSimpleMap = ^TSimpleMap;

  { FPC client that skips the empty "Cookie:" header it would otherwise send
    when its own cookie list is empty. The session manages cookies itself. }
  TSessionHTTPClient = class(TFPHTTPClient)
  protected
    function AllowHeader(var AHeader: string): Boolean; override;
  end;

  { Keeps session cookies correct across redirects: applies Set-Cookie from
    each redirect response, resends the merged cookies on same-host hops,
    and drops them when the redirect leaves the host. }
  TRedirectCookieHandler = class
  public
    Cookies: PSimpleMap;
    procedure HandleRedirect(Sender: TObject; const ASrc: string;
      var ADest: string);
  end;

procedure ApplySetCookie(var Cookies: TSimpleMap; const HeaderValue: string); forward;
procedure ApplySetCookieHeaders(var Cookies: TSimpleMap; Headers: TStrings); forward;
function BuildCookieHeader(const Cookies: TSimpleMap): string; forward;

function TSessionHTTPClient.AllowHeader(var AHeader: string): Boolean;
var
  P: Integer;
begin
  P := Pos(':', AHeader);
  if (P > 0) and SameText(Trim(Copy(AHeader, 1, P - 1)), 'Cookie') and
     (Trim(Copy(AHeader, P + 1, MaxInt)) = '') then
    Exit(False);
  Result := inherited AllowHeader(AHeader);
end;

procedure TRedirectCookieHandler.HandleRedirect(Sender: TObject;
  const ASrc: string; var ADest: string);
var
  Client: TFPHTTPClient;
  DestHost, CookieStr: string;
  Idx: Integer;
begin
  Client := TFPHTTPClient(Sender);
  ApplySetCookieHeaders(Cookies^, Client.ResponseHeaders);
  // FPC would resend only the redirect response's cookies; send ours instead.
  Client.Cookies.Clear;

  Idx := Client.RequestHeaders.IndexOfName('Cookie');
  if Idx >= 0 then
    Client.RequestHeaders.Delete(Idx);

  DestHost := ParseURI(ADest).Host;
  CookieStr := BuildCookieHeader(Cookies^);
  if (CookieStr <> '') and
     ((DestHost = '') or SameText(DestHost, ParseURI(ASrc).Host)) then
    Client.AddHeader('Cookie', CookieStr);
end;

procedure SetTryFailure(var R: TRequestResult; E: Exception);
begin
  R.Success := False;
  R.Error := E.Message;
  R.Response.SetContent('');
  R.Response.SetHeadersText('');
  R.Response.StatusCode := 0;
end;

{ TSimpleMap }

function TSimpleMap.FindIndex(const Key: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FItems) do
  begin
    if SameText(FItems[I].Key, Key) then // Case-insensitive comparison
    begin
      Result := I;
      Exit;
    end;
  end;
end;

procedure TSimpleMap.Add(const Key, Value: string);
begin
  SetItem(Key, Value);
end;

procedure TSimpleMap.Clear;
begin
  SetLength(FItems, 0);
end;

function TSimpleMap.ContainsKey(const Key: string): Boolean;
begin
  Result := FindIndex(Key) >= 0;
end;

function TSimpleMap.Get(const Key: string; const Default: string): string;
var
  Index: Integer;
begin
  Index := FindIndex(Key);
  if Index >= 0 then
    Result := FItems[Index].Value
  else
    Result := Default;
end;

function TSimpleMap.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TSimpleMap.GetKey(Index: Integer): string;
begin
  if (Index >= 0) and (Index < Length(FItems)) then
    Result := FItems[Index].Key
  else
    Result := '';
end;

function TSimpleMap.GetValue(Index: Integer): string;
begin
  if (Index >= 0) and (Index < Length(FItems)) then
    Result := FItems[Index].Value
  else
    Result := '';
end;

function TSimpleMap.IndexOf(const Key: string): Integer;
begin
  Result := FindIndex(Key);
end;

procedure TSimpleMap.Remove(const Key: string);
var
  Index, I: Integer;
begin
  Index := FindIndex(Key);
  if Index >= 0 then
  begin
    // Remove item by shifting remaining items left
    for I := Index to Length(FItems) - 2 do
      FItems[I] := FItems[I + 1];
    SetLength(FItems, Length(FItems) - 1);
  end;
end;

procedure TSimpleMap.SetItem(const Key, Value: string);
var
  Index: Integer;
begin
  Index := FindIndex(Key);
  if Index >= 0 then
  begin
    // Update existing item
    FItems[Index].Value := Value;
  end
  else
  begin
    // Add new item
    SetLength(FItems, Length(FItems) + 1);
    FItems[High(FItems)].Key := Key;
    FItems[High(FItems)].Value := Value;
  end;
end;

{ THttpSession }

class operator THttpSession.Initialize(var Session: THttpSession);
begin
  Session.FClient := nil;
  Session.FBaseURL := '';
  Session.FUserAgent := DEFAULT_USER_AGENT;
  Session.FTimeout := 30000; // 30 seconds default timeout
  Session.FRetryCount := DEFAULT_RETRY_COUNT;
  Session.FRetryDelay := DEFAULT_RETRY_DELAY_MS;

  // Set default headers - TSimpleMap doesn't need explicit initialization
  Session.FHeaders.SetItem('Accept', 'application/json');
  // Do NOT set Content-Type here; set per-request
end;

class operator THttpSession.Finalize(var Session: THttpSession);
begin
  if Assigned(Session.FClient) then
    FreeAndNil(Session.FClient);
  // TSimpleMap uses dynamic arrays and doesn't need manual cleanup
end;

class operator THttpSession.Copy(constref Source: THttpSession; var Dest: THttpSession);
var
  I: Integer;
begin
  // Skip if source and destination are the same
  if @Source = @Dest then
    Exit;

  // Copy simple fields
  Dest.FBaseURL := Source.FBaseURL;
  Dest.FUserAgent := Source.FUserAgent;
  Dest.FTimeout := Source.FTimeout;
  Dest.FRetryCount := Source.FRetryCount;
  Dest.FRetryDelay := Source.FRetryDelay;

  // Copy headers
  Dest.FHeaders.Clear;
  for I := 0 to Source.FHeaders.GetCount - 1 do
    Dest.FHeaders.SetItem(Source.FHeaders.GetKey(I), Source.FHeaders.GetValue(I));

  // Copy cookies
  Dest.FCookies.Clear;
  for I := 0 to Source.FCookies.GetCount - 1 do
    Dest.FCookies.SetItem(Source.FCookies.GetKey(I), Source.FCookies.GetValue(I));

  // Don't copy the client - it will be created on demand
  if Dest.FClient <> nil then
    FreeAndNil(Dest.FClient);
  Dest.FClient := nil;
end;

function THttpSession.GetClient: TFPHTTPClient;
begin
  // Defensive programming - initialize fields that might be accessed
  if Self.FTimeout <= 0 then
    Self.FTimeout := 30000;

  if Self.FUserAgent = '' then
    Self.FUserAgent := DEFAULT_USER_AGENT;

  // Create client if needed
  if Self.FClient = nil then
  begin
    Self.FClient := TSessionHTTPClient.Create(nil);
    Self.FClient.AllowRedirect := True;
    Self.FClient.IOTimeout := Self.FTimeout;
  end;
  Result := Self.FClient;
end;

function THttpSession.GetFullURL(const URL: string): string;
begin
  if (Pos('http://', LowerCase(URL)) = 1) or (Pos('https://', LowerCase(URL)) = 1) then
    Result := URL
  else if FBaseURL = '' then
    Result := URL
  else if URL = '' then
    Result := FBaseURL
  else if (FBaseURL[Length(FBaseURL)] = '/') and
          (URL[1] = '/') then
    Result := FBaseURL + Copy(URL, 2, MaxInt)
  else if (FBaseURL[Length(FBaseURL)] <> '/') and
          (URL[1] <> '/') then
    Result := FBaseURL + '/' + URL
  else
    Result := FBaseURL + URL;
end;

procedure THttpSession.SetupRequest(Client: TFPHTTPClient);
var
  I: Integer;
  CookieStr: string;
begin
  // Clear any existing headers to prevent conflicts
  Client.RequestHeaders.Clear;
  // The session tracks cookies itself; drop the cookies FPC kept from the
  // previous response so cleared or expired cookies are not resent.
  Client.Cookies.Clear;

  // Set headers
  for I := 0 to FHeaders.GetCount - 1 do
    Client.AddHeader(FHeaders.GetKey(I), FHeaders.GetValue(I));

  // Handle cookies - AddHeader will override any existing header with the same name
  CookieStr := BuildCookieHeader(FCookies);
  if CookieStr <> '' then
    Client.AddHeader('Cookie', CookieStr);

  // Set timeout
  Client.IOTimeout := FTimeout;

  // Ensure User-Agent is present (session default unless overridden)
  if Client.RequestHeaders.IndexOfName('User-Agent') = -1 then
    Client.AddHeader('User-Agent', FUserAgent);
end;

function BuildCookieHeader(const Cookies: TSimpleMap): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Cookies.GetCount - 1 do
  begin
    if I > 0 then
      Result := Result + '; ';
    Result := Result + Cookies.GetKey(I) + '=' + Cookies.GetValue(I);
  end;
end;

procedure ApplySetCookie(var Cookies: TSimpleMap; const HeaderValue: string);
var
  Parts: TStringArray;
  Pair, AttrName, AttrValue: string;
  Name, Value: string;
  P, I, MaxAge: Integer;
  Expires: TDateTime;
  Expired: Boolean;
begin
  Parts := HeaderValue.Split([';']);
  if Length(Parts) = 0 then
    Exit;

  Pair := Trim(Parts[0]);
  P := Pos('=', Pair);
  if P <= 1 then
    Exit;
  Name := Trim(Copy(Pair, 1, P - 1));
  Value := Trim(Copy(Pair, P + 1, MaxInt));

  // Max-Age wins over Expires (RFC 6265).
  Expired := False;
  for I := 1 to High(Parts) do
  begin
    Pair := Trim(Parts[I]);
    P := Pos('=', Pair);
    if P = 0 then
      Continue;
    AttrName := Trim(Copy(Pair, 1, P - 1));
    AttrValue := Trim(Copy(Pair, P + 1, MaxInt));
    if SameText(AttrName, 'Max-Age') and TryStrToInt(AttrValue, MaxAge) then
    begin
      Expired := MaxAge <= 0;
      Break;
    end;
    if SameText(AttrName, 'Expires') and
       TryParseHTTPDate(AttrValue, Expires) then
      Expired := Expires <= LocalTimeToUniversal(Now);
  end;

  if Expired then
    Cookies.Remove(Name)
  else
    Cookies.SetItem(Name, Value);
end;

procedure ApplySetCookieHeaders(var Cookies: TSimpleMap; Headers: TStrings);
var
  I: Integer;
begin
  for I := 0 to Headers.Count - 1 do
    if SameText(Trim(Headers.Names[I]), 'Set-Cookie') then
      ApplySetCookie(Cookies, Trim(Headers.ValueFromIndex[I]));
end;

function THttpSession.DoRequest(const Method, URL: string; Body: TStream;
  const ContentType: string; const Headers: array of TKeyValue;
  const Params: array of TKeyValue; Target: TStream;
  OnProgress: TProgressProc; OnProgressEvent: TProgressEvent): TResponse;
var
  Client: TFPHTTPClient;
  RedirectHandler: TRedirectCookieHandler;
  I: Integer;
begin
  Client := GetClient;
  SetupRequest(Client);
  if ContentType <> '' then
    Client.AddHeader('Content-Type', ContentType);
  for I := 0 to High(Headers) do
    Client.AddHeader(Headers[I].Key, Headers[I].Value);

  RedirectHandler := TRedirectCookieHandler.Create;
  try
    RedirectHandler.Cookies := @FCookies;
    Client.OnRedirect := @RedirectHandler.HandleRedirect;
    try
      if Assigned(Target) then
        Result := ExecuteRequest(Client, Method,
          AppendQueryParams(GetFullURL(URL), Params), Body, Target,
          OnProgress, OnProgressEvent, FRetryCount, FRetryDelay)
      else
        Result := ExecuteRequest(Client, Method,
          AppendQueryParams(GetFullURL(URL), Params), Body, FRetryCount,
          FRetryDelay);
    finally
      Client.OnRedirect := nil;
    end;
    ApplySetCookieHeaders(FCookies, Client.ResponseHeaders);
  finally
    RedirectHandler.Free;
  end;
end;

function THttpSession.DoStreamGet(const URL: string; Target: TStream;
  OnProgress: TProgressProc; OnProgressEvent: TProgressEvent): TResponse;
begin
  if Target = nil then
    raise ERequestError.Create('Stream cannot be nil');
  Result := DoRequest('GET', URL, nil, '', [], [], Target, OnProgress,
    OnProgressEvent);
end;

function THttpSession.DoDownload(const URL, FilePath: string;
  OnProgress: TProgressProc; OnProgressEvent: TProgressEvent): TResponse;
var
  PartFile: TDownloadFile;
begin
  PartFile := TDownloadFile.Create(FilePath);
  try
    Result := DoStreamGet(URL, PartFile.Stream, OnProgress, OnProgressEvent);
    if Result.OK then
      PartFile.Commit;
  finally
    PartFile.Free;
  end;
end;

function THttpSession.DoTextRequest(const Method, URL, Body: string;
  HasBody: Boolean; const ContentType: string;
  const Headers: array of TKeyValue;
  const Params: array of TKeyValue): TResponse;
var
  Stream: TStringStream;
begin
  if HasBody then
    Stream := TStringStream.Create(Body)
  else
    Stream := nil;
  try
    if HasBody then
      Result := DoRequest(Method, URL, Stream, ContentType, Headers, Params)
    else
      Result := DoRequest(Method, URL, nil, '', Headers, Params);
  finally
    Stream.Free;
  end;
end;

function THttpSession.Get(const URL: string): TResponse;
begin
  Result := DoTextRequest('GET', URL, '', False, '', [], []);
end;

function THttpSession.Get(const URL: string;
  const Headers: array of TKeyValue): TResponse;
begin
  Result := DoTextRequest('GET', URL, '', False, '', Headers, []);
end;

function THttpSession.GetWithParams(const URL: string;
  const Params: array of TKeyValue): TResponse;
begin
  Result := DoTextRequest('GET', URL, '', False, '', [], Params);
end;

function THttpSession.Post(const URL: string; const Body: string; const ContentType: string): TResponse;
begin
  Result := DoTextRequest('POST', URL, Body, True, ContentType, [], []);
end;

function THttpSession.PostJSON(const URL: string; const JSON: string): TResponse;
begin
  Result := Post(URL, JSON, 'application/json');
end;

function THttpSession.PostJSON(const URL: string;
  const JSON: TJSONData): TResponse;
begin
  if JSON = nil then
    raise ERequestError.Create('JSON data cannot be nil');
  Result := PostJSON(URL, JSON.AsJSON);
end;

function THttpSession.PostForm(const URL: string;
  const Fields: array of TKeyValue): TResponse;
begin
  Result := Post(URL, EncodeFormFields(Fields),
    'application/x-www-form-urlencoded');
end;

function THttpSession.PostMultipart(const URL: string;
  const Fields, Files: array of TKeyValue): TResponse;
var
  Body: TStream;
  ContentType: string;
begin
  Body := BuildMultipartBody(Fields, Files, ContentType);
  try
    Result := DoRequest('POST', URL, Body, ContentType, [], []);
  finally
    Body.Free;
  end;
end;

function THttpSession.Put(const URL: string; const Body: string;
                        const ContentType: string): TResponse;
begin
  Result := DoTextRequest('PUT', URL, Body, True, ContentType, [], []);
end;

function THttpSession.Patch(const URL: string; const Body: string;
                          const ContentType: string): TResponse;
begin
  Result := DoTextRequest('PATCH', URL, Body, True, ContentType, [], []);
end;

function THttpSession.Delete(const URL: string): TResponse;
begin
  Result := DoTextRequest('DELETE', URL, '', False, '', [], []);
end;

function THttpSession.Head(const URL: string): TResponse;
begin
  Result := DoTextRequest('HEAD', URL, '', False, '', [], []);
end;

function THttpSession.Send(const Method, URL: string; const Body: string;
  const ContentType: string; const Headers: array of TKeyValue;
  const Params: array of TKeyValue): TResponse;
begin
  Result := DoTextRequest(UpperCase(Method), URL, Body, Body <> '',
    ContentType, Headers, Params);
end;

// Exception-free variants

function THttpSession.TryGet(const URL: string): TRequestResult;
begin
  try
    Result.Response := Get(URL);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function THttpSession.TryPost(const URL: string; const Body: string;
  const ContentType: string): TRequestResult;
begin
  try
    Result.Response := Post(URL, Body, ContentType);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function THttpSession.TryPostJSON(const URL: string;
  const JSON: string): TRequestResult;
begin
  try
    Result.Response := PostJSON(URL, JSON);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function THttpSession.TryPostForm(const URL: string;
  const Fields: array of TKeyValue): TRequestResult;
begin
  try
    Result.Response := PostForm(URL, Fields);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function THttpSession.TryPostMultipart(const URL: string;
  const Fields, Files: array of TKeyValue): TRequestResult;
begin
  try
    Result.Response := PostMultipart(URL, Fields, Files);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function THttpSession.TryPut(const URL: string; const Body: string;
  const ContentType: string): TRequestResult;
begin
  try
    Result.Response := Put(URL, Body, ContentType);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function THttpSession.TryPatch(const URL: string; const Body: string;
  const ContentType: string): TRequestResult;
begin
  try
    Result.Response := Patch(URL, Body, ContentType);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function THttpSession.TryDelete(const URL: string): TRequestResult;
begin
  try
    Result.Response := Delete(URL);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function THttpSession.TryHead(const URL: string): TRequestResult;
begin
  try
    Result.Response := Head(URL);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function THttpSession.TrySend(const Method, URL: string; const Body: string;
  const ContentType: string; const Headers: array of TKeyValue;
  const Params: array of TKeyValue): TRequestResult;
begin
  try
    Result.Response := Send(Method, URL, Body, ContentType, Headers, Params);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

// Streaming downloads

function THttpSession.Download(const URL, FilePath: string): TResponse;
begin
  Result := DoDownload(URL, FilePath, nil, nil);
end;

function THttpSession.Download(const URL, FilePath: string;
  OnProgress: TProgressProc): TResponse;
begin
  Result := DoDownload(URL, FilePath, OnProgress, nil);
end;

function THttpSession.Download(const URL, FilePath: string;
  OnProgress: TProgressEvent): TResponse;
begin
  Result := DoDownload(URL, FilePath, nil, OnProgress);
end;

function THttpSession.TryDownload(const URL, FilePath: string): TRequestResult;
begin
  Result := TryDownload(URL, FilePath, TProgressProc(nil));
end;

function THttpSession.TryDownload(const URL, FilePath: string;
  OnProgress: TProgressProc): TRequestResult;
begin
  try
    Result.Response := DoDownload(URL, FilePath, OnProgress, nil);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function THttpSession.TryDownload(const URL, FilePath: string;
  OnProgress: TProgressEvent): TRequestResult;
begin
  try
    Result.Response := DoDownload(URL, FilePath, nil, OnProgress);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function THttpSession.GetToStream(const URL: string; Stream: TStream): TResponse;
begin
  Result := DoStreamGet(URL, Stream, nil, nil);
end;

function THttpSession.GetToStream(const URL: string; Stream: TStream;
  OnProgress: TProgressProc): TResponse;
begin
  Result := DoStreamGet(URL, Stream, OnProgress, nil);
end;

function THttpSession.GetToStream(const URL: string; Stream: TStream;
  OnProgress: TProgressEvent): TResponse;
begin
  Result := DoStreamGet(URL, Stream, nil, OnProgress);
end;

function THttpSession.TryGetToStream(const URL: string;
  Stream: TStream): TRequestResult;
begin
  Result := TryGetToStream(URL, Stream, TProgressProc(nil));
end;

function THttpSession.TryGetToStream(const URL: string; Stream: TStream;
  OnProgress: TProgressProc): TRequestResult;
begin
  try
    Result.Response := DoStreamGet(URL, Stream, OnProgress, nil);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function THttpSession.TryGetToStream(const URL: string; Stream: TStream;
  OnProgress: TProgressEvent): TRequestResult;
begin
  try
    Result.Response := DoStreamGet(URL, Stream, nil, OnProgress);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

// Session Configuration

procedure THttpSession.SetHeader(const Name, Value: string);
begin
  FHeaders.SetItem(Name, Value);
end;

procedure THttpSession.SetCookie(const Name, Value: string);
begin
  FCookies.SetItem(Name, Value);
end;

procedure THttpSession.SetBaseURL(const URL: string);
begin
  FBaseURL := URL;
end;

procedure THttpSession.SetUserAgent(const UserAgent: string);
begin
  FUserAgent := UserAgent;
end;

procedure THttpSession.SetTimeout(Timeout: Integer);
begin
  FTimeout := Timeout;
  if FClient <> nil then
    FClient.IOTimeout := Timeout;
end;

// Cookie Management

procedure THttpSession.SetRetry(MaxRetries, BaseDelayMS: Integer);
begin
  if MaxRetries < 0 then
    MaxRetries := 0;
  if BaseDelayMS < 0 then
    BaseDelayMS := 0;
  FRetryCount := MaxRetries;
  FRetryDelay := BaseDelayMS;
end;

procedure THttpSession.ClearCookies;
var
  idx: Integer;
begin
  FCookies.Clear;
  if Assigned(FClient) then
  begin
    FClient.Cookies.Clear;
    if Assigned(FClient.RequestHeaders) then
    begin
      idx := FClient.RequestHeaders.IndexOfName('Cookie');
      if idx >= 0 then
      begin
        FClient.RequestHeaders.Delete(idx);
      end;
    end;
  end;
end;

function THttpSession.GetCookie(const Name: string): string;
begin
  Result := FCookies.Get(Name);
end;

// Header Management

procedure THttpSession.ClearHeaders;
begin
  FHeaders.Clear;
  FHeaders.SetItem('Accept', 'application/json');
  // Do NOT set Content-Type here; set per-request
end;

procedure THttpSession.Init;
begin
  // Init is now an optional reset method. Local variables and fields are
  // initialized automatically by the advanced-record Initialize operator.
  if Assigned(Self.FClient) then
    FreeAndNil(Self.FClient);
  Self.FHeaders.Clear;
  Self.FCookies.Clear;
  Self.FClient := nil;
  Self.FBaseURL := '';
  Self.FUserAgent := DEFAULT_USER_AGENT;
  Self.FTimeout := 30000; // 30 seconds default timeout
  Self.FRetryCount := DEFAULT_RETRY_COUNT;
  Self.FRetryDelay := DEFAULT_RETRY_DELAY_MS;

  // Set default headers
  Self.FHeaders.SetItem('Accept', 'application/json');
end;

end.
