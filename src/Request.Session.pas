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

  { One stored cookie. Domain is lower case, without a leading dot. }
  TSessionCookie = record
    Name: string;
    Value: string;
    Domain: string;
    Path: string;
    HostOnly: Boolean;  // no Domain attribute: only the exact host
    Secure: Boolean;    // only sent over https
    Global: Boolean;    // set with SetCookie: sent to every host called directly
    Expires: TDateTime; // UTC; 0 = until the session ends
  end;

  { Session cookie store with RFC 6265 Domain, Path, Secure, and expiry
    matching. Used by THttpSession; applications rarely need it directly. }
  TCookieJar = record
  private
    FItems: array of TSessionCookie;
    function IndexOf(const Name, Domain, Path: string; Global: Boolean): Integer;
    procedure RemoveName(const Name: string; GlobalOnly: Boolean);
    procedure RemoveExpired;
  public
    procedure Clear;
    procedure Assign(const Source: TCookieJar);
    { Stores a cookie sent to every host (SetCookie). }
    procedure SetGlobal(const Name, Value: string);
    { Stores, replaces, or removes a cookie from a Set-Cookie header value
      received in a response from URL. Invalid Domain attributes are ignored. }
    procedure Apply(const SetCookieValue, URL: string);
    { Cookie header value for a request to URL. }
    function HeaderFor(const URL: string; IncludeGlobal: Boolean): string;
    { Value of the first cookie named Name, or ''. }
    function Get(const Name: string): string;
    function Count: Integer;
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
    FCookies: TCookieJar;
    FBaseURL: string;
    FUserAgent: string;
    FTimeout: Integer;
    FRetryCount: Integer;
    FRetryDelay: Integer;
    FConnectTimeout: Integer;
    FProxyHost: string;
    FProxyPort: Integer;
    FProxyUser: string;
    FProxyPassword: string;

    function GetClient: TFPHTTPClient;
    function GetFullURL(const URL: string): string;
    procedure SetupRequest(Client: TFPHTTPClient; const FullURL: string);
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
    { PUT and PATCH JSON with Content-Type: application/json. }
    function PutJSON(const URL: string; const JSON: string): TResponse; overload;
    function PutJSON(const URL: string; const JSON: TJSONData): TResponse; overload;
    function PatchJSON(const URL: string; const JSON: string): TResponse; overload;
    function PatchJSON(const URL: string; const JSON: TJSONData): TResponse; overload;
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
    function TryPutJSON(const URL: string; const JSON: string): TRequestResult;
    function TryPatchJSON(const URL: string; const JSON: string): TRequestResult;
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
    { Sets a cookie sent with every request, whatever the host or path.
      Cookies from servers follow their Domain and Path instead. }
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
    { Connect timeout in milliseconds (default 3000; 0 = system default). }
    procedure SetConnectTimeout(Timeout: Integer);
    { HTTP proxy for http:// requests; see THttp.SetProxy. SetProxy('', 0)
      removes it. }
    procedure SetProxy(const Host: string; Port: Integer;
      const UserName: string = ''; const Password: string = '');
    { Sends Authorization: Basic for every request. }
    procedure SetBasicAuth(const UserName, Password: string);
    { Sends Authorization: Bearer for every request. }
    procedure SetBearerToken(const Token: string);

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
  PCookieJar = ^TCookieJar;

  { FPC client that skips the empty "Cookie:" header it would otherwise send
    when its own cookie list is empty. The session manages cookies itself. }
  TSessionHTTPClient = class(TFPHTTPClient)
  protected
    function AllowHeader(var AHeader: string): Boolean; override;
  end;

  { Keeps session cookies correct across redirects: applies Set-Cookie from
    each redirect response and sends the cookies that match the next URL.
    Global cookies (SetCookie) are not sent when a redirect leaves the host. }
  TRedirectCookieHandler = class
  public
    Cookies: PCookieJar;
    { The URL being requested; updated on every redirect. }
    CurrentURL: string;
    procedure HandleRedirect(Sender: TObject; const ASrc: string;
      var ADest: string);
  end;

procedure ApplySetCookieHeaders(var Cookies: TCookieJar; Headers: TStrings;
  const URL: string); forward;

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
  NextURL, CookieStr: string;
  Idx: Integer;
begin
  Client := TFPHTTPClient(Sender);
  ApplySetCookieHeaders(Cookies^, Client.ResponseHeaders, ASrc);
  DropAuthOnHostChange(Client, ASrc, ADest);
  // FPC would resend only the redirect response's cookies; send ours instead.
  Client.Cookies.Clear;

  Idx := Client.RequestHeaders.IndexOfName('Cookie');
  if Idx >= 0 then
    Client.RequestHeaders.Delete(Idx);

  NextURL := ADest;
  if not IsAbsoluteURI(NextURL) then
    ResolveRelativeURI(ASrc, ADest, NextURL);
  CurrentURL := NextURL;
  CookieStr := Cookies^.HeaderFor(NextURL,
    SameText(ParseURI(NextURL).Host, ParseURI(ASrc).Host));
  if CookieStr <> '' then
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
  Session.FConnectTimeout := 3000;
  Session.FProxyHost := '';
  Session.FProxyPort := 0;
  Session.FProxyUser := '';
  Session.FProxyPassword := '';

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
  Dest.FConnectTimeout := Source.FConnectTimeout;
  Dest.FProxyHost := Source.FProxyHost;
  Dest.FProxyPort := Source.FProxyPort;
  Dest.FProxyUser := Source.FProxyUser;
  Dest.FProxyPassword := Source.FProxyPassword;

  // Copy headers
  Dest.FHeaders.Clear;
  for I := 0 to Source.FHeaders.GetCount - 1 do
    Dest.FHeaders.SetItem(Source.FHeaders.GetKey(I), Source.FHeaders.GetValue(I));

  // Copy cookies (a separate copy, not shared)
  Dest.FCookies.Assign(Source.FCookies);

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

procedure THttpSession.SetupRequest(Client: TFPHTTPClient;
  const FullURL: string);
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

  // Cookies that match this URL; AddHeader overrides a Cookie set by SetHeader.
  CookieStr := FCookies.HeaderFor(FullURL, True);
  if CookieStr <> '' then
    Client.AddHeader('Cookie', CookieStr);

  // Set timeout
  Client.IOTimeout := FTimeout;
  Client.ConnectTimeout := FConnectTimeout;
  Client.Proxy.Host := FProxyHost;
  Client.Proxy.Port := FProxyPort;
  Client.Proxy.UserName := FProxyUser;
  Client.Proxy.Password := FProxyPassword;

  // Ensure User-Agent is present (session default unless overridden)
  if Client.RequestHeaders.IndexOfName('User-Agent') = -1 then
    Client.AddHeader('User-Agent', FUserAgent);
end;

{ TCookieJar }

function IsIPAddress(const Host: string): Boolean;
var
  I: Integer;
begin
  Result := (Host <> '') and (Pos(':', Host) > 0);
  if Result then
    Exit;
  for I := 1 to Length(Host) do
    if not (Host[I] in ['0'..'9', '.']) then
      Exit(False);
  Result := Host <> '';
end;

{ RFC 6265 5.1.3: Host is Domain, or a subdomain of it (never for IPs). }
function DomainMatches(const Host, Domain: string): Boolean;
begin
  Result := (Host = Domain) or
    (not IsIPAddress(Host) and (Length(Host) > Length(Domain)) and
     (Copy(Host, Length(Host) - Length(Domain), MaxInt) = '.' + Domain));
end;

{ RFC 6265 5.1.4. }
function PathMatches(const RequestPath, CookiePath: string): Boolean;
begin
  Result := (RequestPath = CookiePath) or
    ((Pos(CookiePath, RequestPath) = 1) and
     ((CookiePath[Length(CookiePath)] = '/') or
      (RequestPath[Length(CookiePath) + 1] = '/')));
end;

function URLPath(const URI: TURI): string;
begin
  Result := URI.Path + URI.Document;
  if Result = '' then
    Result := '/'
  else if Result[1] <> '/' then
    Result := '/' + Result;
end;

{ RFC 6265 5.1.4 default-path: the request path up to its last '/'. }
function DefaultCookiePath(const URI: TURI): string;
var
  P: Integer;
begin
  Result := URLPath(URI);
  P := Length(Result);
  while (P > 1) and (Result[P] <> '/') do
    Dec(P);
  if P <= 1 then
    Result := '/'
  else
    Result := Copy(Result, 1, P - 1);
end;

function TCookieJar.IndexOf(const Name, Domain, Path: string;
  Global: Boolean): Integer;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    if (FItems[I].Name = Name) and (FItems[I].Global = Global) and
       (Global or ((FItems[I].Domain = Domain) and (FItems[I].Path = Path))) then
      Exit(I);
  Result := -1;
end;

procedure TCookieJar.RemoveName(const Name: string; GlobalOnly: Boolean);
var
  I: Integer;
begin
  for I := High(FItems) downto 0 do
    if (FItems[I].Name = Name) and (FItems[I].Global or not GlobalOnly) then
      Delete(FItems, I, 1);
end;

procedure TCookieJar.RemoveExpired;
var
  I: Integer;
  NowUTC: TDateTime;
begin
  NowUTC := LocalTimeToUniversal(Now);
  for I := High(FItems) downto 0 do
    if (FItems[I].Expires <> 0) and (FItems[I].Expires <= NowUTC) then
      Delete(FItems, I, 1);
end;

procedure TCookieJar.Clear;
begin
  FItems := nil;
end;

procedure TCookieJar.Assign(const Source: TCookieJar);
begin
  FItems := Copy(Source.FItems);
end;

procedure TCookieJar.SetGlobal(const Name, Value: string);
var
  C: TSessionCookie;
begin
  // Replaces any cookie of that name, as SetCookie always has.
  RemoveName(Name, False);
  C := Default(TSessionCookie);
  C.Name := Name;
  C.Value := Value;
  C.Path := '/';
  C.Global := True;
  Insert(C, FItems, Length(FItems));
end;

procedure TCookieJar.Apply(const SetCookieValue, URL: string);
var
  URI: TURI;
  Parts: TStringArray;
  Pair, AttrName, AttrValue, Host: string;
  C: TSessionCookie;
  P, I, Idx: Integer;
  MaxAge: Int64;
  HasMaxAge, Expired: Boolean;
  ExpiresAt: TDateTime;
begin
  Parts := SetCookieValue.Split([';']);
  if Length(Parts) = 0 then
    Exit;
  Pair := Trim(Parts[0]);
  P := Pos('=', Pair);
  if P <= 1 then
    Exit;

  URI := ParseURI(URL);
  Host := LowerCase(URI.Host);
  C := Default(TSessionCookie);
  C.Name := Trim(Copy(Pair, 1, P - 1));
  C.Value := Trim(Copy(Pair, P + 1, MaxInt));
  C.Domain := Host;
  C.HostOnly := True;
  C.Path := DefaultCookiePath(URI);
  HasMaxAge := False;
  Expired := False;

  for I := 1 to High(Parts) do
  begin
    Pair := Trim(Parts[I]);
    P := Pos('=', Pair);
    if P = 0 then
    begin
      if SameText(Pair, 'Secure') then
        C.Secure := True;
      Continue;
    end;
    AttrName := Trim(Copy(Pair, 1, P - 1));
    AttrValue := Trim(Copy(Pair, P + 1, MaxInt));
    if SameText(AttrName, 'Domain') then
    begin
      AttrValue := LowerCase(AttrValue);
      if (AttrValue <> '') and (AttrValue[1] = '.') then
        Delete(AttrValue, 1, 1);
      if AttrValue = '' then
        Continue;
      // Reject a domain the host is not part of, or a bare top-level name.
      if not DomainMatches(Host, AttrValue) or
         ((Pos('.', AttrValue) = 0) and (AttrValue <> Host)) then
        Exit;
      C.Domain := AttrValue;
      C.HostOnly := AttrValue = Host;
    end
    else if SameText(AttrName, 'Path') then
    begin
      if (AttrValue <> '') and (AttrValue[1] = '/') then
        C.Path := AttrValue;
    end
    else if SameText(AttrName, 'Max-Age') and TryStrToInt64(AttrValue, MaxAge) then
    begin
      // Max-Age wins over Expires (RFC 6265).
      HasMaxAge := True;
      Expired := MaxAge <= 0;
      if not Expired then
        C.Expires := IncSecond(LocalTimeToUniversal(Now), MaxAge);
    end
    else if SameText(AttrName, 'Expires') and not HasMaxAge and
       TryParseHTTPDate(AttrValue, ExpiresAt) then
    begin
      Expired := ExpiresAt <= LocalTimeToUniversal(Now);
      C.Expires := ExpiresAt;
    end;
  end;

  // A server cookie supersedes a SetCookie cookie of the same name.
  RemoveName(C.Name, True);
  Idx := IndexOf(C.Name, C.Domain, C.Path, False);
  if Expired then
  begin
    if Idx >= 0 then
      Delete(FItems, Idx, 1);
  end
  else if Idx >= 0 then
    FItems[Idx] := C
  else
    Insert(C, FItems, Length(FItems));
end;

function TCookieJar.HeaderFor(const URL: string;
  IncludeGlobal: Boolean): string;
var
  URI: TURI;
  Host, Path: string;
  IsHTTPS, Send: Boolean;
  I: Integer;
begin
  Result := '';
  RemoveExpired;
  URI := ParseURI(URL);
  Host := LowerCase(URI.Host);
  Path := URLPath(URI);
  IsHTTPS := SameText(URI.Protocol, 'https');
  for I := 0 to High(FItems) do
  begin
    if FItems[I].Global then
      Send := IncludeGlobal
    else
    begin
      if FItems[I].HostOnly then
        Send := Host = FItems[I].Domain
      else
        Send := DomainMatches(Host, FItems[I].Domain);
      Send := Send and PathMatches(Path, FItems[I].Path) and
        (IsHTTPS or not FItems[I].Secure);
    end;
    if Send then
    begin
      if Result <> '' then
        Result := Result + '; ';
      Result := Result + FItems[I].Name + '=' + FItems[I].Value;
    end;
  end;
end;

function TCookieJar.Get(const Name: string): string;
var
  I: Integer;
begin
  RemoveExpired;
  for I := 0 to High(FItems) do
    if FItems[I].Name = Name then
      Exit(FItems[I].Value);
  Result := '';
end;

function TCookieJar.Count: Integer;
begin
  RemoveExpired;
  Result := Length(FItems);
end;

procedure ApplySetCookieHeaders(var Cookies: TCookieJar; Headers: TStrings;
  const URL: string);
var
  I: Integer;
begin
  for I := 0 to Headers.Count - 1 do
    if SameText(Trim(Headers.Names[I]), 'Set-Cookie') then
      Cookies.Apply(Trim(Headers.ValueFromIndex[I]), URL);
end;

function THttpSession.DoRequest(const Method, URL: string; Body: TStream;
  const ContentType: string; const Headers: array of TKeyValue;
  const Params: array of TKeyValue; Target: TStream;
  OnProgress: TProgressProc; OnProgressEvent: TProgressEvent): TResponse;
var
  Client: TFPHTTPClient;
  RedirectHandler: TRedirectCookieHandler;
  FullURL: string;
  I: Integer;
begin
  FullURL := AppendQueryParams(GetFullURL(URL), Params);
  Client := GetClient;
  SetupRequest(Client, FullURL);
  if ContentType <> '' then
    Client.AddHeader('Content-Type', ContentType);
  for I := 0 to High(Headers) do
    Client.AddHeader(Headers[I].Key, Headers[I].Value);

  RedirectHandler := TRedirectCookieHandler.Create;
  try
    RedirectHandler.Cookies := @FCookies;
    RedirectHandler.CurrentURL := FullURL;
    Client.OnRedirect := @RedirectHandler.HandleRedirect;
    try
      if Assigned(Target) then
        Result := ExecuteRequest(Client, Method, FullURL, Body, Target,
          OnProgress, OnProgressEvent, FRetryCount, FRetryDelay)
      else
        Result := ExecuteRequest(Client, Method, FullURL, Body, FRetryCount,
          FRetryDelay);
    finally
      Client.OnRedirect := nil;
    end;
    // Cookies from the final response belong to the last URL requested.
    ApplySetCookieHeaders(FCookies, Client.ResponseHeaders,
      RedirectHandler.CurrentURL);
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

function THttpSession.PutJSON(const URL: string; const JSON: string): TResponse;
begin
  Result := Put(URL, JSON, 'application/json');
end;

function THttpSession.PutJSON(const URL: string;
  const JSON: TJSONData): TResponse;
begin
  if JSON = nil then
    raise ERequestError.Create('JSON data cannot be nil');
  Result := PutJSON(URL, JSON.AsJSON);
end;

function THttpSession.PatchJSON(const URL: string;
  const JSON: string): TResponse;
begin
  Result := Patch(URL, JSON, 'application/json');
end;

function THttpSession.PatchJSON(const URL: string;
  const JSON: TJSONData): TResponse;
begin
  if JSON = nil then
    raise ERequestError.Create('JSON data cannot be nil');
  Result := PatchJSON(URL, JSON.AsJSON);
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

function THttpSession.TryPutJSON(const URL: string;
  const JSON: string): TRequestResult;
begin
  try
    Result.Response := PutJSON(URL, JSON);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function THttpSession.TryPatchJSON(const URL: string;
  const JSON: string): TRequestResult;
begin
  try
    Result.Response := PatchJSON(URL, JSON);
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
  FCookies.SetGlobal(Name, Value);
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

procedure THttpSession.SetConnectTimeout(Timeout: Integer);
begin
  if Timeout < 0 then
    Timeout := 0;
  FConnectTimeout := Timeout;
end;

procedure THttpSession.SetProxy(const Host: string; Port: Integer;
  const UserName: string; const Password: string);
begin
  FProxyHost := Host;
  FProxyPort := Port;
  FProxyUser := UserName;
  FProxyPassword := Password;
end;

procedure THttpSession.SetBasicAuth(const UserName, Password: string);
begin
  SetHeader('Authorization', KVBasic(UserName, Password).Value);
end;

procedure THttpSession.SetBearerToken(const Token: string);
begin
  SetHeader('Authorization', KVBearer(Token).Value);
end;

procedure THttpSession.SetRetry(MaxRetries, BaseDelayMS: Integer);
begin
  if MaxRetries < 0 then
    MaxRetries := 0;
  if BaseDelayMS < 0 then
    BaseDelayMS := 0;
  FRetryCount := MaxRetries;
  FRetryDelay := BaseDelayMS;
end;

// Cookie Management

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
  Self.FConnectTimeout := 3000;
  Self.FProxyHost := '';
  Self.FProxyPort := 0;
  Self.FProxyUser := '';
  Self.FProxyPassword := '';

  // Set default headers
  Self.FHeaders.SetItem('Accept', 'application/json');
end;

end.
